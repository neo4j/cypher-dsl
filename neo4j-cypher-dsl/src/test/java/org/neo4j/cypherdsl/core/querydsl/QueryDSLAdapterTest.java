/*
 * Copyright (c) 2019-2021 "Neo4j,"
 * Neo4j Sweden AB [https://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.neo4j.cypherdsl.core.querydsl;

//CHECKSTYLE:OFF
import static com.querydsl.core.alias.Alias.$;
import static com.querydsl.core.alias.Alias.alias;
import static com.querydsl.core.types.dsl.Expressions.asNumber;
import static com.querydsl.core.types.dsl.Expressions.asString;
import static com.querydsl.core.types.dsl.Expressions.booleanOperation;
import static com.querydsl.core.types.dsl.Expressions.constant;
import static com.querydsl.core.types.dsl.Expressions.dateOperation;
import static com.querydsl.core.types.dsl.Expressions.numberOperation;
import static com.querydsl.core.types.dsl.Expressions.predicate;
import static com.querydsl.core.types.dsl.Expressions.stringOperation;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.junit.jupiter.params.provider.Arguments.arguments;
//CHECKSTYLE:ON

import java.time.LocalDate;
import java.time.OffsetTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.support.UnsupportedLiteralException;

import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.Ops;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.Predicate;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.dsl.Param;

/**
 * This is an integration test looking at two things:
 * <ol>
 *     <li>The templates we defined for QueryDSL: They must resolve to semantically correct Cypher.</li>
 *     <li>Whether the {@link ToCypherFormatStringVisitor} actually creates syntactically fragments.</li>
 * </ol>
 * As we deal with a lot of operators from QueryDSL, it's more economical to test these two things together.
 * An approach testing the templates in isolation would require rendering them ("converting" to speak in terms of QueryDSL)
 * as well, pretty much repeating what we are doing in the {@link ToCypherFormatStringVisitor} again.
 *
 * @author Michael J. Simons
 * @soundtrack Helge Schneider - Heart Attack No. 1
 */
class QueryDSLAdapterTest {

	static Stream<Arguments> supportedOpsArgs() {

		Stream.Builder<Arguments> r = Stream.builder();

		//@formatter:off
		r.add(arguments("true = true AND false = true", Expressions.TRUE.isTrue().and(Expressions.FALSE.isTrue())));
		r.add(arguments("NOT true", Expressions.TRUE.not()));
		r.add(arguments("true = true OR false = true", Expressions.TRUE.isTrue().or(Expressions.FALSE.isTrue())));
		r.add(arguments("NOT (true XOR false)", booleanOperation(Ops.XNOR,  Expressions.TRUE,  Expressions.FALSE)));
		r.add(arguments("true XOR false", booleanOperation(Ops.XOR,  Expressions.TRUE,  Expressions.FALSE)));

		r.add(arguments("size(['x']) = 0", booleanOperation(Ops.COL_IS_EMPTY, constant(Arrays.asList("x")))));
		r.add(arguments("size(['x']) > 1", numberOperation(Integer.class, Ops.COL_SIZE, constant(Arrays.asList("x"))).gt(1)));

		r.add(arguments("size(['x']) > 1", numberOperation(Integer.class, Ops.COL_SIZE, constant(new String[]{"x"})).gt(1)));

		Map<String, String> aMap = new HashMap<>();
		aMap.put("1", "a");
		aMap.put("2", "b");
		r.add(arguments("size(keys({`1`: 'a', `2`: 'b'})) > 1", numberOperation(Integer.class, Ops.MAP_SIZE, constant(aMap)).gt(1)));
		r.add(arguments("size(keys({`1`: 'a', `2`: 'b'})) = 0", booleanOperation(Ops.MAP_IS_EMPTY, constant(aMap))));
		r.add(arguments("any(v in keys({`1`: 'a', `2`: 'b'}) where v = '1')", booleanOperation(Ops.CONTAINS_KEY, constant(aMap), asString("1"))));
		r.add(arguments("any(v in [k IN KEYS({`1`: 'a', `2`: 'b'}) | {`1`: 'a', `2`: 'b'}[k]] where v = 'b')", booleanOperation(Ops.CONTAINS_VALUE, constant(aMap), asString("b"))));

		r.add(arguments("size('a' + 'b') = 2", stringOperation(Ops.CONCAT, asString("a"), asString("b")).length().eq(2)));
		r.add(arguments("toLower('A') = 'a'", stringOperation(Ops.LOWER, asString("A")).eq(asString("a"))));
		r.add(arguments("substring('1234', 1) = '234'", stringOperation(Ops.SUBSTR_1ARG, asString("1234"), Expressions.ONE).eq(asString("234"))));
		r.add(arguments("substring('1234', 1, 2) = '23'", stringOperation(Ops.SUBSTR_2ARGS, asString("1234"), Expressions.ONE, Expressions.TWO).eq(asString("23"))));
		r.add(arguments("trim(' A ') = 'A'", stringOperation(Ops.TRIM, asString(" A ")).eq(asString("A"))));
		r.add(arguments("toUpper('a') = 'A'", stringOperation(Ops.UPPER, asString("a")).eq(asString("A"))));
		r.add(arguments("'a' =~ 'A'", booleanOperation(Ops.MATCHES, asString("a"), asString("A"))));
		r.add(arguments("'a' =~ ('(?i)' + 'A')", booleanOperation(Ops.MATCHES_IC, asString("a"), asString("A"))));
		r.add(arguments("'a' =~ ('(?i)' + 'A')", booleanOperation(Ops.MATCHES_IC, asString("a"), asString("A"))));
		r.add(arguments("'ABC' STARTS WITH 'a'", booleanOperation(Ops.STARTS_WITH, asString("ABC"), asString("a"))));
		r.add(arguments("toLower('ABC') STARTS WITH toLower('a')", booleanOperation(Ops.STARTS_WITH_IC, asString("ABC"), asString("a"))));
		r.add(arguments("'ABC' ENDS WITH 'c'", booleanOperation(Ops.ENDS_WITH, asString("ABC"), asString("c"))));
		r.add(arguments("toLower('ABC') ENDS WITH toLower('c')", booleanOperation(Ops.ENDS_WITH_IC, asString("ABC"), asString("c"))));
		r.add(arguments("'ABC' CONTAINS 'c'", booleanOperation(Ops.STRING_CONTAINS, asString("ABC"), asString("c"))));
		r.add(arguments("toLower('ABC') CONTAINS toLower('c')", booleanOperation(Ops.STRING_CONTAINS_IC, asString("ABC"), asString("c"))));
		r.add(arguments("substring('1234', 2, 1) = '3'", Expressions.operation(Character.class, Ops.CHAR_AT, asString("1234"),  Expressions.TWO).eq('3')));
		r.add(arguments("size('ABC') = 3", asString("ABC").length().eq(3)));
		r.add(arguments("'ABC' =~ '.*' + 'a' + '.*'", booleanOperation(Ops.LIKE, asString("ABC"), asString("a"))));
		r.add(arguments("'ABC' =~ '(?i).*' + 'a' + '.*'", booleanOperation(Ops.LIKE_IC, asString("ABC"), asString("a"))));
		r.add(arguments("size(left('ABCD', 3)) = 3", stringOperation(Ops.StringOps.LEFT, asString("ABCD"), asNumber(3)).length().eq(3)));
		r.add(arguments("size(right('ABCD', 3)) = 3", stringOperation(Ops.StringOps.RIGHT, asString("ABCD"), asNumber(3)).length().eq(3)));
		r.add(arguments("size(ltrim(' ABCD')) = 4", stringOperation(Ops.StringOps.LTRIM, asString(" ABCD"), asNumber(3)).length().eq(4)));
		r.add(arguments("size(rtrim('ABCD ')) = 4", stringOperation(Ops.StringOps.RTRIM, asString("ABCD "), asNumber(3)).length().eq(4)));

		r.add(arguments("datetime() >= datetime('2021-03-10T15:50:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE).goe(ZonedDateTime.of(2021, 3, 10, 15, 50, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("date() >= date('2021-03-10')", dateOperation(LocalDate.class, Ops.DateTimeOps.CURRENT_DATE).goe(LocalDate.of(2021, 3, 10))));
		r.add(arguments("time() >= time('15:53:00Z')", dateOperation(OffsetTime.class, Ops.DateTimeOps.CURRENT_TIME).goe(OffsetTime.of(15, 53, 0, 0, ZoneOffset.UTC))));
		r.add(arguments("datetime().epochmillis >= 23", dateOperation(Long.class, Ops.DateTimeOps.CURRENT_TIMESTAMP).goe(23L)));
		r.add(arguments("date('2021-03-10') >= date('2021-03-10')", dateOperation(LocalDate.class, Ops.DateTimeOps.DATE, Expressions.asString("2021-03-10")).goe(LocalDate.of(2021, 3, 10))));

		r.add(arguments("datetime().millisecond >= 23", dateOperation(Integer.class, Ops.DateTimeOps.MILLISECOND, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().second >= 23", dateOperation(Integer.class, Ops.DateTimeOps.SECOND, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().minute >= 23", dateOperation(Integer.class, Ops.DateTimeOps.MINUTE, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().hour >= 23", dateOperation(Integer.class, Ops.DateTimeOps.HOUR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().week >= 23", dateOperation(Integer.class, Ops.DateTimeOps.WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().month >= 23", dateOperation(Integer.class, Ops.DateTimeOps.MONTH, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().year >= 23", dateOperation(Integer.class, Ops.DateTimeOps.YEAR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().weekYear >= 23", dateOperation(Integer.class, Ops.DateTimeOps.YEAR_WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));
		r.add(arguments("datetime().dayOfWeek >= 23", dateOperation(Integer.class, Ops.DateTimeOps.DAY_OF_WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23)));

		r.add(arguments("datetime() + duration({years: 23}) >= datetime('2021-03-10T16:48:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_YEARS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime() + duration({months: 23}) >= datetime('2021-03-10T16:48:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_MONTHS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime() + duration({weeks: 23}) >= datetime('2021-03-10T16:48:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_WEEKS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime() + duration({days: 23}) >= datetime('2021-03-10T16:48:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_DAYS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime() + duration({hours: 23}) >= datetime('2021-03-10T16:48:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_HOURS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime() + duration({minutes: 23}) >= datetime('2021-03-10T16:48:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_MINUTES, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime() + duration({seconds: 23}) >= datetime('2021-03-10T16:48:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_SECONDS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin")))));

		r.add(arguments("duration.between(datetime(), datetime()).years = 0", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_YEARS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0)));
		r.add(arguments("duration.between(datetime(), datetime()).months = 0", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_MONTHS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0)));
		r.add(arguments("duration.between(datetime(), datetime()).weeks = 0", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_WEEKS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0)));
		r.add(arguments("duration.between(datetime(), datetime()).days = 0", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_DAYS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0)));
		r.add(arguments("duration.between(datetime(), datetime()).hours = 0", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_HOURS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0)));
		r.add(arguments("duration.between(datetime(), datetime()).minutes = 0", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_MINUTES, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0)));
		r.add(arguments("duration.between(datetime(), datetime()).seconds = 0", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_SECONDS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0)));

		r.add(arguments("date.truncate('year', datetime()) >= datetime('2021-03-10T17:02:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_YEAR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("date.truncate('month', datetime()) >= datetime('2021-03-10T17:02:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_MONTH, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("date.truncate('week', datetime()) >= datetime('2021-03-10T17:02:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("date.truncate('day', datetime()) >= datetime('2021-03-10T17:02:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_DAY, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime.truncate('hour', datetime()) >= datetime('2021-03-10T17:02:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_HOUR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime.truncate('minute', datetime()) >= datetime('2021-03-10T17:02:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_MINUTE, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin")))));
		r.add(arguments("datetime.truncate('second', datetime()) >= datetime('2021-03-10T17:02:00+01:00[Europe/Berlin]')", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_SECOND, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin")))));

		r.add(arguments("abs(1) > 1.0", numberOperation(Double.class, Ops.MathOps.ABS, asNumber(1)).gt(1)));
		r.add(arguments("acos(1) > 1.0", numberOperation(Double.class, Ops.MathOps.ACOS, asNumber(1)).gt(1)));
		r.add(arguments("asin(1) > 1.0", numberOperation(Double.class, Ops.MathOps.ASIN, asNumber(1)).gt(1)));
		r.add(arguments("atan(1) > 1.0", numberOperation(Double.class, Ops.MathOps.ATAN, asNumber(1)).gt(1)));
		r.add(arguments("ceil(1) > 1.0", numberOperation(Double.class, Ops.MathOps.CEIL, asNumber(1)).gt(1)));
		r.add(arguments("cos(1) > 1.0", numberOperation(Double.class, Ops.MathOps.COS, asNumber(1)).gt(1)));
		r.add(arguments("cot(1) > 1.0", numberOperation(Double.class, Ops.MathOps.COT, asNumber(1)).gt(1)));
		r.add(arguments("degrees(1) > 1.0", numberOperation(Double.class, Ops.MathOps.DEG, asNumber(1)).gt(1)));
		r.add(arguments("tan(1) > 1.0", numberOperation(Double.class, Ops.MathOps.TAN, asNumber(1)).gt(1)));
		r.add(arguments("sqrt(4) > 1.0", numberOperation(Double.class, Ops.MathOps.SQRT, asNumber(4)).gt(1)));
		r.add(arguments("sign(1) > 1.0", numberOperation(Double.class, Ops.MathOps.SIGN, asNumber(1)).gt(1)));
		r.add(arguments("sin(1) > 1.0", numberOperation(Double.class, Ops.MathOps.SIN, asNumber(1)).gt(1)));
		r.add(arguments("round(1) > 1.0", numberOperation(Double.class, Ops.MathOps.ROUND, asNumber(1)).gt(1)));
		r.add(arguments("round(1, 1) > 1.0", numberOperation(Double.class, Ops.MathOps.ROUND2, asNumber(1), asNumber(1)).gt(1)));
		r.add(arguments("radians(1) > 1.0", numberOperation(Double.class, Ops.MathOps.RAD, asNumber(1)).gt(1)));
		r.add(arguments("CASE WHEN 1 < 2 THEN 1 ELSE 2 END > 1.0", numberOperation(Double.class, Ops.MathOps.MIN, asNumber(1), asNumber(2)).gt(1)));
		r.add(arguments("CASE WHEN 1 > 2 THEN 1 ELSE 2 END > 1.0", numberOperation(Double.class, Ops.MathOps.MAX, asNumber(1), asNumber(2)).gt(1)));
		r.add(arguments("floor(1.1) > 1.0", numberOperation(Double.class, Ops.MathOps.FLOOR, asNumber(1.1)).gt(1)));
		r.add(arguments("exp(1) > 1.0", numberOperation(Double.class, Ops.MathOps.EXP, asNumber(1)).gt(1)));

		Predicate p = Expressions.cases().when(Expressions.TRUE).then(Expressions.asNumber(1))
			.when(Expressions.FALSE).then(Expressions.asNumber(2))
			.otherwise(Expressions.asNumber(3)).gt(3);
		r.add(arguments("(CASE WHEN true THEN 1 WHEN false THEN 2 ELSE 3 END) > 3", p));
		//@formatter:on

		return r.build();
	}

	@MethodSource("supportedOpsArgs")
	@ParameterizedTest(name = "{index} {0}")
	void supportedOps(String expectedFragment, Predicate predicate) {

		Statement statement = Cypher.with(Cypher.literalOf(1).as("e"))
			.where(Cypher.adapt(predicate).asCondition())
			.returning("e")
			.build();

		assertThat(statement.getCypher())
			.isEqualTo("WITH 1 AS e WHERE " + expectedFragment + " RETURN e");
	}

	static Stream<Arguments> unsupportedOpsShouldBeRecognizedBeforeHandArgs() {

		Stream.Builder<Arguments> r = Stream.builder();
		r.add(arguments(numberOperation(Integer.class, Ops.INDEX_OF, asString("ABC"), asString("B")).eq(1)));
		r.add(arguments(
			numberOperation(Integer.class, Ops.INDEX_OF_2ARGS, asString("ABC"), asString("B"), asNumber(3)).eq(1)));

		return r.build();
	}

	@MethodSource("unsupportedOpsShouldBeRecognizedBeforeHandArgs")
	@ParameterizedTest
	void unsupportedOpsShouldBeRecognizedBeforeHand(Predicate predicate) {

		Assertions.assertThatIllegalArgumentException().isThrownBy(() -> Cypher.adapt(predicate).asCondition());
	}

	@Test
	void unsupportedLiterals() {

		assertThatExceptionOfType(UnsupportedLiteralException.class)
			.isThrownBy(() -> Cypher.adapt(Expressions.asDate(new Date())).asExpression())
			.withMessageStartingWith("Unsupported literal type: class java.util.Date");
	}

	@Test
	void queryingByPathBasedOnClassShouldWork() {

		Path<Person> person = Expressions.path(Person.class, "n");
		Path<String> personFirstName = Expressions.path(String.class, person, "firstName");
		Path<Integer> personAge = Expressions.path(Integer.class, person, "age");
		BooleanBuilder expr = new BooleanBuilder(predicate(Ops.EQ, personFirstName,
			constant("P"))).and(predicate(Ops.GT, personAge, constant(25)));

		Statement statement = Cypher.match(Cypher.adapt(person).asNode())
			.where(Cypher.adapt(expr).asCondition()).returning(Cypher.adapt(person).asName()).build();
		assertThat(statement.getParameters()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName = 'P' AND n.age > 25 RETURN n");
	}

	@Test
	void queryingByProxyShouldWork() {

		Person person = alias(Person.class, "n");
		Predicate p = $(person.getFirstName()).eq("P")
			.and($(person.getAge()).gt(25));

		Statement statement = Cypher.match(Cypher.node("Person").named("n"))
			.where(Cypher.adapt(p).asCondition()).returning(Cypher.name(person.toString())).build();
		assertThat(statement.getParameters()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName = 'P' AND n.age > 25 RETURN n");
	}

	@Test
	void queryingByQClassShouldWork() {

		QPerson person = QPerson.person;
		Predicate p = person.firstName.eq("P").and(person.age.gt(25));

		Statement statement = Cypher.match(Cypher.adapt(person).asNode())
			.where(Cypher.adapt(p).asCondition()).returning(Cypher.adapt(person).asName()).build();
		assertThat(statement.getParameters()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (person:`Person`) WHERE person.firstName = 'P' AND person.age > 25 RETURN person");
	}

	@Test
	void pathsShouldBeTurnedIntoExpressions() {

		QPerson person = QPerson.person;
		Statement statement = Cypher.match(Cypher.adapt(person).asNode())
			.where(Cypher.adapt(person.firstName.eq("Rickard")).asCondition())
			.returning(Cypher.adapt(person.firstName).asExpression())
			.orderBy(Cypher.adapt(person.firstName).asExpression().descending())
			.build();

		assertThat(statement.getParameters()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo(
				"MATCH (person:`Person`) WHERE person.firstName = 'Rickard' RETURN person.firstName ORDER BY person.firstName DESC");
	}

	@Test
	void qClassToNodeShouldWork() {

		// tag::query-dsl-simple[]
		QPerson n = new QPerson("n"); // <.>
		Statement statement = Cypher.match(Cypher.adapt(n).asNode()) // <.>
			.where(Cypher.adapt(n.firstName.eq("P").and(n.age.gt(25))).asCondition()) // <.>
			.returning(Cypher.adapt(n).asName()) // <.>
			.build();

		assertThat(statement.getParameters()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName = 'P' AND n.age > 25 RETURN n");
		// end::query-dsl-simple[]
	}

	@Test
	void parametersShouldBeRendered() {

		// tag::query-dsl-parameters[]
		QPerson n = new QPerson("n");
		Statement statement = Cypher.match(Cypher.adapt(n).asNode())
			.where(Cypher.adapt(n.firstName.eq(new Param<>(String.class, "name"))
					.and(n.age.gt(new Param<>(Integer.class, "age"))) // <.>
				).asCondition()
			)
			.returning(Cypher.adapt(n).asName())
			.build();

		assertThat(statement.getParameterNames()).hasSize(2); // <.>
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName = $name AND n.age > $age RETURN n");
		// end::query-dsl-parameters[]
	}

	@Test
	void regexShouldBeSupported() {

		QPerson n = new QPerson("n");
		Statement statement = Cypher.match(Cypher.adapt(n).asNode())
			.where(Cypher.adapt(n.firstName.matches("(?i).*rick.*")).asCondition())
			.returning(Cypher.adapt(n).asName())
			.build();

		assertThat(statement.getParameterNames()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName =~ '(?i).*rick.*' RETURN n");
	}

	@Test
	void existShouldWork() {

		QPerson n = new QPerson("n");
		Statement statement = Cypher.match(Cypher.adapt(n).asNode())
			.where(Cypher.adapt(Expressions.predicate(Ops.EXISTS, n.firstName)).asCondition())
			.returning(Cypher.adapt(n).asName())
			.build();

		assertThat(statement.getParameterNames()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE exists(n.firstName) RETURN n");
	}

	@Test
	void shouldNotAdaptArbitraryThingsAsCondition() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.adapt(Expressions.TWO).asCondition())
			.withMessage("Only Query-DSL predicates can be turned into Cypher-DSL's predicates.");
	}

	@Test
	void shouldNotAdaptArbitraryThingsAsNodes() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.adapt(Expressions.TWO).asNode())
			.withMessage("Only Query-DSL paths can be turned into nodes.");
	}

	@Test
	void shouldNotAdaptArbitraryThingsAsNames() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.adapt(Expressions.TWO).asName())
			.withMessage("Only Query-DSL paths can be turned into names.");
	}
}
