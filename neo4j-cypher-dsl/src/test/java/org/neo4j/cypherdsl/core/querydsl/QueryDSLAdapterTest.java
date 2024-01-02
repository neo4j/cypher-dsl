/*
 * Copyright (c) 2019-2024 "Neo4j,"
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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.junit.jupiter.params.provider.Arguments.arguments;
//CHECKSTYLE:ON

import java.time.LocalDate;
import java.time.OffsetTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.Condition;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Literal.UnsupportedLiteralException;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

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

	static Map<String, Object> expected(Object... args) {
		Map<String, Object> result = new HashMap<>();
		for (int i = 0; i < args.length; i++) {
			result.put(String.format("pcdsl%02d", i + 1), args[i]);
		}
		return result;
	}


	static Stream<Arguments> supportedOpsArgs() {

		Stream.Builder<Arguments> r = Stream.builder();

		//@formatter:off
		r.add(arguments("true = true AND false = true", Expressions.TRUE.isTrue().and(Expressions.FALSE.isTrue()), Collections.emptyMap()));
		r.add(arguments("NOT true", Expressions.TRUE.not(), Collections.emptyMap()));
		r.add(arguments("true = true OR false = true", Expressions.TRUE.isTrue().or(Expressions.FALSE.isTrue()), Collections.emptyMap()));
		r.add(arguments("NOT (true XOR false)", booleanOperation(Ops.XNOR,  Expressions.TRUE,  Expressions.FALSE), Collections.emptyMap()));
		r.add(arguments("true XOR false", booleanOperation(Ops.XOR,  Expressions.TRUE,  Expressions.FALSE), Collections.emptyMap()));

		r.add(arguments("size($pcdsl01) = 0", booleanOperation(Ops.COL_IS_EMPTY, constant(Arrays.asList("x"))), expected(Collections.singletonList("x"))));
		r.add(arguments("size($pcdsl01) > $pcdsl02", numberOperation(Integer.class, Ops.COL_SIZE, constant(Arrays.asList("x"))).gt(1), expected(Collections.singletonList("x"), 1)));
		r.add(arguments("size($pcdsl01) > $pcdsl02", numberOperation(Integer.class, Ops.COL_SIZE, constant(new String[]{"x"})).gt(1), expected(new String[]{"x"}, 1)));

		Map<String, String> aMap = new HashMap<>();
		aMap.put("1", "a");
		aMap.put("2", "b");
		r.add(arguments("size(keys($pcdsl01)) > $pcdsl02", numberOperation(Integer.class, Ops.MAP_SIZE, constant(aMap)).gt(1), expected(aMap, 1)));
		r.add(arguments("size(keys($pcdsl01)) = 0", booleanOperation(Ops.MAP_IS_EMPTY, constant(aMap)), expected(aMap)));
		r.add(arguments("any(v in keys($pcdsl01) where v = $pcdsl02)", booleanOperation(Ops.CONTAINS_KEY, constant(aMap), asString("1")), expected(aMap, "1")));
		r.add(arguments("any(v in [k IN KEYS($pcdsl01) | $pcdsl01[k]] where v = $pcdsl02)", booleanOperation(Ops.CONTAINS_VALUE, constant(aMap), asString("b")), expected(aMap, "b")));

		r.add(arguments("size($pcdsl01 + $pcdsl02) = $pcdsl03", stringOperation(Ops.CONCAT, asString("a"), asString("b")).length().eq(2), expected("a", "b", 2)));
		r.add(arguments("toLower($pcdsl01) = $pcdsl02", stringOperation(Ops.LOWER, asString("A")).eq(asString("a")), expected("A", "a")));
		r.add(arguments("substring($pcdsl01, 1) = $pcdsl02", stringOperation(Ops.SUBSTR_1ARG, asString("1234"), Expressions.ONE).eq(asString("234")), expected("1234", "234")));
		r.add(arguments("substring($pcdsl01, 1, 2) = $pcdsl02", stringOperation(Ops.SUBSTR_2ARGS, asString("1234"), Expressions.ONE, Expressions.TWO).eq(asString("23")), expected("1234", "23")));
		r.add(arguments("trim($pcdsl01) = $pcdsl02", stringOperation(Ops.TRIM, asString(" A ")).eq(asString("A")), expected(" A ", "A")));
		r.add(arguments("toUpper($pcdsl01) = $pcdsl02", stringOperation(Ops.UPPER, asString("a")).eq(asString("A")), expected("a", "A")));
		r.add(arguments("$pcdsl01 =~ $pcdsl02", booleanOperation(Ops.MATCHES, asString("a"), asString("A")), expected("a", "A")));
		r.add(arguments("$pcdsl01 =~ ('(?i)' + $pcdsl02)", booleanOperation(Ops.MATCHES_IC, asString("a"), asString("A")), expected("a", "A")));
		r.add(arguments("$pcdsl01 =~ ('(?i)' + $pcdsl02)", booleanOperation(Ops.MATCHES_IC, asString("a"), asString("A")), expected("a", "A")));
		r.add(arguments("$pcdsl01 STARTS WITH $pcdsl02", booleanOperation(Ops.STARTS_WITH, asString("ABC"), asString("a")), expected("ABC", "a")));
		r.add(arguments("toLower($pcdsl01) STARTS WITH toLower($pcdsl02)", booleanOperation(Ops.STARTS_WITH_IC, asString("ABC"), asString("a")), expected("ABC", "a")));
		r.add(arguments("$pcdsl01 ENDS WITH $pcdsl02", booleanOperation(Ops.ENDS_WITH, asString("ABC"), asString("c")), expected("ABC", "c")));
		r.add(arguments("toLower($pcdsl01) ENDS WITH toLower($pcdsl02)", booleanOperation(Ops.ENDS_WITH_IC, asString("ABC"), asString("c")), expected("ABC", "c")));
		r.add(arguments("$pcdsl01 CONTAINS $pcdsl02", booleanOperation(Ops.STRING_CONTAINS, asString("ABC"), asString("c")), expected("ABC", "c")));
		r.add(arguments("toLower($pcdsl01) CONTAINS toLower($pcdsl02)", booleanOperation(Ops.STRING_CONTAINS_IC, asString("ABC"), asString("c")), expected("ABC", "c")));
		r.add(arguments("substring($pcdsl01, 2, 1) = $pcdsl02", Expressions.operation(Character.class, Ops.CHAR_AT, asString("1234"), Expressions.TWO).eq('3'), expected("1234", '3')));
		r.add(arguments("size($pcdsl01) = $pcdsl02", asString("ABC").length().eq(3), expected("ABC", 3)));
		r.add(arguments("$pcdsl01 =~ '.*' + $pcdsl02 + '.*'", booleanOperation(Ops.LIKE, asString("ABC"), asString("a")), expected("ABC", "a")));
		r.add(arguments("$pcdsl01 =~ '(?i).*' + $pcdsl02 + '.*'", booleanOperation(Ops.LIKE_IC, asString("ABC"), asString("a")), expected("ABC", "a")));
		r.add(arguments("size(left($pcdsl01, $pcdsl02)) = $pcdsl02", stringOperation(Ops.StringOps.LEFT, asString("ABCD"), asNumber(3)).length().eq(3), expected("ABCD", 3)));
		r.add(arguments("size(right($pcdsl01, $pcdsl02)) = $pcdsl02", stringOperation(Ops.StringOps.RIGHT, asString("ABCD"), asNumber(3)).length().eq(3), expected("ABCD", 3)));
		r.add(arguments("size(ltrim($pcdsl01)) = $pcdsl02", stringOperation(Ops.StringOps.LTRIM, asString(" ABCD")).length().eq(4), expected(" ABCD", 4)));
		r.add(arguments("size(rtrim($pcdsl01)) = $pcdsl02", stringOperation(Ops.StringOps.RTRIM, asString("ABCD ")).length().eq(4), expected("ABCD ", 4)));

		ZonedDateTime t = ZonedDateTime.of(2021, 3, 10, 15, 50, 0, 0, ZoneId.of("Europe/Berlin"));
		r.add(arguments("datetime() >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE).goe(t), expected(t)));
		LocalDate l = LocalDate.of(2021, 3, 10);
		r.add(arguments("date() >= $pcdsl01", dateOperation(LocalDate.class, Ops.DateTimeOps.CURRENT_DATE).goe(l), expected(l)));
		OffsetTime ot = OffsetTime.of(15, 53, 0, 0, ZoneOffset.UTC);
		r.add(arguments("time() >= $pcdsl01", dateOperation(OffsetTime.class, Ops.DateTimeOps.CURRENT_TIME).goe(ot), expected(ot)));
		r.add(arguments("datetime().epochmillis >= $pcdsl01", dateOperation(Long.class, Ops.DateTimeOps.CURRENT_TIMESTAMP).goe(23L), expected(23L)));
		r.add(arguments("date($pcdsl01) >= $pcdsl02", dateOperation(LocalDate.class, Ops.DateTimeOps.DATE, Expressions.asString("2021-03-10")).goe(l), expected("2021-03-10", l)));

		r.add(arguments("datetime().millisecond >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.MILLISECOND, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().second >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.SECOND, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().minute >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.MINUTE, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().hour >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.HOUR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().week >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().month >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.MONTH, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().year >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.YEAR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().weekYear >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.YEAR_WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));
		r.add(arguments("datetime().dayOfWeek >= $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DAY_OF_WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(23), expected(23)));

		t = ZonedDateTime.of(2021, 3, 10, 16, 48, 0, 0, ZoneId.of("Europe/Berlin"));
		r.add(arguments("datetime() + duration({years: $pcdsl01}) >= $pcdsl02", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_YEARS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(t), expected(23, t)));
		r.add(arguments("datetime() + duration({months: $pcdsl01}) >= $pcdsl02", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_MONTHS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(t), expected(23, t)));
		r.add(arguments("datetime() + duration({weeks: $pcdsl01}) >= $pcdsl02", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_WEEKS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(t), expected(23, t)));
		r.add(arguments("datetime() + duration({days: $pcdsl01}) >= $pcdsl02", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_DAYS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(t), expected(23, t)));
		r.add(arguments("datetime() + duration({hours: $pcdsl01}) >= $pcdsl02", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_HOURS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(t), expected(23, t)));
		r.add(arguments("datetime() + duration({minutes: $pcdsl01}) >= $pcdsl02", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_MINUTES, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(t), expected(23, t)));
		r.add(arguments("datetime() + duration({seconds: $pcdsl01}) >= $pcdsl02", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.ADD_SECONDS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), asNumber(23)).goe(t), expected(23, t)));

		r.add(arguments("duration.between(datetime(), datetime()).years = $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_YEARS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0), expected(0)));
		r.add(arguments("duration.between(datetime(), datetime()).months = $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_MONTHS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0), expected(0)));
		r.add(arguments("duration.between(datetime(), datetime()).weeks = $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_WEEKS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0), expected(0)));
		r.add(arguments("duration.between(datetime(), datetime()).days = $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_DAYS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0), expected(0)));
		r.add(arguments("duration.between(datetime(), datetime()).hours = $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_HOURS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0), expected(0)));
		r.add(arguments("duration.between(datetime(), datetime()).minutes = $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_MINUTES, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0), expected(0)));
		r.add(arguments("duration.between(datetime(), datetime()).seconds = $pcdsl01", dateOperation(Integer.class, Ops.DateTimeOps.DIFF_SECONDS, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE), dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).eq(0), expected(0)));

		t = ZonedDateTime.of(2021, 3, 10, 17, 02, 0, 0, ZoneId.of("Europe/Berlin"));
		r.add(arguments("date.truncate('year', datetime()) >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_YEAR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(t), expected(t)));
		r.add(arguments("date.truncate('month', datetime()) >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_MONTH, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(t), expected(t)));
		r.add(arguments("date.truncate('week', datetime()) >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_WEEK, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(t), expected(t)));
		r.add(arguments("date.truncate('day', datetime()) >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_DAY, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(t), expected(t)));
		r.add(arguments("datetime.truncate('hour', datetime()) >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_HOUR, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(t), expected(t)));
		r.add(arguments("datetime.truncate('minute', datetime()) >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_MINUTE, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(t), expected(t)));
		r.add(arguments("datetime.truncate('second', datetime()) >= $pcdsl01", dateOperation(ZonedDateTime.class, Ops.DateTimeOps.TRUNC_SECOND, dateOperation(ZonedDateTime.class, Ops.DateTimeOps.SYSDATE)).goe(t), expected(t)));

		r.add(arguments("abs($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.ABS, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("acos($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.ACOS, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("asin($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.ASIN, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("atan($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.ATAN, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("ceil($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.CEIL, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("cos($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.COS, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("cot($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.COT, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("degrees($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.DEG, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("tan($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.TAN, asNumber(1)).gt(1), expected(1,  1.0)));
		r.add(arguments("sqrt($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.SQRT, asNumber(4)).gt(1), expected(4, 1.0)));
		r.add(arguments("sign($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.SIGN, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("sin($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.SIN, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("round($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.ROUND, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("round($pcdsl01, $pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.ROUND2, asNumber(1), asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("radians($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.RAD, asNumber(1)).gt(1), expected(1, 1.0)));
		r.add(arguments("CASE WHEN $pcdsl01 < $pcdsl02 THEN $pcdsl01 ELSE $pcdsl02 END > $pcdsl03", numberOperation(Double.class, Ops.MathOps.MIN, asNumber(1), asNumber(2)).gt(1), expected(1, 2, 1.0)));
		r.add(arguments("CASE WHEN $pcdsl01 > $pcdsl02 THEN $pcdsl01 ELSE $pcdsl02 END > $pcdsl03", numberOperation(Double.class, Ops.MathOps.MAX, asNumber(1), asNumber(2)).gt(1), expected(1, 2, 1.0)));
		r.add(arguments("floor($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.FLOOR, asNumber(1.1)).gt(1), expected(1.1, 1.0)));
		r.add(arguments("exp($pcdsl01) > $pcdsl02", numberOperation(Double.class, Ops.MathOps.EXP, asNumber(1)).gt(1), expected(1, 1.0)));

		Predicate p = Expressions.cases().when(Expressions.TRUE).then(Expressions.asNumber(1))
			.when(Expressions.FALSE).then(Expressions.asNumber(2))
			.otherwise(Expressions.asNumber(3)).gt(3);
		r.add(arguments("(CASE WHEN true THEN $pcdsl01 WHEN false THEN $pcdsl02 ELSE $pcdsl03 END) > $pcdsl03", p, expected(1, 2, 3)));
		//@formatter:on

		return r.build();
	}

	@MethodSource("supportedOpsArgs")
	@ParameterizedTest(name = "{index} {0}")
	void supportedOpsWithParameters(String expectedFragment, Predicate predicate, Map<String, Object> expectedParameters) {

		Statement statement = Cypher.with(Cypher.literalOf(1).as("e"))
			.where(Cypher.adapt(predicate).asCondition())
			.returning("e")
			.build();

		statement.setRenderConstantsAsParameters(true);
		assertThat(statement.getCatalog().getParameters()).containsExactlyEntriesOf(expectedParameters);
		assertThat(statement.getCypher())
			.isEqualTo("WITH 1 AS e WHERE " + expectedFragment + " RETURN e");
	}

	@MethodSource("supportedOpsArgs")
	@ParameterizedTest(name = "{index} {0}")
	void supportedOpsWithLiterals(String expectedFragment, Predicate predicate, Map<String, Object> expectedParameters) {

		Statement statement = Cypher.with(Cypher.literalOf(1).as("e"))
			.where(Cypher.adapt(predicate).asCondition())
			.returning("e")
			.build();

		String expectedString = "WITH 1 AS e WHERE " + expectedFragment + " RETURN e";
		Map<String, Object> finalExpectedParameters = new HashMap<>();
		for (Map.Entry<String, Object> entry : expectedParameters.entrySet()) {
			String k = entry.getKey();
			Object v = entry.getValue();
			try {
				String replacement = Cypher.literalOf(v).asString();
				expectedString = expectedString.replaceAll(java.util.regex.Pattern.quote("$" + k), replacement);
			} catch (UnsupportedLiteralException e) {
				finalExpectedParameters.put(k, v);
			}
		}
		assertThat(statement.getCatalog().getParameters()).containsExactlyEntriesOf(finalExpectedParameters);
		assertThat(statement.getCypher()).isEqualTo(expectedString);
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
	void queryingByPathBasedOnClassShouldWork() {

		Path<Person> person = Expressions.path(Person.class, "n");
		Path<String> personFirstName = Expressions.path(String.class, person, "firstName");
		Path<Integer> personAge = Expressions.path(Integer.class, person, "age");
		BooleanBuilder expr = new BooleanBuilder(predicate(Ops.EQ, personFirstName,
			constant("P"))).and(predicate(Ops.GT, personAge, constant(25)));

		Statement statement = Cypher.match(Cypher.adapt(person).asNode())
			.where(Cypher.adapt(expr).asCondition()).returning(Cypher.adapt(person).asName()).build();
		assertThat(statement.getCatalog().getParameters()).isEmpty();
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
		assertThat(statement.getCatalog().getParameters()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName = 'P' AND n.age > 25 RETURN n");
	}

	@Test
	void queryingByQClassShouldWork() {

		QPerson person = QPerson.person;
		Predicate p = person.firstName.eq("P").and(person.age.gt(25));

		Statement statement = Cypher.match(Cypher.adapt(person).asNode())
			.where(Cypher.adapt(p).asCondition()).returning(Cypher.adapt(person).asName()).build();
		assertThat(statement.getCatalog().getParameters()).isEmpty();
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

		assertThat(statement.getCatalog().getParameters()).isEmpty();
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

		assertThat(statement.getCatalog().getParameters()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName = 'P' AND n.age > 25 RETURN n");
		// end::query-dsl-simple[]
	}

	@Test
	void qClassToNodeShouldWorkNoConstants() {

		// tag::query-dsl-simple-avoid-constants[]
		QPerson n = new QPerson("n");
		Statement statement = Cypher.match(Cypher.adapt(n).asNode())
			.where(Cypher.adapt(n.firstName.eq("P").and(n.age.gt(25))).asCondition())
			.returning(Cypher.adapt(n).asName())
			.build();

		statement.setRenderConstantsAsParameters(true); // <.>
		assertThat(statement.getCatalog().getParameters()).containsEntry("pcdsl01", "P"); // <.>
		assertThat(statement.getCatalog().getParameters()).containsEntry("pcdsl02", 25);
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n:`Person`) WHERE n.firstName = $pcdsl01 AND n.age > $pcdsl02 RETURN n"); // <.>
		// end::query-dsl-simple-avoid-constants[]
	}

	@Test
	void changingModeShouldWork() {

		QPerson n = new QPerson("n");
		Statement statement = Cypher.match(Cypher.adapt(n).asNode())
			.where(Cypher.adapt(n.firstName.eq("P").and(n.age.gt(25))).asCondition())
			.returning(Cypher.adapt(n).asName())
			.build();

		statement.setRenderConstantsAsParameters(true); // <.>
		assertThat(statement.getCatalog().getParameters()).containsEntry("pcdsl01", "P"); // <.>
		assertThat(statement.getCatalog().getParameters()).containsEntry("pcdsl02", 25);
		assertThat(statement.getCypher()).isEqualTo("MATCH (n:`Person`) WHERE n.firstName = $pcdsl01 AND n.age > $pcdsl02 RETURN n"); // <.>

		statement.setRenderConstantsAsParameters(false);
		assertThat(statement.getCatalog().getParameters()).isEmpty();
		assertThat(statement.getCypher()).isEqualTo("MATCH (n:`Person`) WHERE n.firstName = 'P' AND n.age > 25 RETURN n");
	}

	@Test
	void prettyPrinterShouldAlwaysUseConstants() {

		QPerson n = new QPerson("n");
		Statement statement = Cypher.match(Cypher.adapt(n).asNode())
			.where(Cypher.adapt(n.firstName.eq("P").and(n.age.gt(25))).asCondition())
			.returning(Cypher.adapt(n).asName())
			.build();

		statement.setRenderConstantsAsParameters(true);
		assertThat(statement.getCatalog().getParameters()).containsEntry("pcdsl01", "P");
		assertThat(statement.getCatalog().getParameters()).containsEntry("pcdsl02", 25);
		assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(statement))
			.isEqualTo("""
				MATCH (n:Person)
				WHERE n.firstName = 'P' AND n.age > 25
				RETURN n"""
			);
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

		assertThat(statement.getCatalog().getParameterNames()).hasSize(2); // <.>
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

		assertThat(statement.getCatalog().getParameterNames()).isEmpty();
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

		assertThat(statement.getCatalog().getParameterNames()).isEmpty();
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

	@Test
	void emptyBooleanBuilder() {

		Condition condition = Cypher.adapt(new BooleanBuilder()).asCondition();
		Statement statement = Cypher.match(Cypher.anyNode().named("n"))
			.where(condition)
			.returning(Cypher.name("n"))
			.build();

		assertThat(statement.getCatalog().getParameterNames()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n) RETURN n");
	}

	@Test
	void booleanBuilder() {

		Condition condition = Cypher.adapt(new BooleanBuilder().and(Expressions.FALSE).and(Expressions.TRUE)).asCondition();
		Statement statement = Cypher.match(Cypher.anyNode().named("n"))
			.where(condition)
			.returning(Cypher.name("n"))
			.build();

		assertThat(statement.getCatalog().getParameterNames()).isEmpty();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (n) WHERE false AND true RETURN n");
	}

	@Test
	void communityExample() {

		QPerson user = QPerson.person;

		Predicate predicate = user.firstName.equalsIgnoreCase("dave")
			.and(user.lastName.startsWithIgnoreCase("mathews"));

		Node p = Cypher.node("Entity").named("p");
		String cypher = Cypher
			.match(p)
			.where(Cypher.adapt(predicate).asCondition())
			.returning(p)
			.build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (p:`Entity`) WHERE toLower(person.firstName) = 'dave' AND toLower(person.lastName) STARTS WITH 'mathews' RETURN p");
	}

	@Test
	void communityExampleWithWorkaround() {

		QPerson user = QPerson.person;

		Predicate predicate = user.firstName.toLowerCase().eq("dave".toLowerCase())
			.and(user.lastName.startsWithIgnoreCase("mathews"));

		Node p = Cypher.node("Entity").named("p");
		String cypher = Cypher
			.match(p)
			.where(Cypher.adapt(predicate).asCondition())
			.returning(p)
			.build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (p:`Entity`) WHERE toLower(person.firstName) = 'dave' AND toLower(person.lastName) STARTS WITH 'mathews' RETURN p");
	}
}
