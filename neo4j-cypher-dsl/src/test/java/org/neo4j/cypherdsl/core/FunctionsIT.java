/*
 * Copyright (c) 2019-2023 "Neo4j,"
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
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Locale;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 * @author Gerrit Meier
 */
class FunctionsIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Test
	void propertiesInvocationShouldBeReusable() {

		Node source = Cypher.node("Movie", Cypher.mapOf("title", Cypher.literalOf("The Matrix"))).named("src");
		FunctionInvocation p = Cypher.properties(source);
		Node copy = Cypher.node("MovieCopy").named("copy");

		String query = cypherRenderer.render(
			Cypher.match(source).create(copy).set(copy, p).returning(copy).build()
		);
		assertThat(query).isEqualTo(
			"MATCH (src:`Movie` {title: 'The Matrix'}) CREATE (copy:`MovieCopy`) SET copy = properties(src) RETURN copy");
	}

	@Nested
	class Reduction {

		@Test
		void reductionOfPaths() {

			Relationship r = Cypher.node("Node").named("n").relationshipTo(Cypher.node("Node2").named("m")).named("r");
			NamedPath namedPath = Cypher.path("patternPath").definedBy(r);

			SymbolicName x = Cypher.name("x");
			ListExpression emptyList = Cypher.listOf();
			FunctionInvocation listToReduce = Cypher.relationships(namedPath);
			SymbolicName n = Cypher.name("n");
			Statement statement = Cypher.returning(
				Cypher.reduce(n)
					.in(listToReduce)
					.map(x.add(n))
					.accumulateOn(x)
					.withInitialValueOf(emptyList)
			).build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo("RETURN reduce(x = [], n IN relationships(patternPath) | (x + n))");
		}

		@Test
		void manual() {

			Node a = Cypher.anyNode().named("a");
			Node b = Cypher.anyNode().named("b");
			Node c = Cypher.anyNode().named("c");
			NamedPath p = Cypher.path("p").definedBy(a.relationshipTo(b).relationshipTo(c));
			SymbolicName n = Cypher.name("n");
			SymbolicName totalAge = Cypher.name("totalAge");
			Statement statement = Cypher.match(p)
				.where(a.property("name").isEqualTo(Cypher.literalOf("Alice")))
				.and(b.property("name").isEqualTo(Cypher.literalOf("Bob")))
				.and(c.property("name").isEqualTo(Cypher.literalOf("Daniel")))
				.returning(
					Cypher.reduce(n)
						.in(Cypher.nodes(p))
						.map(totalAge.add(Cypher.property(n, "age")))
						.accumulateOn(totalAge)
						.withInitialValueOf(Cypher.literalOf(0)).as("reduction")
				).build();
			assertThat(cypherRenderer.render(statement))
					.isEqualTo("MATCH p = (a)-->(b)-->(c) "
							+ "WHERE (a.name = 'Alice' AND b.name = 'Bob' AND c.name = 'Daniel') "
							+ "RETURN reduce(totalAge = 0, n IN nodes(p) | (totalAge + n.age)) AS reduction");
		}

		@Test
		void ofListComprehension() {

			SymbolicName n = Cypher.name("n");
			SymbolicName totalAge = Cypher.name("totalAge");
			SymbolicName x = Cypher.name("x");
			Statement statement = Cypher.returning(
				Cypher.reduce(n)
					.in(Cypher.listWith(x)
						.in(Cypher.range(0, 10))
						.where(x.remainder(Cypher.literalOf(2)).isEqualTo(Cypher.literalOf(0)))
						.returning(x.pow(Cypher.literalOf(3)))
					)
					.map(totalAge.add(n))
					.accumulateOn(totalAge)
					.withInitialValueOf(Cypher.literalOf(0.0)).as("result"))
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"RETURN reduce(totalAge = 0.0, n IN [x IN range(0, 10) WHERE (x % 2) = 0 | x^3] | (totalAge + n)) AS result");
		}
	}

	@ParameterizedTest
	@EnumSource(BuiltInFunctions.MathematicalFunctions.class)
	void mathFunctionsShouldBeRenderedAsExpected(BuiltInFunctions.MathematicalFunctions function)
		throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

		if (function.getMinArgs() != function.getMaxArgs()) {
			Class<?>[] argTypes = new Class<?>[2];
			argTypes[0] = Expression.class;
			argTypes[1] = Expression[].class;
			@SuppressWarnings("deprecation") Method m = Functions.class.getMethod(function.name().toLowerCase(Locale.ROOT), argTypes);

			Expression arg1 = Cypher.literalOf(1);
			for (int n = 0; n <= function.getMaxArgs() - function.getMinArgs(); ++n) {
				Expression[] vargs = new Expression[n];
				StringBuilder expected = new StringBuilder("RETURN " + function.getImplementationName() + "(1");
				for (int i = 0; i < n; ++i) {
					vargs[i] = Cypher.literalOf(i);
					expected.append(", ");
					expected.append(i);
				}
				FunctionInvocation f = (FunctionInvocation) m.invoke(null, arg1, vargs);
				expected.append(");");
				assertThat(cypherRenderer.render(Cypher.returning(f).build()) + ";")
					.isEqualTo(expected.toString());
			}
		} else {
			StringBuilder expected = new StringBuilder("RETURN " + function.getImplementationName() + "(");

			int n = function.getMinArgs();
			Class<?>[] argTypes = new Class<?>[n];
			Expression[] args = new Expression[n];

			for (int i = 0; i < n; ++i) {
				argTypes[i] = Expression.class;
				args[i] = Cypher.literalOf(i);
				if (i > 0) {
					expected.append(", ");
				}
				expected.append(i);
			}
			expected.append(");");

			@SuppressWarnings("deprecation") Method m = Functions.class.getMethod(function.name().toLowerCase(Locale.ROOT), argTypes);
			FunctionInvocation f = (FunctionInvocation) m.invoke(null, (Object[]) args);
			assertThat(cypherRenderer.render(Cypher.returning(f).build()) + ";")
				.isEqualTo(expected.toString());
		}
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("functionsToTest")
	void functionShouldBeRenderedAsExpected(FunctionInvocation functionInvocation, String expected) {
		assertThat(cypherRenderer.render(Cypher.returning(functionInvocation).build())).isEqualTo(expected);
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("neo5jSpecificFunctions")
	void neo5jSpecificFunctionsShouldWork(FunctionInvocation functionInvocation, String expected) {
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(Dialect.NEO4J_5).build());
		assertThat(renderer.render(Cypher.returning(functionInvocation).build())).isEqualTo(expected);
	}

	@Test
	void assortedErrors() {
		var literalExpression = Cypher.literalOf("something");
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.left(literalExpression, null)).withMessage("length might not be null when the expression is not null");
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.right(literalExpression, null)).withMessage("length might not be null when the expression is not null");
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.substring(literalExpression, null, null)).withMessage("start is required");
	}

	private static Stream<Arguments> neo5jSpecificFunctions() {
		Node n = Cypher.node("Node").named("n");
		Node m = Cypher.node("Node2").named("m");
		Relationship r = n.relationshipTo(m).named("r");

		return Stream.of(
			Arguments.of(Cypher.elementId(n), "RETURN elementId(n)"),
			Arguments.of(Cypher.elementId(r), "RETURN elementId(r)")
		);
	}

	private static Stream<Arguments> functionsToTest() {
		Node n = Cypher.node("Node").named("n");
		Node m = Cypher.node("Node2").named("m");
		Relationship r = n.relationshipTo(m).named("r");
		Expression e1 = Cypher.name("e1");
		Expression e2 = Cypher.name("e2");
		FunctionInvocation p1 = Cypher.point(Cypher.mapOf("latitude", Cypher.literalOf(1), "longitude", Cypher
			.literalOf(2)));
		FunctionInvocation p2 = Cypher.point(Cypher.mapOf("latitude", Cypher.literalOf(3), "longitude", Cypher
			.literalOf(4)));

		// NOTE: Not all of those return valid Cypher statements. They are used only for integration testing the function calls so far.
		@SuppressWarnings("deprecation") var idOfRel = Functions.id(r);
		return Stream.of(
			Arguments.of(Cypher.left(null, null), "RETURN left(NULL, NULL)"),
			Arguments.of(Cypher.left(Cypher.literalOf("hello"), Cypher.literalOf(3)), "RETURN left('hello', 3)"),
			Arguments.of(Cypher.ltrim(Cypher.literalOf("   hello")), "RETURN ltrim('   hello')"),
			Arguments.of(Cypher.replace(Cypher.literalOf("hello"), Cypher.literalOf("l"), Cypher.literalOf("w")), "RETURN replace('hello', 'l', 'w')"),
			Arguments.of(Cypher.reverse(Cypher.literalOf("hello")), "RETURN reverse('hello')"),
			Arguments.of(Cypher.right(null, null), "RETURN right(NULL, NULL)"),
			Arguments.of(Cypher.right(Cypher.literalOf("hello"), Cypher.literalOf(3)), "RETURN right('hello', 3)"),
			Arguments.of(Cypher.rtrim(Cypher.literalOf("   hello  ")), "RETURN rtrim('   hello  ')"),
			Arguments.of(Cypher.substring(Cypher.literalOf("hello"), Cypher.literalOf(1), Cypher.literalOf(3)), "RETURN substring('hello', 1, 3)"),
			Arguments.of(Cypher.substring(Cypher.literalOf("hello"), Cypher.literalOf(2), null), "RETURN substring('hello', 2)"),
			Arguments.of(Cypher.toStringOrNull(Cypher.literalOf("hello")), "RETURN toStringOrNull('hello')"),
			Arguments.of(idOfRel, "RETURN id(r)"),
			Arguments.of(Cypher.elementId(n), "RETURN toString(id(n))"),
			Arguments.of(Cypher.elementId(r), "RETURN toString(id(r))"),
			Arguments.of(Cypher.keys(n), "RETURN keys(n)"),
			Arguments.of(Cypher.keys(r), "RETURN keys(r)"),
			Arguments.of(Cypher.keys(e1), "RETURN keys(e1)"),
			Arguments.of(Cypher.labels(n), "RETURN labels(n)"),
			Arguments.of(Cypher.type(r), "RETURN type(r)"),
			Arguments.of(Cypher.count(n), "RETURN count(n)"),
			Arguments.of(Cypher.countDistinct(n), "RETURN count(DISTINCT n)"),
			Arguments.of(Cypher.count(e1), "RETURN count(e1)"),
			Arguments.of(Cypher.countDistinct(e1), "RETURN count(DISTINCT e1)"),
			Arguments.of(Cypher.coalesce(e1, e2), "RETURN coalesce(e1, e2)"),
			Arguments.of(Cypher.toLower(e1), "RETURN toLower(e1)"),
			Arguments.of(Cypher.toUpper(e1), "RETURN toUpper(e1)"),
			Arguments.of(Cypher.trim(e1), "RETURN trim(e1)"),
			Arguments.of(Cypher.split(e1, Cypher.literalOf(",")), "RETURN split(e1, ',')"),
			Arguments.of(Cypher.size(e1), "RETURN size(e1)"),
			Arguments.of(Cypher.size(r), "RETURN size((n:`Node`)-[r]->(m:`Node2`))"),
			Arguments.of(Cypher.exists(e1), "RETURN exists(e1)"),
			Arguments.of(Cypher.distance(p1, p2),
				"RETURN distance(point({latitude: 1, longitude: 2}), point({latitude: 3, longitude: 4}))"),
			Arguments.of(Cypher.avg(e1), "RETURN avg(e1)"),
			Arguments.of(Cypher.avgDistinct(e1), "RETURN avg(DISTINCT e1)"),
			Arguments.of(Cypher.collect(e1), "RETURN collect(e1)"),
			Arguments.of(Cypher.collectDistinct(e1), "RETURN collect(DISTINCT e1)"),
			Arguments.of(Cypher.collect(n), "RETURN collect(n)"),
			Arguments.of(Cypher.collectDistinct(n), "RETURN collect(DISTINCT n)"),
			Arguments.of(Cypher.max(e1), "RETURN max(e1)"),
			Arguments.of(Cypher.maxDistinct(e1), "RETURN max(DISTINCT e1)"),
			Arguments.of(Cypher.min(e1), "RETURN min(e1)"),
			Arguments.of(Cypher.minDistinct(e1), "RETURN min(DISTINCT e1)"),
			Arguments.of(Cypher.percentileCont(e1, 0.4), "RETURN percentileCont(e1, 0.4)"),
			Arguments.of(Cypher.percentileContDistinct(e1, 0.4), "RETURN percentileCont(DISTINCT e1, 0.4)"),
			Arguments.of(Cypher.percentileDisc(e1, 0.4), "RETURN percentileDisc(e1, 0.4)"),
			Arguments.of(Cypher.percentileDiscDistinct(e1, 0.4), "RETURN percentileDisc(DISTINCT e1, 0.4)"),
			Arguments.of(Cypher.stDev(e1), "RETURN stDev(e1)"),
			Arguments.of(Cypher.stDevDistinct(e1), "RETURN stDev(DISTINCT e1)"),
			Arguments.of(Cypher.stDevP(e1), "RETURN stDevP(e1)"),
			Arguments.of(Cypher.stDevPDistinct(e1), "RETURN stDevP(DISTINCT e1)"),
			Arguments.of(Cypher.sum(e1), "RETURN sum(e1)"),
			Arguments.of(Cypher.sumDistinct(e1), "RETURN sum(DISTINCT e1)"),
			Arguments.of(Cypher.range(Cypher.literalOf(1), Cypher.literalOf(3)), "RETURN range(1, 3)"),
			Arguments.of(Cypher.range(Cypher.literalOf(1), Cypher.literalOf(3), Cypher.literalOf(2)),
				"RETURN range(1, 3, 2)"),
			Arguments.of(Cypher.head(e1), "RETURN head(e1)"),
			Arguments.of(Cypher.last(e1), "RETURN last(e1)"),
			Arguments.of(Cypher.nodes(Cypher.path("p").definedBy(r)), "RETURN nodes(p)"),
			Arguments.of(Cypher.length(Cypher.path("p").definedBy(r)), "RETURN length(p)"),
			Arguments.of(Cypher.shortestPath(r), "RETURN shortestPath((n:`Node`)-[r]->(m:`Node2`))"),
			Arguments.of(Cypher.properties(n), "RETURN properties(n)"),
			Arguments.of(Cypher.properties(r), "RETURN properties(r)"),
			Arguments.of(Cypher.properties(Cypher.mapOf("a", Cypher.literalOf("b"))), "RETURN properties({a: 'b'})"),
			Arguments.of(Cypher.relationships(Cypher.path("p").definedBy(r)), "RETURN relationships(p)"),
			Arguments.of(Cypher.startNode(r), "RETURN startNode(r)"),
			Arguments.of(Cypher.endNode(r), "RETURN endNode(r)"),
			Arguments.of(Cypher.date(), "RETURN date()"),
			Arguments.of(Cypher.calendarDate(2020, 9, 21), "RETURN date({year: 2020, month: 9, day: 21})"),
			Arguments.of(Cypher.weekDate(1984, 10, 3), "RETURN date({year: 1984, week: 10, dayOfWeek: 3})"),
			Arguments.of(Cypher.weekDate(1984, 10, null), "RETURN date({year: 1984, week: 10})"),
			Arguments.of(Cypher.weekDate(1984, null, null), "RETURN date({year: 1984})"),
			Arguments
				.of(Cypher.quarterDate(1984, 10, 45), "RETURN date({year: 1984, quarter: 10, dayOfQuarter: 45})"),
			Arguments.of(Cypher.quarterDate(1984, 10, null), "RETURN date({year: 1984, quarter: 10})"),
			Arguments.of(Cypher.quarterDate(1984, null, null), "RETURN date({year: 1984})"),
			Arguments.of(Cypher.ordinalDate(1984, 202), "RETURN date({year: 1984, ordinalDay: 202})"),
			Arguments.of(Cypher.ordinalDate(1984, null), "RETURN date({year: 1984})"),
			Arguments.of(Cypher.date(Cypher.mapOf("year", Cypher.literalOf(2020))), "RETURN date({year: 2020})"),
			Arguments.of(Cypher.date("2020-09-15"), "RETURN date('2020-09-15')"),
			Arguments.of(Cypher.date(Cypher.parameter("$myDateParameter")), "RETURN date($myDateParameter)"),
			Arguments.of(Cypher.datetime(), "RETURN datetime()"),
			Arguments.of(Cypher.datetime(
				Cypher.mapOf("year", Cypher.literalOf(1984), "month", Cypher.literalOf(10), "day",
					Cypher.literalOf(11), "hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "timezone",
					Cypher.literalOf("Europe/Stockholm")
				)
			), "RETURN datetime({year: 1984, month: 10, day: 11, hour: 12, minute: 31, timezone: 'Europe/Stockholm'})"),
			Arguments
				.of(Cypher.datetime("2015-07-21T21:40:32.142+0100"),
					"RETURN datetime('2015-07-21T21:40:32.142+0100')"),
			Arguments.of(Cypher.datetime(Cypher.parameter("$myDateParameter")), "RETURN datetime($myDateParameter)"),
			Arguments.of(Cypher.datetime(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN datetime({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Cypher.localdatetime(), "RETURN localdatetime()"),
			Arguments.of(Cypher.localdatetime(
				Cypher.mapOf("year", Cypher.literalOf(1984), "month", Cypher.literalOf(10), "day",
					Cypher.literalOf(11), "hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "second",
					Cypher.literalOf(14), "millisecond", Cypher.literalOf(123), "microsecond", Cypher.literalOf(456),
					"nanosecond", Cypher.literalOf(789)
				)
				),
				"RETURN localdatetime({year: 1984, month: 10, day: 11, hour: 12, minute: 31, second: 14, millisecond: 123, microsecond: 456, nanosecond: 789})"),
			Arguments
				.of(Cypher.localdatetime("2015-07-21T21:40:32.142"),
					"RETURN localdatetime('2015-07-21T21:40:32.142')"),
			Arguments.of(Cypher.localdatetime(Cypher.parameter("$myDateParameter")),
				"RETURN localdatetime($myDateParameter)"),
			Arguments.of(Cypher.localdatetime(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN localdatetime({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Cypher.localtime(), "RETURN localtime()"),
			Arguments.of(Cypher.localtime(
				Cypher.mapOf("hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "second",
					Cypher.literalOf(14), "millisecond", Cypher.literalOf(123), "microsecond", Cypher.literalOf(456),
					"nanosecond", Cypher.literalOf(789)
				)
				),
				"RETURN localtime({hour: 12, minute: 31, second: 14, millisecond: 123, microsecond: 456, nanosecond: 789})"),
			Arguments
				.of(Cypher.localtime("21:40:32.142"), "RETURN localtime('21:40:32.142')"),
			Arguments
				.of(Cypher.localtime(Cypher.parameter("$myDateParameter")), "RETURN localtime($myDateParameter)"),
			Arguments.of(Cypher.localtime(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN localtime({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Cypher.time(), "RETURN time()"),
			Arguments.of(Cypher.time(
				Cypher.mapOf("hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "second",
					Cypher.literalOf(14), "millisecond", Cypher.literalOf(123), "microsecond", Cypher.literalOf(456),
					"nanosecond", Cypher.literalOf(789)
				)
			), "RETURN time({hour: 12, minute: 31, second: 14, millisecond: 123, microsecond: 456, nanosecond: 789})"),
			Arguments
				.of(Cypher.time("21:40:32.142"), "RETURN time('21:40:32.142')"),
			Arguments.of(Cypher.time(Cypher.parameter("$myDateParameter")), "RETURN time($myDateParameter)"),
			Arguments.of(Cypher.time(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN time({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Cypher.duration(
				Cypher
					.mapOf("days", Cypher.literalOf(14), "hours", Cypher.literalOf(16), "minutes", Cypher.literalOf(12))
			), "RETURN duration({days: 14, hours: 16, minutes: 12})"),
			Arguments
				.of(Cypher.duration("P14DT16H12M"), "RETURN duration('P14DT16H12M')"),
			Arguments.of(Cypher.duration(Cypher.parameter("$myDateParameter")), "RETURN duration($myDateParameter)"),
			Arguments.of(Cypher.toInteger(Cypher.literalOf("23")), "RETURN toInteger('23')"),
			Arguments.of(Cypher.toString(Cypher.literalOf(23)), "RETURN toString(23)"),
			Arguments.of(Cypher.toFloat(Cypher.literalOf("23.42")), "RETURN toFloat('23.42')"),
			Arguments.of(Cypher.toBoolean(Cypher.literalOf("false")), "RETURN toBoolean('false')"),
			Arguments.of(Cypher.randomUUID(), "RETURN randomUUID()"),
			Arguments.of(Cypher.cartesian(2.3, 4.5), "RETURN point({x: 2.3, y: 4.5})"),
			Arguments.of(Cypher.coordinate(56.7, 12.78), "RETURN point({longitude: 56.7, latitude: 12.78})")
		);
	}

	@Test // GH-777
	void isEmptyShouldWork() {

		var p = Cypher.node("Person").named("p");
		var stmt = Cypher.match(p)
			.where(Cypher.not(Cypher.isEmpty(p.property("nationality"))))
			.returning(p.property("name"), p.property("nationality"))
			.build().getCypher();

		assertThat(stmt).isEqualTo("MATCH (p:`Person`) " +
			"WHERE NOT (isEmpty(p.nationality)) " +
			"RETURN p.name, p.nationality");
	}

	@Nested // GH-728
	class Graphs {

		@Test
		void namesShouldWork() {
			assertThat(Cypher.returning(Cypher.graphNames().as("name")).build().getCypher()).isEqualTo("RETURN graph.names() AS name");
		}

		@Test
		void propertiesByNameShouldWork() {
			var name = Cypher.name("name");
			var stmnt = Cypher.unwind(Cypher.graphNames()).as(name)
				.returning(name, Cypher.graphPropertiesByName(name).as("props"))
				.build();
			assertThat(stmnt.getCypher()).isEqualTo("UNWIND graph.names() AS name RETURN name, graph.propertiesByName(name) AS props");
		}

		@Test
		void byNameShouldWork() {
			var name = Cypher.name("graphName");
			var stmnt = Cypher.unwind(Cypher.graphNames()).as(name)
				.call(Cypher.use(
					Cypher.graphByName(name),
					Cypher.match(Cypher.anyNode("n")).returning(Cypher.name("n")).build())
				)
				.returning(Cypher.name("n"))
				.build();
			assertThat(stmnt.getCypher()).isEqualTo("UNWIND graph.names() AS graphName CALL {USE graph.byName(graphName) MATCH (n) RETURN n} RETURN n");
		}
	}
}
