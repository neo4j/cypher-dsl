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
package org.neo4j.cypherdsl.core;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Locale;
import java.util.TimeZone;
import java.util.stream.Stream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.MethodSource;
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
		FunctionInvocation p = Functions.properties(source);
		Node copy = Cypher.node("MovieCopy").named("copy");

		String query = cypherRenderer.render(
			Cypher.match(source).create(copy).set(copy, p).returning(copy).build()
		);
		Assertions.assertThat(query).isEqualTo(
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
			FunctionInvocation listToReduce = Functions.relationships(namedPath);
			SymbolicName n = Cypher.name("n");
			Statement statement = Cypher.returning(
				Functions.reduce(n)
					.in(listToReduce)
					.map(x.add(n))
					.accumulateOn(x)
					.withInitialValueOf(emptyList)
			).build();

			Assertions.assertThat(cypherRenderer.render(statement))
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
					Functions.reduce(n)
						.in(Functions.nodes(p))
						.map(totalAge.add(Cypher.property(n, "age")))
						.accumulateOn(totalAge)
						.withInitialValueOf(Cypher.literalOf(0)).as("reduction")
				).build();
			Assertions.assertThat(cypherRenderer.render(statement))
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
				Functions.reduce(n)
					.in(Cypher.listWith(x)
						.in(Functions.range(0, 10))
						.where(x.remainder(Cypher.literalOf(2)).isEqualTo(Cypher.literalOf(0)))
						.returning(x.pow(Cypher.literalOf(3)))
					)
					.map(totalAge.add(n))
					.accumulateOn(totalAge)
					.withInitialValueOf(Cypher.literalOf(0.0)).as("result"))
				.build();

			Assertions.assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"RETURN reduce(totalAge = 0.0, n IN [x IN range(0, 10) WHERE (x % 2) = 0 | x^3] | (totalAge + n)) AS result");
		}
	}

	@ParameterizedTest
	@EnumSource(BuiltInFunctions.MathematicalFunctions.class)
	void mathFunctionsShouldBeRenderedAsExpected(BuiltInFunctions.MathematicalFunctions function)
		throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {

		if (function.getMinArgs() != function.getMaxArgs()) {
			Class[] argTypes = new Class[2];
			argTypes[0] = Expression.class;
			argTypes[1] = Expression[].class;
			Method m = Functions.class.getMethod(function.name().toLowerCase(Locale.ROOT), argTypes);

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
				Assertions.assertThat(cypherRenderer.render(Cypher.returning(f).build()) + ";")
					.isEqualTo(expected.toString());
			}
		} else {
			StringBuilder expected = new StringBuilder("RETURN " + function.getImplementationName() + "(");

			int n = function.getMinArgs();
			Class[] argTypes = new Class[n];
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

			Method m = Functions.class.getMethod(function.name().toLowerCase(Locale.ROOT), argTypes);
			FunctionInvocation f = (FunctionInvocation) m.invoke(null, (Object[]) args);
			Assertions.assertThat(cypherRenderer.render(Cypher.returning(f).build()) + ";")
				.isEqualTo(expected.toString());
		}
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("functionsToTest")
	void functionShouldBeRenderedAsExpected(FunctionInvocation functionInvocation, String expected) {
		Assertions.assertThat(cypherRenderer.render(Cypher.returning(functionInvocation).build())).isEqualTo(expected);
	}

	private static Stream<Arguments> functionsToTest() {
		Node n = Cypher.node("Node").named("n");
		Node m = Cypher.node("Node2").named("m");
		Relationship r = n.relationshipTo(m).named("r");
		Expression e1 = Cypher.name("e1");
		Expression e2 = Cypher.name("e2");
		FunctionInvocation p1 = Functions.point(Cypher.mapOf("latitude", Cypher.literalOf(1), "longitude", Cypher
			.literalOf(2)));
		FunctionInvocation p2 = Functions.point(Cypher.mapOf("latitude", Cypher.literalOf(3), "longitude", Cypher
			.literalOf(4)));

		// NOTE: Not all of those return valid Cypher statements. They are used only for integration testing the function calls so far.
		return Stream.of(
			Arguments.of(Functions.id(n), "RETURN id(n)"),
			Arguments.of(Functions.id(r), "RETURN id(r)"),
			Arguments.of(Functions.keys(n), "RETURN keys(n)"),
			Arguments.of(Functions.keys(r), "RETURN keys(r)"),
			Arguments.of(Functions.keys(e1), "RETURN keys(e1)"),
			Arguments.of(Functions.labels(n), "RETURN labels(n)"),
			Arguments.of(Functions.type(r), "RETURN type(r)"),
			Arguments.of(Functions.count(n), "RETURN count(n)"),
			Arguments.of(Functions.countDistinct(n), "RETURN count(DISTINCT n)"),
			Arguments.of(Functions.count(e1), "RETURN count(e1)"),
			Arguments.of(Functions.countDistinct(e1), "RETURN count(DISTINCT e1)"),
			Arguments.of(Functions.coalesce(e1, e2), "RETURN coalesce(e1, e2)"),
			Arguments.of(Functions.toLower(e1), "RETURN toLower(e1)"),
			Arguments.of(Functions.size(e1), "RETURN size(e1)"),
			Arguments.of(Functions.size(r), "RETURN size((n:`Node`)-[r]->(m:`Node2`))"),
			Arguments.of(Functions.exists(e1), "RETURN exists(e1)"),
			Arguments.of(Functions.distance(p1, p2),
				"RETURN distance(point({latitude: 1, longitude: 2}), point({latitude: 3, longitude: 4}))"),
			Arguments.of(Functions.avg(e1), "RETURN avg(e1)"),
			Arguments.of(Functions.avgDistinct(e1), "RETURN avg(DISTINCT e1)"),
			Arguments.of(Functions.collect(e1), "RETURN collect(e1)"),
			Arguments.of(Functions.collectDistinct(e1), "RETURN collect(DISTINCT e1)"),
			Arguments.of(Functions.collect(n), "RETURN collect(n)"),
			Arguments.of(Functions.collectDistinct(n), "RETURN collect(DISTINCT n)"),
			Arguments.of(Functions.max(e1), "RETURN max(e1)"),
			Arguments.of(Functions.maxDistinct(e1), "RETURN max(DISTINCT e1)"),
			Arguments.of(Functions.min(e1), "RETURN min(e1)"),
			Arguments.of(Functions.minDistinct(e1), "RETURN min(DISTINCT e1)"),
			Arguments.of(Functions.percentileCont(e1, 0.4), "RETURN percentileCont(e1, 0.4)"),
			Arguments.of(Functions.percentileContDistinct(e1, 0.4), "RETURN percentileCont(DISTINCT e1, 0.4)"),
			Arguments.of(Functions.percentileDisc(e1, 0.4), "RETURN percentileDisc(e1, 0.4)"),
			Arguments.of(Functions.percentileDiscDistinct(e1, 0.4), "RETURN percentileDisc(DISTINCT e1, 0.4)"),
			Arguments.of(Functions.stDev(e1), "RETURN stDev(e1)"),
			Arguments.of(Functions.stDevDistinct(e1), "RETURN stDev(DISTINCT e1)"),
			Arguments.of(Functions.stDevP(e1), "RETURN stDevP(e1)"),
			Arguments.of(Functions.stDevPDistinct(e1), "RETURN stDevP(DISTINCT e1)"),
			Arguments.of(Functions.sum(e1), "RETURN sum(e1)"),
			Arguments.of(Functions.sumDistinct(e1), "RETURN sum(DISTINCT e1)"),
			Arguments.of(Functions.range(Cypher.literalOf(1), Cypher.literalOf(3)), "RETURN range(1, 3)"),
			Arguments.of(Functions.range(Cypher.literalOf(1), Cypher.literalOf(3), Cypher.literalOf(2)),
				"RETURN range(1, 3, 2)"),
			Arguments.of(Functions.head(e1), "RETURN head(e1)"),
			Arguments.of(Functions.last(e1), "RETURN last(e1)"),
			Arguments.of(Functions.nodes(Cypher.path("p").definedBy(r)), "RETURN nodes(p)"),
			Arguments.of(Functions.shortestPath(r), "RETURN shortestPath((n:`Node`)-[r]->(m:`Node2`))"),
			Arguments.of(Functions.properties(n), "RETURN properties(n)"),
			Arguments.of(Functions.properties(r), "RETURN properties(r)"),
			Arguments.of(Functions.properties(Cypher.mapOf("a", Cypher.literalOf("b"))), "RETURN properties({a: 'b'})"),
			Arguments.of(Functions.relationships(Cypher.path("p").definedBy(r)), "RETURN relationships(p)"),
			Arguments.of(Functions.startNode(r), "RETURN startNode(r)"),
			Arguments.of(Functions.endNode(r), "RETURN endNode(r)"),
			Arguments.of(Functions.date(), "RETURN date()"),
			Arguments.of(Functions.calendarDate(2020, 9, 21), "RETURN date({year: 2020, month: 9, day: 21})"),
			Arguments.of(Functions.weekDate(1984, 10, 3), "RETURN date({year: 1984, week: 10, dayOfWeek: 3})"),
			Arguments.of(Functions.weekDate(1984, 10, null), "RETURN date({year: 1984, week: 10})"),
			Arguments.of(Functions.weekDate(1984, null, null), "RETURN date({year: 1984})"),
			Arguments
				.of(Functions.quarterDate(1984, 10, 45), "RETURN date({year: 1984, quarter: 10, dayOfQuarter: 45})"),
			Arguments.of(Functions.quarterDate(1984, 10, null), "RETURN date({year: 1984, quarter: 10})"),
			Arguments.of(Functions.quarterDate(1984, null, null), "RETURN date({year: 1984})"),
			Arguments.of(Functions.ordinalDate(1984, 202), "RETURN date({year: 1984, ordinalDay: 202})"),
			Arguments.of(Functions.ordinalDate(1984, null), "RETURN date({year: 1984})"),
			Arguments.of(Functions.date(Cypher.mapOf("year", Cypher.literalOf(2020))), "RETURN date({year: 2020})"),
			Arguments.of(Functions.date("2020-09-15"), "RETURN date('2020-09-15')"),
			Arguments.of(Functions.date(Cypher.parameter("$myDateParameter")), "RETURN date($myDateParameter)"),
			Arguments.of(Functions.datetime(), "RETURN datetime()"),
			Arguments.of(Functions.datetime(
				Cypher.mapOf("year", Cypher.literalOf(1984), "month", Cypher.literalOf(10), "day",
					Cypher.literalOf(11), "hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "timezone",
					Cypher.literalOf("Europe/Stockholm")
				)
			), "RETURN datetime({year: 1984, month: 10, day: 11, hour: 12, minute: 31, timezone: 'Europe/Stockholm'})"),
			Arguments
				.of(Functions.datetime("2015-07-21T21:40:32.142+0100"),
					"RETURN datetime('2015-07-21T21:40:32.142+0100')"),
			Arguments.of(Functions.datetime(Cypher.parameter("$myDateParameter")), "RETURN datetime($myDateParameter)"),
			Arguments.of(Functions.datetime(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN datetime({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Functions.localdatetime(), "RETURN localdatetime()"),
			Arguments.of(Functions.localdatetime(
				Cypher.mapOf("year", Cypher.literalOf(1984), "month", Cypher.literalOf(10), "day",
					Cypher.literalOf(11), "hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "second",
					Cypher.literalOf(14), "millisecond", Cypher.literalOf(123), "microsecond", Cypher.literalOf(456),
					"nanosecond", Cypher.literalOf(789)
				)
				),
				"RETURN localdatetime({year: 1984, month: 10, day: 11, hour: 12, minute: 31, second: 14, millisecond: 123, microsecond: 456, nanosecond: 789})"),
			Arguments
				.of(Functions.localdatetime("2015-07-21T21:40:32.142"),
					"RETURN localdatetime('2015-07-21T21:40:32.142')"),
			Arguments.of(Functions.localdatetime(Cypher.parameter("$myDateParameter")),
				"RETURN localdatetime($myDateParameter)"),
			Arguments.of(Functions.localdatetime(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN localdatetime({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Functions.localtime(), "RETURN localtime()"),
			Arguments.of(Functions.localtime(
				Cypher.mapOf("hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "second",
					Cypher.literalOf(14), "millisecond", Cypher.literalOf(123), "microsecond", Cypher.literalOf(456),
					"nanosecond", Cypher.literalOf(789)
				)
				),
				"RETURN localtime({hour: 12, minute: 31, second: 14, millisecond: 123, microsecond: 456, nanosecond: 789})"),
			Arguments
				.of(Functions.localtime("21:40:32.142"), "RETURN localtime('21:40:32.142')"),
			Arguments
				.of(Functions.localtime(Cypher.parameter("$myDateParameter")), "RETURN localtime($myDateParameter)"),
			Arguments.of(Functions.localtime(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN localtime({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Functions.time(), "RETURN time()"),
			Arguments.of(Functions.time(
				Cypher.mapOf("hour", Cypher.literalOf(12), "minute", Cypher.literalOf(31), "second",
					Cypher.literalOf(14), "millisecond", Cypher.literalOf(123), "microsecond", Cypher.literalOf(456),
					"nanosecond", Cypher.literalOf(789)
				)
			), "RETURN time({hour: 12, minute: 31, second: 14, millisecond: 123, microsecond: 456, nanosecond: 789})"),
			Arguments
				.of(Functions.time("21:40:32.142"), "RETURN time('21:40:32.142')"),
			Arguments.of(Functions.time(Cypher.parameter("$myDateParameter")), "RETURN time($myDateParameter)"),
			Arguments.of(Functions.time(TimeZone.getTimeZone("America/Los_Angeles")),
				"RETURN time({timezone: 'America/Los_Angeles'})"),
			Arguments.of(Functions.duration(
				Cypher
					.mapOf("days", Cypher.literalOf(14), "hours", Cypher.literalOf(16), "minutes", Cypher.literalOf(12))
			), "RETURN duration({days: 14, hours: 16, minutes: 12})"),
			Arguments
				.of(Functions.duration("P14DT16H12M"), "RETURN duration('P14DT16H12M')"),
			Arguments.of(Functions.duration(Cypher.parameter("$myDateParameter")), "RETURN duration($myDateParameter)")
		);
	}
}
