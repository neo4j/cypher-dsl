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
package org.neo4j.cypherdsl.parser;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatNoException;
import static org.assertj.core.api.Assertions.assertThatRuntimeException;

import java.util.Arrays;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.Clause;
import org.neo4j.cypherdsl.core.Clauses;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.FunctionInvocation;
import org.neo4j.cypherdsl.core.Literal;
import org.neo4j.cypherdsl.core.Match;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.RelationshipPattern;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.Where;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class CypherParserTest {

	@Nested
	class RelationshipPatterns {

		@ParameterizedTest
		@CsvSource(nullValues = "N/A", value = {"N/A, N/A", "N/A, 5", "5, N/A", "5, 10", "-,-"})
		void simplePatternWithVariousLengths(String minimum, String maximum) {
			StringBuilder simplePattern = new StringBuilder("(n)-");
			if (minimum != null || maximum != null) {
				simplePattern.append("[*");
				if (minimum != null && !"-".equals(minimum)) {
					simplePattern.append(minimum);
				}
				if (!"-".equals(maximum)) {
					simplePattern.append("..");
					if (maximum != null) {
						simplePattern.append(maximum);
					}
				}
				simplePattern.append("]");
			}
			simplePattern.append("->(m)");
			var rel = CypherParser.parseRelationship(simplePattern.toString());
			assertThat(Cypher.match(rel).returning(Cypher.asterisk()).build().getCypher())
				.isEqualTo(String.format("MATCH %s RETURN *", simplePattern));
		}

		@ParameterizedTest
		@ValueSource(strings = {"T", "T1|T2", "T1|T2|T3"})
		void types(String types) {
			var rel = CypherParser.parseRelationship(String.format("(n)-[:%s]->(m)", types));
			assertThat(Cypher.match(rel).returning(Cypher.asterisk()).build().getCypher())
				.isEqualTo(String.format("MATCH (n)-[:%s]->(m) RETURN *",
					Arrays.stream(types.split("\\|")).map(v -> String.format("`%s`", v))
						.collect(Collectors.joining("|"))));
		}

		@ParameterizedTest
		@CsvSource({"-,-", "<-,-", "-,->"})
		void direction(String left, String right) {
			StringBuilder simplePattern = new StringBuilder("(n)")
				.append(left)
				.append(right)
				.append("(m)");
			var rel = CypherParser.parseRelationship(simplePattern.toString());
			assertThat(Cypher.match(rel).returning(Cypher.asterisk()).build().getCypher())
				.isEqualTo(String.format("MATCH %s RETURN *", simplePattern));
		}

		@Test
		void pointyThingAtBothSidesIsNotSupported() {
			assertThatIllegalArgumentException().isThrownBy(() -> CypherParser.parseRelationship("(n)<-->(m)"))
				.withMessage("Only left-to-right, right-to-left or unidirectional path elements are supported.");
		}

		@Test
		void chain() {
			RelationshipPattern rel = CypherParser.parseRelationship("(n)-->()-->(o)");
			assertThat(Cypher.match(rel).returning(Cypher.asterisk()).build().getCypher())
				.isEqualTo("MATCH (n)-->()-->(o) RETURN *");
		}

		@Test
		void shortestPath() {
			Expression ex = CypherParser.parseExpression("shortestPath((n:A)-->(o:B))");
			assertThat(Cypher.returning(ex).build().getCypher())
				.isEqualTo("RETURN shortestPath((n:`A`)-->(o:`B`))");
		}

		@Test
		void allShortestPaths() {
			Expression ex = CypherParser.parseExpression("allShortestPaths((n:A)-->(o:B))");
			assertThat(Cypher.returning(ex).build().getCypher())
				.isEqualTo("RETURN allShortestPaths((n:`A`)-->(o:`B`))");
		}

		@Test
		void names() {
			RelationshipPattern rel = CypherParser.parseRelationship("(n)-[r1]->()-[r2]->(o)");
			assertThat(Cypher.match(rel).returning(Cypher.asterisk()).build().getCypher())
				.isEqualTo("MATCH (n)-[r1]->()-[r2]->(o) RETURN *");
		}

		@Test
		void properties() {
			RelationshipPattern rel = CypherParser.parseRelationship("(n)-[{a: 'b', c: 'd'}]->(o)");
			assertThat(Cypher.match(rel).returning(Cypher.asterisk()).build().getCypher())
				.isEqualTo("MATCH (n)-[ {a: 'b', c: 'd'}]->(o) RETURN *");
		}

		@Test
		void propertiesOnAChain() {
			RelationshipPattern rel = CypherParser
				.parseRelationship("(n)-[r:TYPE{x:'x'}]->(m)-[{a: 'b', c: 'd'}]->(o)");
			assertThat(Cypher.match(rel).returning(Cypher.asterisk()).build().getCypher())
				.isEqualTo("MATCH (n)-[r:`TYPE` {x: 'x'}]->(m)-[ {a: 'b', c: 'd'}]->(o) RETURN *");
		}
	}

	@Test
	void shouldProvideANiceErrorMessage() {

		assertThatExceptionOfType(UnsupportedCypherException.class)
			.isThrownBy(() -> CypherParser.parse("CREATE ROLE myrole"))
			.withMessage("You used one Cypher construct not yet supported by the Cypher-DSL:\n"
				+ "\n"
				+ "\tCREATE ROLE myrole\n"
				+ "\n"
				+ "Feel free to open an issue so that we might add support for it at https://github.com/neo4j-contrib/cypher-dsl/issues/new");
	}

	@Test
	void shouldParseCount() {
		assertExpression("Count(*)", "count(*)");
	}

	@Test
	void shouldParseIn() {
		assertExpression("n in [1,2,3]", "n IN [1, 2, 3]");
	}

	@ParameterizedTest
	@CsvSource(value = {
		"f()| f()",
		"foo.bar()| foo.bar()",
		"foo.bar(e)| foo.bar(e)",
		"foo.bar(e,f)| foo.bar(e, f)",
		"count(distinct e,f)| count(DISTINCT e, f)"
	}, delimiterString = "|")
	void shouldParseFunctionInvocation(String input, String expected) {
		assertExpression(input, expected);
	}

	@Nested
	class Literals {

		@Test
		void shouldParseListLiteral() {
			assertExpression("[1,2,a, 'b']", "[1, 2, a, 'b']");
		}

		@Test
		void shouldParseLookups() {
			assertExpression("n[23]", "n[23]");
		}
	}

	@Nested
	class Parameters {

		@Test
		void newParameterShouldWork() {
			assertExpression("$foo", "$foo");
		}

		@Test
		void newNumberedParameterShouldWork() {
			assertExpression("$1", "$1");
		}
	}

	static void assertExpression(String expression) {
		assertExpression(expression, expression);
	}

	static void assertExpression(String expression, String expected) {

		Expression e = CypherParser.parseExpression(expression);
		assertThat(Cypher.returning(e).build().getCypher())
			.isEqualTo(String.format("RETURN %s", expected));
	}

	static void assertNode(Node node, String cypherDslRepresentation) {

		assertThat(Cypher.match(node).returning(Cypher.asterisk()).build().getCypher())
			.isEqualTo(String.format("MATCH %s RETURN *", cypherDslRepresentation));
	}

	@Nested
	class Predicates {

		@Test
		void any() {

			assertThatIllegalArgumentException()
				.isThrownBy(() -> CypherParser.parseExpression("any(color IN n.liked_colors)"))
				.withMessage("any(...) requires a WHERE predicate");
			assertExpression("any(color IN n.liked_colors WHERE color = 'yellow')");
		}

		@Test
		void none() {

			assertThatIllegalArgumentException()
				.isThrownBy(() -> CypherParser.parseExpression("none(color IN n.liked_colors)"))
				.withMessage("none(...) requires a WHERE predicate");
			assertExpression("none(color IN n.liked_colors WHERE color = 'yellow')");
		}

		@Test
		void single() {

			assertThatIllegalArgumentException()
				.isThrownBy(() -> CypherParser.parseExpression("single(color IN n.liked_colors)"))
				.withMessage("single(...) requires a WHERE predicate");
			assertExpression("single(color IN n.liked_colors WHERE color = 'yellow')");
		}

		@Test
		void all() {

			assertThatIllegalArgumentException()
				.isThrownBy(() -> CypherParser.parseExpression("all(color IN n.liked_colors)"))
				.withMessage("all(...) requires a WHERE predicate");
			assertExpression("all(color IN n.liked_colors WHERE color = 'yellow')");
		}
	}

	@Test
	void onNewReturnItemCallbacksShouldBeApplied() {

		var cnt = new AtomicInteger(0);
		var options = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_RETURN_ITEM, AliasedExpression.class, e -> {
				assertThat(cnt.compareAndSet(0, 1)).isTrue();
				if (e instanceof AliasedExpression) {
					return (AliasedExpression) e;
				}
				return e.as("foo");
			})
			.withCallback(ExpressionCreatedEventType.ON_RETURN_ITEM, Expression.class, e -> {
				assertThat(cnt.compareAndSet(1, 2)).isTrue();
				return e;
			}).build();

		var statement = CypherParser.parseStatement("RETURN 1", options);
		assertThat(cnt.get()).isEqualTo(2);
		assertThat(statement.getCypher()).isEqualTo("RETURN 1 AS foo");
	}

	@Test
	void shouldParseReturnAll() {
		var statement1 = CypherParser.parseStatement("WITH 1 AS foo WITH *, 2 AS bar RETURN *");
		assertThat(statement1.getCypher()).isEqualTo("WITH 1 AS foo WITH *, 2 AS bar RETURN *");

		var statement2 = CypherParser.parseStatement("WITH 1 AS foo WITH *, 2 AS bar RETURN foo");
		assertThat(statement2.getCypher()).isEqualTo("WITH 1 AS foo WITH *, 2 AS bar RETURN foo");
	}

	@ParameterizedTest
	@ValueSource(strings = {"CREATE", "MERGE", "MATCH"})
	void patternElementCallBacksShouldBeApplied(String clause) {

		var builder = Options.newOptions();
		EnumSet.allOf(PatternElementCreatedEventType.class)
			.forEach(et ->
				builder.withCallback(et, patternElement -> {
						if (patternElement instanceof Node) {
							var existing = ((Node) patternElement).getLabels().get(0).getValue();
							return Cypher.node(existing, "FirstLabelAdded");
						}
						return patternElement;
					})
					.withCallback(et, patternElement -> {
						if (patternElement instanceof Node) {
							var l1 = ((Node) patternElement).getLabels().get(0).getValue();
							var l2 = ((Node) patternElement).getLabels().get(1).getValue();
							return Cypher.node(l1, l2, "SecondLabelAdded");
						}
						return patternElement;
					})
			);

		var options = builder.build();
		var statement = CypherParser.parseStatement(clause + " (n:Movie) RETURN n", options);
		assertThat(statement.getCypher()).isEqualTo(
			clause + " (:`Movie`:`FirstLabelAdded`:`SecondLabelAdded`) RETURN n");
	}

	@Test
	void shouldNotAllowInvalidCallbacks() {

		assertThatIllegalArgumentException().isThrownBy(() ->
			Options.newOptions()
				.withCallback(ExpressionCreatedEventType.ON_SET_PROPERTY, Expression.class, e -> e).build()
		).withMessage(
			"The type that is produced by 'ON_SET_PROPERTY' is not compatible with interface org.neo4j.cypherdsl.core.Expression");
	}

	private static Stream<Arguments> inputAndIdentifiableExpressions() {
		return Stream.of(
			Arguments.of("""
					CALL {
						MATCH (p:Person)-[:LIKES]->(:Technology {type: "Java"})
						RETURN p

						UNION

						MATCH (p:Person)
						WHERE size((p)-[:IS_FRIENDS_WITH]->()) > 1
						RETURN p
					}
					RETURN p.name AS person, p.birthdate AS dob
					ORDER BY dob DESC""",
				List.of("person", "dob")
			),
			Arguments.of("""
					MATCH p=(start)-[*]->(finish)
					WHERE start.name = 'A' AND finish.f = 'D'
					FOREACH (n IN nodes(p) | SET n.marked = true)""",
				List.of("finish", "start", "p")
			),
			Arguments.of("""
					MATCH (a)
					WHERE a.name = 'Eskil'
					RETURN a.array, [x IN a.array WHERE size(x) = 3]""",
				List.of("a.array")
			)
		);
	}

	@ParameterizedTest
	@MethodSource("inputAndIdentifiableExpressions")
	void parseAndIdentifyShouldWork(String cypher, List<String> expected) {

		var identifiables = CypherParser.parse(cypher).getCatalog().getIdentifiableExpressions();
		assertThat(identifiables.stream().map(Cypher::format))
			.containsExactlyInAnyOrderElementsOf(expected);
	}

	@Test
	void transformingWhereShouldWork() {

		var query = "MATCH (m:Movie {title: 'The Matrix'}) WHERE m.releaseYear IS NOT NULL OR false RETURN m";
		var options = Options.newOptions().withMatchClauseFactory(matchDefinition -> (Match) Clauses.match(matchDefinition.optional(), matchDefinition.patternElements(), Where.from(Cypher.isFalse()), matchDefinition.optionalHints())).build();
		var cypher = CypherParser.parse(query, options).getCypher();
		assertThat(cypher).isEqualTo("MATCH (m:`Movie` {title: 'The Matrix'}) WHERE false RETURN m");
	}

	@Test
	void labelColonConjunction() {
		assertThat(CypherParser.parse("MATCH (a:A:B:C) RETURN a").getCypher()).isEqualTo("MATCH (a:`A`:`B`:`C`) RETURN a");
	}

	@Test
	void labelColonDisjunction() {
		assertThat(CypherParser.parse("MATCH (wallstreet {title: 'Wall Street'})<-[:ACTED_IN|:DIRECTED]-(person) RETURN person.name").getCypher())
			.isEqualTo("MATCH (wallstreet {title: 'Wall Street'})<-[:`ACTED_IN`|`DIRECTED`]-(person) RETURN person.name");
	}

	@Test
	void newOrEdLabels() {
		String statement = "MATCH (:`Human`|`Occupation`)-[:`OCCUPATION`*0..1]-(h:`Human`) RETURN h";
		Statement parsed = CypherParser.parseStatement(statement);
		var result = parsed.getCypher();
		assertThat(result).isEqualTo("MATCH (:`Human`|`Occupation`)-[:`OCCUPATION`*0..1]-(h:`Human`) RETURN h");
	}

	@ParameterizedTest
	@ValueSource(strings = {
		"MATCH (h:A&B&C) RETURN h",
		"MATCH (h:!A&!B&!C) RETURN h",
		"MATCH (h:A|B|C) RETURN h",
		"MATCH (h:!A|!B|!C) RETURN h",
		"MATCH (h:A|B&C|D) RETURN h",
		"MATCH (h:A|!(B&C)|D) RETURN h",
		"MATCH (h:(A|B)&!(C|D)) RETURN h",
		"MATCH (h:(A|B)&(C|D)) RETURN h",
		"MATCH (h:!(A|B)&(C|D)) RETURN h",
		"MATCH (h:!(!(A|B)&(C|D))) RETURN h",
		"MATCH (h:A&B|C&D) RETURN h",
		"MATCH (h:(Human|Occupation|X)&!A|B|C) RETURN h"
	})
	void labelExpressions(String statement) {

		Statement parsed = CypherParser.parseStatement(statement);
		String cypher = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build()).render(parsed);
		assertThat(cypher).isEqualTo(statement);
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
		MATCH (h:(A&B)) RETURN h,MATCH (h:A&B) RETURN h
		MATCH (h:A|(B&C)|D) RETURN h,MATCH (h:A|B&C|D) RETURN h
		MATCH (h:(A&B)|C&D) RETURN h,MATCH (h:A&B|C&D) RETURN h
		MATCH (h:((A&B)|C&D)) RETURN h,MATCH (h:A&B|C&D) RETURN h
		MATCH (h:!!(A|B)&(C|D)) RETURN h,MATCH (h:(A|B)&(C|D)) RETURN h
		MATCH (h:!(!(A|B))&(C|D)) RETURN h,MATCH (h:(A|B)&(C|D)) RETURN h
		""")
	void optimizedLabelExpressions(String statement, String expected) {

		Statement parsed = CypherParser.parseStatement(statement);
		String cypher = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build()).render(parsed);
		assertThat(cypher).isEqualTo(expected);
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
		MATCH (n:Person WHERE n.name = 'Tom Hanks') RETURN n,MATCH (n:Person WHERE n.name = 'Tom Hanks') RETURN n
		MATCH (n:Person WHERE n.name = 'Tom Hanks' OR n.name = 'Bud Spencer') RETURN n,MATCH (n:Person WHERE (n.name = 'Tom Hanks' OR n.name = 'Bud Spencer')) RETURN n
		MATCH (n:Person WHERE n.name = 'Tom Hanks') WHERE n.born <= 2000 RETURN n,MATCH (n:Person WHERE n.name = 'Tom Hanks') WHERE n.born <= 2000 RETURN n
		MATCH (n:Person WHERE n.name = 'Tom Hanks' OR n.name = 'Bud Spencer') WHERE n.born <= 2000 RETURN n,MATCH (n:Person WHERE (n.name = 'Tom Hanks' OR n.name = 'Bud Spencer')) WHERE n.born <= 2000 RETURN n
		""")
	void whereInMatch(String statement, String expected) {

		Statement parsed = CypherParser.parseStatement(statement);
		String cypher = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build()).render(parsed);
		assertThat(cypher).isEqualTo(expected);
	}

	@Test
	void nodePatternInCall() {

		var statement = """
			match (n:Person)
			call {
			match (n:Movie {title: 'The Matrix'}) where n.released >= 1900 return n as m
			}
			return n.name
			""";
		Statement parsed = CypherParser.parseStatement(statement);
		String cypher = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build()).render(parsed);
		assertThat(cypher).isEqualTo("MATCH (n:Person) CALL {MATCH (n:Movie {title: 'The Matrix'}) WHERE n.released >= 1900 RETURN n AS m} RETURN n.name");
	}

	@Test
	void shouldBeAbleToParseIntoSortedMaps() {

		var in = """
			MATCH (this:Movie)
			WHERE this.released = $releaseYear
			CALL {
				WITH this
				MATCH (this_actorsAggregate_this1:Actor)-[this_actorsAggregate_this0:ACTED_IN]->(this)
				RETURN {
					min: min(this_actorsAggregate_this0.screentime),
					max: max(this_actorsAggregate_this0.screentime),
					average: avg(this_actorsAggregate_this0.screentime),
					sum: sum(this_actorsAggregate_this0.screentime)
				} AS this_actorsAggregate_var2
			}
			RETURN this {
				actorsAggregate: {
					edge: {
						screentime: this_actorsAggregate_var2
					}
				}
			} AS this
			""";

		var parsed = CypherParser.parse(in, Options.newOptions().createSortedMaps(true).build());
		var cypher = Renderer.getRenderer(Configuration.newConfig().withPrettyPrint(true).withIndentStyle(Configuration.IndentStyle.TAB).build()).render(parsed);
		assertThat(cypher)
			.isEqualTo("""
				MATCH (this:Movie)
				WHERE this.released = $releaseYear
				CALL {
					WITH this
					MATCH (this_actorsAggregate_this1:Actor)-[this_actorsAggregate_this0:ACTED_IN]->(this)
					RETURN {
						average: avg(this_actorsAggregate_this0.screentime),
						max: max(this_actorsAggregate_this0.screentime),
						min: min(this_actorsAggregate_this0.screentime),
						sum: sum(this_actorsAggregate_this0.screentime)
					} AS this_actorsAggregate_var2
				}
				RETURN this {
					actorsAggregate: {
						edge: {
							screentime: this_actorsAggregate_var2
						}
					}
				} AS this""");
	}

	@Test // GH-729
	void fillingParametersOfParsedCypherManual() {

		var oidValue = "look ma, no Cypher injection'}) MATCH (n) DETACH DELETE n --";
		String userProvidedCypher =
			"MATCH (p:`confignode` {oid: $oid}) " +
			"CALL apoc.path.subgraphAll(p, {relationshipFilter: 'BELONGS_TO_ARRAY|IN|IN_ARRAY', minLevel: 1, maxLevel: 3}) " +
			"YIELD nodes, relationships " +
			"FOREACH(node in nodes |detach delete node)" +
			"RETURN nodes";

		var options = Options.newOptions().withCallback(ExpressionCreatedEventType.ON_NEW_PARAMETER, Parameter.class, newExpression -> {
			var p = (Parameter<?>) newExpression;
			if ("oid".equals(p.getName())) {
				return Cypher.parameter(p.getName(), oidValue);
			}
			return p;
		}).build();

		var userStatement = CypherParser.parse(userProvidedCypher, options);

		var prettyPrintingRenderer = Renderer.getRenderer(Configuration.prettyPrinting());
		assertThat(prettyPrintingRenderer.render(userStatement))
			.isEqualTo("""
				MATCH (p:confignode {
				  oid: $oid
				}) CALL apoc.path.subgraphAll(p, {
				  relationshipFilter: 'BELONGS_TO_ARRAY|IN|IN_ARRAY',
				  minLevel: 1,
				  maxLevel: 3
				}) YIELD nodes, relationships FOREACH (node IN nodes | DETACH DELETE node)
				RETURN nodes""");

		assertThat(userStatement.getCatalog().getParameters()).containsEntry("oid", "look ma, no Cypher injection'}) MATCH (n) DETACH DELETE n --");
	}

	@Test // GH-729
	void fillingParametersOfParsedCypher() {

		var oidValue = "look ma, no Cypher injection'}) MATCH (n) DETACH DELETE n --";
		String userProvidedCypher =
			"MATCH (p:`confignode` {oid: $oid}) " +
			"CALL apoc.path.subgraphAll(p, {relationshipFilter: 'BELONGS_TO_ARRAY|IN|IN_ARRAY', minLevel: 1, maxLevel: 3}) " +
			"YIELD nodes, relationships " +
			"FOREACH(node in nodes |detach delete node)" +
			"RETURN nodes";

		var options = Options.newOptions().withParameterValues(Map.of("oid", oidValue)).build();
		var userStatement = CypherParser.parse(userProvidedCypher, options);
		assertThat(userStatement.getCatalog().getParameters()).containsEntry("oid", "look ma, no Cypher injection'}) MATCH (n) DETACH DELETE n --");
	}

	@Test // GH-739
	void literalCallbacks() {

		var literals = new LinkedHashSet<Literal<?>>();
		var options = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_NEW_LITERAL, Expression.class, l -> {
				literals.add((Literal<?>) l);
				return l;
			})
			.build();

		CypherParser.parse("RETURN NULL, 1.0 as d, 100 as l, 0x13af as h, 0o1372 as o, 'Hallo' as s, true, false, Inf, NaN", options);

		assertThat(literals.stream().map(Literal::getClass).distinct()).hasSize(6);
		assertThat(literals)
			.map(Literal::asString)
			.containsExactly("NULL", "1.0", "100", "5039", "762", "'Hallo'", "true", "false", "Infinity", "NaN");
	}

	@Nested
	class InvocationCallbacks {

		@Test
		void onNewFunctionCallbacksShouldBeInvoked() {

			var functions = new AtomicInteger(0);
			var options = Options.newOptions()
				// The best: doing nothing
				.withCallback(InvocationCreatedEventType.ON_INVOCATION, Expression.class, e -> {
					assertThat(functions.compareAndSet(0, 1)).isTrue();
					return e;
				})
				// Example of rewriting a function
				.withCallback(InvocationCreatedEventType.ON_INVOCATION, FunctionInvocation.class, e -> {
					assertThat(functions.compareAndSet(1, 2)).isTrue();
					if ("asString".equals(e.getFunctionName())) {
						AtomicReference<Expression> args = new AtomicReference<>();
						e.accept(segment -> {
							if (!(segment instanceof FunctionInvocation) && segment instanceof Expression expression) {
								args.compareAndSet(null, expression);
							}
						});
						return (FunctionInvocation) Cypher.call("toString").withArgs(args.get()).asFunction();
					}
					return e;
				}).build();

			var statement = CypherParser.parseStatement("RETURN asString(1) AS x", options);
			assertThat(functions.get()).isEqualTo(2);
			assertThat(statement.getCypher()).isEqualTo("RETURN toString(1) AS x");
		}

		@Test
		void onNewProcedureCallbacksShouldBeInvoked() {

			var procedures = new AtomicInteger(0);
			var options = Options.newOptions()
				.withCallback(InvocationCreatedEventType.ON_CALL, Clause.class, e -> {
					assertThat(procedures.compareAndSet(0, 1)).isTrue();
					return e;
				})
				.withCallback(InvocationCreatedEventType.ON_CALL, Clause.class, e -> {
					assertThat(procedures.compareAndSet(1, 2)).isTrue();
					return e;
				}).build();

			var statement = CypherParser.parseStatement("call { call dbms.showCurrentUser() yield username return username} return username", options);
			assertThat(procedures.get()).isEqualTo(2);
			assertThat(statement.getCypher()).isEqualTo("CALL {CALL dbms.showCurrentUser() YIELD username RETURN username} RETURN username");
		}

		@Test
		void preventingProcedureCalls() {
			var options = Options.newOptions()
				.withCallback(InvocationCreatedEventType.ON_CALL, Clause.class, e -> {
					throw new RuntimeException("Procedure calls are not allowed!");
				}).build();

			assertThatRuntimeException()
				.isThrownBy(() -> CypherParser.parseStatement("call { call dbms.showCurrentUser() yield username return username} return username", options))
				.withRootCauseInstanceOf(RuntimeException.class)
				.withStackTraceContaining("Procedure calls are not allowed!");

			assertThatNoException()
				.isThrownBy(() -> CypherParser.parseStatement("call {match (n:Movie) return n.title as title} return title", options));
		}

		@Test
		void preventingFunctionCalls() {
			var options = Options.newOptions()
				.withCallback(InvocationCreatedEventType.ON_INVOCATION, FunctionInvocation.class, functionInvocation -> {
					if("id".equals(functionInvocation.getFunctionName())) {
						throw new RuntimeException("id must not be used anymore");
					}
					return functionInvocation;
				}).build();

			for(String invalidQuery : List.of("MATCH (n) RETURN id(n)", "MATCH (n) WHERE id(n) = $id RETURN n, {__id: id(n), __labels: labels(n)} AS __node__")) {
				CypherParser.parseExpression(invalidQuery, options);
				assertThatRuntimeException()
					.isThrownBy(() -> CypherParser.parseStatement(invalidQuery, options))
					.withRootCauseInstanceOf(RuntimeException.class)
					.withStackTraceContaining("id must not be used anymore");
			}

			assertThatNoException()
				.isThrownBy(() -> CypherParser.parseStatement("MATCH (n) RETURN elementId(n)", options));
		}
	}

	@ParameterizedTest
	@CsvSource(delimiterString = "%%", textBlock = """
		MATCH (statement) WHERE EXISTS { MATCH (statement)-[:FOO WHERE true]->(m) WHERE false } RETURN statement %% MATCH (statement) WHERE EXISTS { MATCH (statement)-[:FOO WHERE true]->(m) WHERE false } RETURN statement
		MATCH (patternWithWhere) WHERE EXISTS { (patternWithWhere)-[:FOO WHERE true]->(m) WHERE false } RETURN statement %% MATCH (patternWithWhere) WHERE EXISTS { (patternWithWhere)-[:FOO WHERE true]->(m) WHERE false } RETURN statement
		MATCH (person:Person) WHERE COUNT { (person)-[:HAS_DOG]->(:Dog) } > 1 RETURN person.name AS name %% MATCH (person:Person) WHERE COUNT { (person)-[:HAS_DOG]->(:Dog) } > 1 RETURN person.name AS name
		MATCH (person:Person) WHERE COUNT { MATCH (person)-[:HAS_DOG]->(:Dog) } > 1 RETURN person.name AS name %% MATCH (person:Person) WHERE COUNT { MATCH (person)-[:HAS_DOG]->(:Dog) } > 1 RETURN person.name AS name
		MATCH (person:Person) WHERE COUNT { MATCH (person)-[d:HAS_DOG WHERE d.since > 10]->(:Dog) } > 1 RETURN person.name AS name %% MATCH (person:Person) WHERE COUNT { MATCH (person)-[d:HAS_DOG WHERE d.since > 10]->(:Dog) } > 1 RETURN person.name AS name
		MATCH (person:Person) WHERE COUNT { MATCH (person)-[d:HAS_DOG WHERE d.since > 10]->(:Dog) WHERE false} > 1 RETURN person.name AS name %% MATCH (person:Person) WHERE COUNT { MATCH (person)-[d:HAS_DOG WHERE d.since > 10]->(:Dog) WHERE false } > 1 RETURN person.name AS name
		""")
	void existAndCountSubqueriesWithPatternOrStatements(String input, String expected) {
		var renderer = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build());
		var cypher = renderer.render(CypherParser.parse(input));
		Assertions.assertThat(cypher).isEqualTo(expected.trim());
	}

	@Test // GH-903
	void variablesOfListPredicatesMustNotBeScoped() {

		var cypher = """
			MATCH (this:Movie)
			WHERE single(this0 IN [(this)-[this1:IN_GENRE]->(this0:Genre) WHERE this0.name = $param0 | 1] WHERE true)
			RETURN this { .actorCount } AS this""";

		var cfg = Configuration.newConfig()
			.withPrettyPrint(true)
			.withGeneratedNames(true)
			.build();
		var renderer = Renderer.getRenderer(cfg);
		var parseOptions = Options.newOptions().createSortedMaps(true).build();
		var normalized = renderer.render(CypherParser.parse(cypher, parseOptions));
		assertThat(normalized).isEqualTo("""
			MATCH (v0:Movie)
			WHERE single(v1 IN [(v0)-[v2:IN_GENRE]->(v3:Genre)
			WHERE v3.name = $p0 | 1]
			WHERE true)
			RETURN v0 {
			  .actorCount
			} AS v4""");
	}

	@Test
	void newConcatenate() {
		var cypher = "RETURN 'a' || 'B'";
		var cfg = Configuration.newConfig()
			.withPrettyPrint(true)
			.withGeneratedNames(true)
			.build();
		var renderer = Renderer.getRenderer(cfg);
		var normalized = renderer.render(CypherParser.parse(cypher));
		assertThat(normalized).isEqualTo("RETURN ('a' + 'B')");
	}


	@ParameterizedTest
	@CsvSource(delimiterString = "|", textBlock = """
		RETURN trim(' asd ')               | RETURN trim(' asd ')
		RETURN trim(BOTH ' ' FROM ' asd ') | RETURN trim(' asd ', ' ')
		RETURN trim(LEADING ' ' FROM ' asd ') | RETURN ltrim(' asd ', ' ')
		RETURN trim(TRAILING ' ' FROM ' asd ') | RETURN rtrim(' asd ', ' ')
		RETURN btrim(' asd ') | RETURN btrim(' asd ')
		RETURN btrim(' asd ', ' ') | RETURN btrim(' asd ', ' ')
		RETURN ltrim(' asd ') | RETURN ltrim(' asd ')
		RETURN ltrim(' asd ', ' ') | RETURN ltrim(' asd ', ' ')
		RETURN rtrim(' asd ') | RETURN rtrim(' asd ')
		RETURN rtrim(' asd ', ' ') | RETURN rtrim(' asd ', ' ')
		""")
	void trimShouldWork(String in, String out) {

		var renderer = Renderer.getRenderer(Configuration.defaultConfig());
		var normalized = renderer.render(CypherParser.parse(in));
		assertThat(normalized).isEqualTo(out);
	}
}
