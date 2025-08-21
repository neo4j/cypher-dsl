/*
 * Copyright (c) 2019-2025 "Neo4j,"
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

import java.net.URI;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class SubqueriesIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Nested
	class Scope {
		@Test
		void nodePatternInCallMustBeFullAndNotKnown() {
			Statement parsed = Cypher.match(Cypher.node("Person").named("n"))
				.call(Cypher.match(Cypher.node("Movie").named("n").withProperties("title", Cypher.literalOf("The Matrix"))).where(Cypher.anyNode("n").property("released").gte(Cypher.literalOf(1980))).returning(Cypher.anyNode("n").as("m")).build())
				.returning(Cypher.anyNode("n").property("name"))
				.build();
			String cypher = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build()).render(parsed);
			assertThat(cypher).isEqualTo("MATCH (n:Person) CALL {MATCH (n:Movie {title: 'The Matrix'}) WHERE n.released >= 1980 RETURN n AS m} RETURN n.name");
		}
	}

	@Nested
	class DialectSupport {

		@ParameterizedTest
		@CsvSource(delimiterString = "|", textBlock = """
			NEO4J_5|UNWIND [0, 1, 2] AS x CALL {RETURN 'hello' AS innerReturn} RETURN innerReturn
			NEO4J_5_23|UNWIND [0, 1, 2] AS x CALL (*) {RETURN 'hello' AS innerReturn} RETURN innerReturn
			""")
		void starterExample(Dialect dialect, String expected) {
			var stmt = Cypher.unwind(Cypher.listOf(Cypher.literalOf(0), Cypher.literalOf(1), Cypher.literalOf(2)))
				.as("x")
				.call(Cypher.returning(Cypher.literalOf("hello").as("innerReturn")).build())
				.returning("innerReturn")
				.build();

			var renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
			assertThat(renderer.render(stmt)).isEqualTo(expected);
		}

		@ParameterizedTest
		@CsvSource(delimiterString = "|", textBlock = """
			NEO4J_5|MATCH (p:Player), (t:Team) CALL {WITH p WITH p, rand() AS random SET p.rating = random RETURN p.name AS playerName, p.rating AS rating} RETURN playerName, rating, t AS team ORDER BY rating LIMIT 1
			NEO4J_5_23|MATCH (p:Player), (t:Team) CALL (p) {WITH p, rand() AS random SET p.rating = random RETURN p.name AS playerName, p.rating AS rating} RETURN playerName, rating, t AS team ORDER BY rating LIMIT 1
			""")
		void someImports(Dialect dialect, String expected) {

			var p = Cypher.node("Player").named("p");
			var t = Cypher.node("Team").named("t");

			var rating = Cypher.name("rating");
			var playerName = Cypher.name("playerName");
			var stmt = Cypher.match(p, t)
				.call(
					Cypher.with(p, Cypher.rand().as("random"))
						.set(p.property("rating").to(Cypher.name("random")))
						.returning(p.property("name").as(playerName), p.property("rating").as(rating))
						.build(), p
				).returning(playerName, rating, t.as("team"))
				.orderBy(rating)
				.limit(1).build();

			var renderer = Renderer.getRenderer(Configuration.newConfig()
				.alwaysEscapeNames(false)
				.withDialect(dialect).build());
			assertThat(renderer.render(stmt)).isEqualTo(expected);
		}

		@ParameterizedTest
		@CsvSource(delimiterString = "|", textBlock = """
			NEO4J_5|MATCH (p:Player), (t:Team) CALL {WITH * RETURN p AS player, t AS team} RETURN player, team
			NEO4J_5_23|MATCH (p:Player), (t:Team) CALL (*) {RETURN p AS player, t AS team} RETURN player, team
			""")
		void allImports(Dialect dialect, String expected) {

			var p = Cypher.node("Player").named("p");
			var t = Cypher.node("Team").named("t");

			var player = Cypher.name("player");
			var stmt = Cypher.match(p, t)
				.call(
					Cypher.returning(p.as("player"), t.as("team")).build(), Cypher.asterisk()
				).returning(player, Cypher.name("team"))
				.build();

			var renderer = Renderer.getRenderer(Configuration.newConfig()
				.alwaysEscapeNames(false)
				.withDialect(dialect).build());
			assertThat(renderer.render(stmt)).isEqualTo(expected);
		}

		@ParameterizedTest
		@CsvSource(delimiterString = "|", textBlock = """
			NEO4J_5|MATCH (t:Team) CALL {MATCH (p:Player) RETURN count(p) AS totalPlayers} RETURN count(t) AS totalTeams, totalPlayers
			NEO4J_5_23|MATCH (t:Team) CALL (*) {MATCH (p:Player) RETURN count(p) AS totalPlayers} RETURN count(t) AS totalTeams, totalPlayers
			""")
		void noImports(Dialect dialect, String expected) {

			var p = Cypher.node("Player").named("p");
			var t = Cypher.node("Team").named("t");

			var stmt = Cypher.match(t)
				.call(
					Cypher.match(p).returning(Cypher.count(p).as("totalPlayers")).build()
				).returning(Cypher.count(t).as("totalTeams"), Cypher.name("totalPlayers"))
				.build();

			var renderer = Renderer.getRenderer(Configuration.newConfig()
				.alwaysEscapeNames(false)
				.withDialect(dialect).build());
			assertThat(renderer.render(stmt)).isEqualTo(expected);
		}
	}

	@Nested
	class ResultReturningSubqueries {

		@Test
		void importingVariablesShouldWork() {

			Statement statement = Cypher.unwind(Cypher.literalOf(0), Cypher.literalOf(1), Cypher.literalOf(2)).as("x")
				.call(Cypher.with(Cypher.name("x")).returning(Cypher.name("x").multiply(Cypher.literalOf(10)).as("y"))
					.build())
				.returning("x", "y").build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo("UNWIND [0, 1, 2] AS x CALL {WITH x RETURN (x * 10) AS y} RETURN x, y");

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("x"),
				SymbolicName.of("y"));
		}

		@Test
		void postUnionProcessingShouldWork() {

			Property ageProperty = Cypher.property("p", "age");
			Property nameProperty = Cypher.property("p", "name");

			// This must be 2 different person nodes and statement, otherwise the union will reuse it.
			Statement s1 = Cypher.match(Cypher.node("Person").named("p"))
				.returning("p").orderBy(ageProperty.ascending()).limit(1).build();
			Statement s2 = Cypher.match(Cypher.node("Person").named("p"))
				.returning("p").orderBy(ageProperty.descending()).limit(1).build();

			Statement statement = Cypher.call(Cypher.union(s1, s2))
				.returning(nameProperty, ageProperty).orderBy(nameProperty)
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"CALL {MATCH (p:`Person`) RETURN p ORDER BY p.age ASC LIMIT 1 UNION MATCH (p:`Person`) RETURN p ORDER BY p.age DESC LIMIT 1} RETURN p.name, p.age ORDER BY p.name");
		}

		@Test
		void aggregationAndSideEffectsShouldWork() {

			Node person = Cypher.node("Person").named("p");
			Node clone = Cypher.node("Clone").named("c");
			Statement statement = Cypher.match(person)
				.call(Cypher.unwind(Cypher.range(1, 5)).as("i")
					.create(clone)
					.returning(Cypher.count(clone).as("numberOfClones")).build())
				.returning(person.property("name"), Cypher.name("numberOfClones")).build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (p:`Person`) CALL {UNWIND range(1, 5) AS i CREATE (c:`Clone`) RETURN count(c) AS numberOfClones} RETURN p.name, numberOfClones");
		}

		@Test
		void aggregationOnImportedVariablesShouldWork() {

			Node person = Cypher.node("Person").named("p");
			Node other = Cypher.node("Person").named("other");
			Statement statement = Cypher.match(person)
				.call(Cypher.with(person)
					.match(other).where(other.property("age").lt(person.property("age")))
					.returning(Cypher.count(other).as("youngerPersonsCount")).build())
				.returning(person.property("name"), Cypher.name("youngerPersonsCount")).build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"MATCH (p:`Person`) CALL {WITH p MATCH (other:`Person`) WHERE other.age < p.age RETURN count(other) AS youngerPersonsCount} RETURN p.name, youngerPersonsCount");
		}

		@Test
		void nestedAfterProcedureCall() {

			// With with
			Statement statement = Cypher.call("dbms.components").yield("name").with("name")
				.call(Cypher.with("name").match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(Cypher.name("name"))).returning("n").build())
				.returning("n")
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"CALL dbms.components() YIELD name WITH name CALL {WITH name MATCH (n) WHERE n.name = name RETURN n} RETURN n");

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

			// Without with
			statement = Cypher.call("dbms.components").yield("name")
				.call(Cypher.with("name").match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(Cypher.name("name"))).returning("n").build())
				.returning("n")
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"CALL dbms.components() YIELD name CALL {WITH name MATCH (n) WHERE n.name = name RETURN n} RETURN n");

			// After inQueryCall with with
			SymbolicName label = Cypher.name("label");
			statement = Cypher
				.match(Cypher.anyNode().named("n")).with("n")
				.call("db.labels")
				.yield(label)
				.with(label)
				.call(Cypher.with(label).match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(label)).returning("n").build())
				.returning("n")
				.build();
			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (n) WITH n CALL db.labels() YIELD label WITH label CALL {WITH label MATCH (n) WHERE n.name = label RETURN n} RETURN n");

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

			// After inQueryCall without with
			statement = Cypher
				.match(Cypher.anyNode().named("n")).with("n")
				.call("db.labels")
				.yield(label)
				.call(Cypher.with(label).match(Cypher.anyNode().named("n2"))
					.where(Cypher.property("n2", "name").isEqualTo(label)).returning("n2").build())
				.returning("n2")
				.build();
			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (n) WITH n CALL db.labels() YIELD label CALL {WITH label MATCH (n2) WHERE n2.name = label RETURN n2} RETURN n2");
		}

		@Test
		void afterRegularWith() {

			Statement statement = Cypher.match(Cypher.node("Person").named("p")).with("p")
				.call(Cypher.with("p").match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(Cypher.property("p", "name"))).returning("n").build())
				.returning("n")
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (p:`Person`) WITH p CALL {WITH p MATCH (n) WHERE n.name = p.name RETURN n} RETURN n");

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));
		}

		@Test
		void afterRegularWithManualImport() {

			Statement statement = Cypher.match(Cypher.node("Person").named("p")).with("p")
				.call(Cypher.match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(Cypher.property("p", "name"))).returning("n").build(), "p")
				.returning("n")
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (p:`Person`) WITH p CALL {WITH p MATCH (n) WHERE n.name = p.name RETURN n} RETURN n");

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));
		}

		@Test
		void callsCallingCalls() {

			Statement someStatement = Cypher.match(Cypher.anyNode().named("n")).returning("n").build();
			Statement statement = someStatement;

			for (int i = 0; i < 5; ++i) {
				statement = Cypher.call(statement).returning("n").build();
			}

			assertThat(cypherRenderer.render(statement)).isEqualTo(
				"CALL {CALL {CALL {CALL {CALL {MATCH (n) RETURN n} RETURN n} RETURN n} RETURN n} RETURN n} RETURN n");
		}
	}

	@Nested
	class ExistentialSubqueries {

		@Test // GH-578
		void fullStatementsAsExistentialSubQuery() {

			Node p = Cypher.node("Person").named("person");
			Statement inner = Cypher.match(p.relationshipTo(Cypher.node("Dog"), "HAS_DOG")).returning(p.property("name")).build();
			Statement outer = Cypher.match(p).where(Cypher.exists(inner)).returning(p.property("name").as("name")).build();

			String cypher = outer.getCypher();
			assertThat(cypher).isEqualTo("MATCH (person:`Person`) WHERE EXISTS { MATCH (person)-[:`HAS_DOG`]->(:`Dog`) RETURN person.name } RETURN person.name AS name");
		}

		@Test // GH-578
		void fullStatementsAsExistentialSubQueryWithImports() {

			Node p = Cypher.node("Person").named("person");
			Node d = Cypher.node("Dog").named("d");
			SymbolicName dogName = Cypher.name("dogName");
			Statement inner = Cypher.match(p.relationshipTo(d, "HAS_DOG")).where(d.property("name").eq(dogName)).returning(p.property("name")).build();
			Statement outer = Cypher.match(p).where(Cypher.exists(inner, Cypher.literalOf("Ozzy").as(dogName))).returning(p.property("name").as("name")).build();

			String cypher = outer.getCypher();
			assertThat(cypher).isEqualTo("MATCH (person:`Person`) WHERE EXISTS { WITH 'Ozzy' AS dogName MATCH (person)-[:`HAS_DOG`]->(d:`Dog`) WHERE d.name = dogName RETURN person.name } RETURN person.name AS name");
		}

		@Test
		void simple() {

			Node p = Cypher.node("Person").named("p");
			Node friend = Cypher.node("Person").named("friend");

			Relationship r = p.relationshipTo(friend, "IS_FRIENDS_WITH").named("r");
			Statement statement = Cypher.match(r)
				.where(Cypher.match(
					p.relationshipTo(Cypher.node("Company").withProperties("name", Cypher.literalOf("Neo4j")),
						"WORKS_FOR")).asCondition())
				.returning(p, r, friend)
				.build();

			assertThat(cypherRenderer.render(statement)).isEqualTo(
				"MATCH (p:`Person`)-[r:`IS_FRIENDS_WITH`]->(friend:`Person`) WHERE EXISTS { MATCH (p)-[:`WORKS_FOR`]->(:`Company` {name: 'Neo4j'}) } RETURN p, r, friend");
		}

		@Test
		void withBooleanOpsAndWhere() {

			Node p = Cypher.node("Person").named("person");
			Node company = Cypher.anyNode().named("company");
			Node t = Cypher.node("Technology").named("t");

			Statement statement = Cypher.match(p.relationshipTo(company, "WORKS_FOR"))
				.where(company.property("name").startsWith(Cypher.literalOf("Company")))
				.and(Cypher.match(p.relationshipTo(t, "LIKES"))
					.where(Cypher.size(t.relationshipFrom(Cypher.anyNode(), "LIKES")).gte(Cypher.literalOf(3)))
					.asCondition())
				.returning(p.property("name").as("person"), company.property("name").as("company"))
				.build();

			assertThat(cypherRenderer.render(statement)).isEqualTo(
				"MATCH (person:`Person`)-[:`WORKS_FOR`]->(company) WHERE (company.name STARTS WITH 'Company' AND EXISTS { MATCH (person)-[:`LIKES`]->(t:`Technology`) WHERE size((t)<-[:`LIKES`]-()) >= 3 }) RETURN person.name AS person, company.name AS company");

			statement = Cypher.match(p.relationshipTo(company, "WORKS_FOR"))
				.where(Cypher.match(p.relationshipTo(t, "LIKES"))
					.where(Cypher.size(t.relationshipFrom(Cypher.anyNode(), "LIKES")).gte(Cypher.literalOf(3)))
					.asCondition())
				.and(company.property("name").startsWith(Cypher.literalOf("Company")))
				.returning(p.property("name").as("person"), company.property("name").as("company"))
				.build();

			assertThat(cypherRenderer.render(statement)).isEqualTo(
				"MATCH (person:`Person`)-[:`WORKS_FOR`]->(company) WHERE (EXISTS { MATCH (person)-[:`LIKES`]->(t:`Technology`) WHERE size((t)<-[:`LIKES`]-()) >= 3 } AND company.name STARTS WITH 'Company') RETURN person.name AS person, company.name AS company");

			statement = Cypher.match(p.relationshipTo(company, "WORKS_FOR"))
				.where(
					Cypher.match(p.relationshipTo(t, "LIKES"))
						.where(Cypher.size(t.relationshipFrom(Cypher.anyNode(), "LIKES")).gte(Cypher.literalOf(3)))
						.asCondition().and(company.property("name").startsWith(Cypher.literalOf("Company")))
				)
				.returning(p.property("name").as("person"), company.property("name").as("company"))
				.build();

			assertThat(cypherRenderer.render(statement)).isEqualTo(
				"MATCH (person:`Person`)-[:`WORKS_FOR`]->(company) WHERE (EXISTS { MATCH (person)-[:`LIKES`]->(t:`Technology`) WHERE size((t)<-[:`LIKES`]-()) >= 3 } AND company.name STARTS WITH 'Company') RETURN person.name AS person, company.name AS company");
		}
	}

	@Nested
	class InTransactions {

		@Test
		void docs44_7() {

			SymbolicName line = Cypher.name("line");
			Statement statement = Cypher.loadCSV(URI.create("file:///friends.csv")).as("line")
				.callInTransactions(Cypher.with("line").create(Cypher.node("Person")
					.withProperties(
						"name", Cypher.valueAt(line, 1),
						"age", Cypher.toInteger(Cypher.valueAt(line, 2))
					)).build()
				)
				.build();
			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"LOAD CSV FROM 'file:///friends.csv' AS line CALL {WITH line CREATE (:`Person` {name: line[1], age: toInteger(line[2])})} IN TRANSACTIONS");
		}

		@Test
		void docs44_7_1a() {

			SymbolicName line = Cypher.name("line");
			Statement statement = Cypher.loadCSV(URI.create("file:///friends.csv")).as("line")
				.callInTransactions(Cypher.with("line").create(Cypher.node("Person")
					.withProperties(
						"name", Cypher.valueAt(line, 1),
						"age", Cypher.toInteger(Cypher.valueAt(line, 2))
					)).build(), 2
				)
				.build();
			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"LOAD CSV FROM 'file:///friends.csv' AS line CALL {WITH line CREATE (:`Person` {name: line[1], age: toInteger(line[2])})} IN TRANSACTIONS OF 2 ROWS");
		}

		@Test
		void docs44_7_1b() {

			Statement statement = Cypher.match(Cypher.anyNode("n"))
				.callInTransactions(Cypher.with("n").detachDelete("n").build(), 2)
				.build();
			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (n) CALL {WITH n DETACH DELETE n} IN TRANSACTIONS OF 2 ROWS");
		}

		@ParameterizedTest
		@ValueSource(ints = { -1, 23 })
		void afterRegularWith(int numRows) {

			ResultStatement subquery = Cypher.create(
					Cypher.anyNode("p").relationshipTo(Cypher.node("User").named("u"), "IS"))
				.returning(Cypher.name("u"))
				.build();

			StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere with = Cypher.match(
				Cypher.node("Person").named("p")).with("p");
			Statement statement = (numRows < 0 ?
				with.callInTransactions(subquery, "p") :
				with.callInTransactions(subquery, numRows, "p"))
				.returning("u")
				.build();

			String expected = "MATCH (p:`Person`) WITH p CALL {WITH p CREATE (p)-[:`IS`]->(u:`User`) RETURN u} IN TRANSACTIONS";
			if (numRows > 0) {
				expected += " OF " + numRows + " ROWS";
			}
			expected += " RETURN u";
			assertThat(cypherRenderer.render(statement)).isEqualTo(expected);

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("u"));
		}

		@ParameterizedTest
		@ValueSource(ints = { -1, 23 })
		void afterRegularWithSymNameImport(int numRows) {

			SymbolicName p = Cypher.name("p");
			ResultStatement subquery = Cypher.create(
					Cypher.anyNode(p).relationshipTo(Cypher.node("User").named("u"), "IS"))
				.returning(Cypher.name("u"))
				.build();

			StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere with = Cypher.match(
				Cypher.node("Person").named(p)).with(p);
			Statement statement = (numRows < 0 ?
				with.callInTransactions(subquery, p) :
				with.callInTransactions(subquery, numRows, p))
				.returning("u")
				.build();

			String expected = "MATCH (p:`Person`) WITH p CALL {WITH p CREATE (p)-[:`IS`]->(u:`User`) RETURN u} IN TRANSACTIONS";
			if (numRows > 0) {
				expected += " OF " + numRows + " ROWS";
			}
			expected += " RETURN u";
			assertThat(cypherRenderer.render(statement)).isEqualTo(expected);

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("u"));
		}

		@Test
		void nestedAfterProcedureCall() {

			// With with
			Statement statement = Cypher.call("dbms.components").yield("name").with("name")
				.callInTransactions(Cypher.with("name").match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(Cypher.name("name"))).returning("n").build())
				.returning("n")
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"CALL dbms.components() YIELD name WITH name CALL {WITH name MATCH (n) WHERE n.name = name RETURN n} IN TRANSACTIONS RETURN n");

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

			// Without with
			statement = Cypher.call("dbms.components").yield("name")
				.callInTransactions(Cypher.with("name").match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(Cypher.name("name"))).returning("n").build())
				.returning("n")
				.build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo(
					"CALL dbms.components() YIELD name CALL {WITH name MATCH (n) WHERE n.name = name RETURN n} IN TRANSACTIONS RETURN n");

			// After inQueryCall with with
			SymbolicName label = Cypher.name("label");
			statement = Cypher
				.match(Cypher.anyNode().named("n")).with("n")
				.call("db.labels")
				.yield(label)
				.with(label)
				.callInTransactions(Cypher.with(label).match(Cypher.anyNode().named("n"))
					.where(Cypher.property("n", "name").isEqualTo(label)).returning("n").build())
				.returning("n")
				.build();
			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (n) WITH n CALL db.labels() YIELD label WITH label CALL {WITH label MATCH (n) WHERE n.name = label RETURN n} IN TRANSACTIONS RETURN n");

			assertThat(statement.getCatalog().getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

			// After inQueryCall without with
			statement = Cypher
				.match(Cypher.anyNode().named("n")).with("n")
				.call("db.labels")
				.yield(label)
				.callInTransactions(Cypher.with(label).match(Cypher.anyNode().named("n2"))
					.where(Cypher.property("n2", "name").isEqualTo(label)).returning("n2").build())
				.returning("n2")
				.build();
			assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (n) WITH n CALL db.labels() YIELD label CALL {WITH label MATCH (n2) WHERE n2.name = label RETURN n2} IN TRANSACTIONS RETURN n2");
		}
	}

	@Nested
	class CollectSubqueries {

		private final Node person = Cypher.node("Person").named("person");
		private final Property personName = person.property("name");
		private final Node dog = Cypher.node("Dog").named("dog");
		private final Property dogName = dog.property("name");
		private final Relationship hasDog = person.relationshipTo(dog, "HAS_DOG");
		private final Node cat = Cypher.node("Cat").named("cat");

		@Test
		void shouldCheckForReturnStatement() {

			var nonReturnStatement = Cypher.create(Cypher.node("Foo")).build();
			assertThatIllegalArgumentException().isThrownBy(() -> Cypher.collect(nonReturnStatement))
				.withMessage(
					"The final RETURN clause in a subquery used with COLLECT is mandatory and the RETURN clause must return exactly one column.");
		}

		@Test
		void simple() {

			var inner = Cypher.match(person.relationshipTo(dog, "HAS_DOG")).returning(dogName)
				.build();
			var stmt = Cypher.match(person)
				.where(Cypher.literalOf("Ozzy").in(Cypher.collect(inner)))
				.returning(person.property("name").as("name"))
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) WHERE 'Ozzy' IN COLLECT { MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`) RETURN dog.name } RETURN person.name AS name");
		}

		@Test
		void withWhereClause() {

			var r = this.hasDog.named("r");
			var inner = Cypher.match(r)
				.where(r.property("since").gt(Cypher.literalOf(2017)))
				.returning(dogName)
				.build();
			var stmt = Cypher.match(person)
				.returning(person.property("name").as("name"), Cypher.collect(inner).as("youngDogs"))
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) "
					+ "RETURN person.name AS name, COLLECT {"
					+ " MATCH (person)-[r:`HAS_DOG`]->(dog:`Dog`)"
					+ " WHERE r.since > 2017"
					+ " RETURN dog.name "
					+ "} AS youngDogs");
		}

		@Test
		void withAUnion() {

			var hasCat = person.relationshipTo(cat, "HAS_CAT");
			var inner =
				Cypher.union(
					Cypher.match(hasDog).returning(dogName.as("petName")).build(),
					Cypher.match(hasCat).returning(cat.property("name").as("petName")).build()
				);

			var stmt = Cypher.match(person)
				.returning(person.property("name").as("name"), Cypher.collect(inner).as("petNames"))
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) "
					+ "RETURN person.name AS name, "
					+ "COLLECT {"
					+ " MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`)"
					+ " RETURN dog.name AS petName"
					+ " UNION"
					+ " MATCH (person)-[:`HAS_CAT`]->(cat:`Cat`)"
					+ " RETURN cat.name AS petName "
					+ "} AS petNames");
		}

		@Test
		void withWith() {

			var r = this.hasDog.named("r");
			var yearOfTheDog = Cypher.literalOf(2018).as("yearOfTheDog");
			var inner = Cypher.match(r)
				.where(r.property("since").eq(yearOfTheDog))
				.returning(dogName)
				.build();
			var stmt = Cypher.match(person)
				.returning(person.property("name").as("name"),
					Cypher.subqueryWith(yearOfTheDog).collect(inner).as("dogsOfTheYear")
				)
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) "
					+ "RETURN person.name AS name, COLLECT {"
					+ " WITH 2018 AS yearOfTheDog"
					+ " MATCH (person)-[r:`HAS_DOG`]->(dog:`Dog`)"
					+ " WHERE r.since = yearOfTheDog"
					+ " RETURN dog.name "
					+ "} AS dogsOfTheYear");
		}

		@Test
		void inReturn() {

			var toy = Cypher.node("Toy").named("t");
			var inner = Cypher.match(hasDog)
				.match(dog.relationshipTo(toy, "HAS_TOY"))
				.returning(toy.property("name"))
				.build();
			var stmt = Cypher.match(person)
				.returning(person.property("name"),
					Cypher.collect(inner).as("toyNames")
				)
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) "
					+ "RETURN person.name, "
					+ "COLLECT {"
					+ " MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`)"
					+ " MATCH (dog)-[:`HAS_TOY`]->(t:`Toy`)"
					+ " RETURN t.name "
					+ "} AS toyNames");
		}

		@Test
		void collectInSet() {

			var inner = Cypher.match(hasDog)
				.returning(dogName)
				.build();
			var stmt = Cypher.match(person)
				.where(person.property("name").eq(Cypher.literalOf("Peter")))
				.set(person.property("dogNames").to(Cypher.collect(inner)))
				.returning(person.property("dogNames").as("dogNames"))
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) WHERE person.name = 'Peter' "
					+ "SET person.dogNames = COLLECT { MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`) RETURN dog.name } "
					+ "RETURN person.dogNames AS dogNames");
		}

		@Test
		void inCase() {

			var inner = Cypher.match(hasDog)
				.returning(dogName)
				.build();
			var stmt = Cypher.match(person)
				.returning(
					Cypher.caseExpression().when(Cypher.collect(inner).eq(Cypher.listOf()))
						.then(Cypher.literalOf("No Dogs ").concat(personName))
						.elseDefault(personName).as("result")
				)
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) "
					+ "RETURN "
					+ "CASE "
					+ "WHEN COLLECT { MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`) RETURN dog.name } = [] THEN ('No Dogs ' + person.name) "
					+ "ELSE person.name "
					+ "END AS result");
		}

		@Test
		void asGroupingKey() {

			var inner = Cypher.match(hasDog)
				.returning(dogName)
				.build();
			var stmt = Cypher.match(person)
				.returning(
					Cypher.collect(inner).as("dogNames"),
					Cypher.avg(person.property("age")).as("averageAge")
				).orderBy(Cypher.name("dogNames"))
				.build();
			assertThat(stmt.getCypher())
				.isEqualTo(
					"MATCH (person:`Person`) "
					+ "RETURN COLLECT { MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`) RETURN dog.name } AS dogNames,"
					+ " avg(person.age) AS averageAge "
					+ "ORDER BY dogNames");
		}
	}

	@Test // GH-533
	void unitSubqueries() {

		var p = Cypher.node("Person").named("p");
		var outer = Cypher.match(p);
		var inner = Cypher.with(p)
				.unwind(Cypher.range(1, 5)).as("i")
				.create(Cypher.node("Person").withProperties("name", p.property("name"))).build();

		var statement = outer.call(inner)
			.returning(Cypher.count(Cypher.asterisk()))
			.build();
		assertThat(statement.getCypher())
			.isEqualTo("MATCH (p:`Person`) CALL {WITH p UNWIND range(1, 5) AS i CREATE (:`Person` {name: p.name})} RETURN count(*)");
	}
}
