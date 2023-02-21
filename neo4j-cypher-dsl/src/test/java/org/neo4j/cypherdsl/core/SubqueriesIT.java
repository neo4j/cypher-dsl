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

import java.net.URI;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
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
	class ResultReturningSubqueries {

		@Test
		void importingVariablesShouldWork() {

			Statement statement = Cypher.unwind(Cypher.literalOf(0), Cypher.literalOf(1), Cypher.literalOf(2)).as("x")
				.call(Cypher.with(Cypher.name("x")).returning(Cypher.name("x").multiply(Cypher.literalOf(10)).as("y"))
					.build())
				.returning("x", "y").build();

			assertThat(cypherRenderer.render(statement))
				.isEqualTo("UNWIND [0, 1, 2] AS x CALL {WITH x RETURN (x * 10) AS y} RETURN x, y");

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("x"),
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
				.call(Cypher.unwind(Functions.range(1, 5)).as("i")
					.create(clone)
					.returning(Functions.count(clone).as("numberOfClones")).build())
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
					.returning(Functions.count(other).as("youngerPersonsCount")).build())
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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));
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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));
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
					.where(Functions.size(t.relationshipFrom(Cypher.anyNode(), "LIKES")).gte(Cypher.literalOf(3)))
					.asCondition())
				.returning(p.property("name").as("person"), company.property("name").as("company"))
				.build();

			assertThat(cypherRenderer.render(statement)).isEqualTo(
				"MATCH (person:`Person`)-[:`WORKS_FOR`]->(company) WHERE (company.name STARTS WITH 'Company' AND EXISTS { MATCH (person)-[:`LIKES`]->(t:`Technology`) WHERE size((t)<-[:`LIKES`]-()) >= 3 }) RETURN person.name AS person, company.name AS company");

			statement = Cypher.match(p.relationshipTo(company, "WORKS_FOR"))
				.where(Cypher.match(p.relationshipTo(t, "LIKES"))
					.where(Functions.size(t.relationshipFrom(Cypher.anyNode(), "LIKES")).gte(Cypher.literalOf(3)))
					.asCondition())
				.and(company.property("name").startsWith(Cypher.literalOf("Company")))
				.returning(p.property("name").as("person"), company.property("name").as("company"))
				.build();

			assertThat(cypherRenderer.render(statement)).isEqualTo(
				"MATCH (person:`Person`)-[:`WORKS_FOR`]->(company) WHERE (EXISTS { MATCH (person)-[:`LIKES`]->(t:`Technology`) WHERE size((t)<-[:`LIKES`]-()) >= 3 } AND company.name STARTS WITH 'Company') RETURN person.name AS person, company.name AS company");

			statement = Cypher.match(p.relationshipTo(company, "WORKS_FOR"))
				.where(
					Cypher.match(p.relationshipTo(t, "LIKES"))
						.where(Functions.size(t.relationshipFrom(Cypher.anyNode(), "LIKES")).gte(Cypher.literalOf(3)))
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
						"age", Functions.toInteger(Cypher.valueAt(line, 2))
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
						"age", Functions.toInteger(Cypher.valueAt(line, 2))
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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("u"));
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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("u"));
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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

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

			assertThat(statement.getIdentifiableExpressions()).containsExactlyInAnyOrder(SymbolicName.of("n"));

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
}
