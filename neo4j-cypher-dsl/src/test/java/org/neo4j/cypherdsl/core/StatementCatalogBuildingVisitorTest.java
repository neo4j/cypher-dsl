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

import java.net.URI;
import java.util.Set;

import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class StatementCatalogBuildingVisitorTest {

	@Test
	void simpleShowCase() {

		// tag::catalog-example[]
		var p = Cypher.node("Person").named("p");
		var m = Cypher.node("Movie").named("m");
		var a = m.withProperties("title", Cypher.literalOf("The Matrix"))
			.relationshipFrom(p, "ACTED_IN").named("a");
		var statement = Cypher
			.match(a)
			.where(p.property("born").gte(Cypher.parameter("born", 1979)))
			.returning(p)
			.build();

		var catalog = statement.getCatalog();

		assertThat(catalog.getNodeLabels())
			.extracting(StatementCatalog.Token::value)
			.containsExactlyInAnyOrder("Person", "Movie");
		assertThat(catalog.getProperties())
			.containsExactlyInAnyOrder(
				StatementCatalog.property(Set.of(StatementCatalog.label("Movie")), "title"),
				StatementCatalog.property(Set.of(StatementCatalog.label("Person")), "born")
			);
		// end::catalog-example[]

		var cypher = statement.getCypher();
		assertThat(cypher).isEqualTo("MATCH (m:`Movie` {title: 'The Matrix'})<-[a:`ACTED_IN`]-(p:`Person`) WHERE p.born >= $born RETURN p");
	}

	@Test
	void labelExpressionsShouldWork() {

		var p = Cypher.node(new LabelExpression("Person").or(new LabelExpression("Actor")));
		var statement = Cypher
			.match(p)
			.where(p.property("born").gte(Cypher.parameter("born", 1979)))
			.returning(p)
			.build();


		var catalog = statement.getCatalog();

		assertThat(catalog.getNodeLabels())
			.extracting(StatementCatalog.Token::value)
			.containsExactlyInAnyOrder("Actor", "Person");
		assertThat(catalog.getProperties())
			.containsExactlyInAnyOrder(
				StatementCatalog.property(Set.of(StatementCatalog.label("Actor"), StatementCatalog.label("Person")), "born")
			);
	}

	@Test
	void labelFiltersShouldWork() {
		var n = Cypher.node("Person").withProperties("name", Cypher.literalOf("John Doe")).named("n");
		var m = Cypher.node("Person").named("m");

		var statement = Cypher.match(n.relationshipTo(m, "IS_FRIEND_WITH"))
			.where(n.hasLabels("Active")).and(m.hasLabels("Kind", "Positive").and(m.property("name").eq(Cypher.literalOf("Jane"))))
			.returning(Cypher.asterisk())
			.build();
		var cypher = statement.getCypher();
		assertThat(cypher).isEqualTo("MATCH (n:`Person` {name: 'John Doe'})-[:`IS_FRIEND_WITH`]->(m:`Person`) WHERE (n:`Active` AND m:`Kind`:`Positive` AND m.name = 'Jane') RETURN *");

		var catalog = statement.getCatalog();
		var expectedLabelFilters = Set.of(
			new StatementCatalog.LabelFilter("n", Set.of(StatementCatalog.label("Active"))),
			new StatementCatalog.LabelFilter("m", Set.of(StatementCatalog.label("Kind"), StatementCatalog.label("Positive")))
		);
		assertThat(catalog.getAllFilters())
			.hasSize(4)
			.filteredOn(StatementCatalog.LabelFilter.class::isInstance)
			.map(StatementCatalog.LabelFilter.class::cast)
			.containsExactlyInAnyOrderElementsOf(expectedLabelFilters);
		assertThat(catalog.getAllLabelFilters()).containsExactlyInAnyOrderElementsOf(expectedLabelFilters);
	}

	@Test // GH-674
	void shouldThrowWhenAskingWithTypeForTypes() {

		var n = Cypher.node("Person").withProperties("name", Cypher.literalOf("John Doe")).named("n");
		var m = Cypher.node("Person").named("m");

		var catalog = Cypher.match(n.relationshipTo(m, "IS_FRIEND_WITH"))
			.where(n.hasLabels("Active")).and(m.hasLabels("Kind", "Positive").and(m.property("name").eq(Cypher.literalOf("Jane"))))
			.returning(Cypher.asterisk())
			.build().getCatalog();

		var aType = StatementCatalog.Token.type("whatever");
		var expectedMessage = "Token[type=RELATIONSHIP_TYPE, value=whatever] must be a node label, not a relationship type";
		assertThatIllegalArgumentException()
			.isThrownBy(() -> catalog.getIncomingRelations(aType))
			.withMessage(expectedMessage);
		assertThatIllegalArgumentException()
			.isThrownBy(() -> catalog.getOutgoingRelations(aType))
			.withMessage(expectedMessage);
		assertThatIllegalArgumentException()
			.isThrownBy(() -> catalog.getUndirectedRelations(aType))
			.withMessage(expectedMessage);
	}

	@Test // GH-674
	void shouldThrowWhenAskingWithLabelForLabels() {

		var n = Cypher.node("Person").withProperties("name", Cypher.literalOf("John Doe")).named("n");
		var m = Cypher.node("Person").named("m");

		var catalog = Cypher.match(n.relationshipTo(m, "IS_FRIEND_WITH"))
			.where(n.hasLabels("Active")).and(m.hasLabels("Kind", "Positive").and(m.property("name").eq(Cypher.literalOf("Jane"))))
			.returning(Cypher.asterisk())
			.build().getCatalog();

		var aLabel = StatementCatalog.Token.label("whatever");
		var expectedMessage = "Token[type=NODE_LABEL, value=whatever] must be a relationship type, not a node label";
		assertThatIllegalArgumentException()
			.isThrownBy(() -> catalog.getTargetNodes(aLabel))
			.withMessage(expectedMessage);
		assertThatIllegalArgumentException()
			.isThrownBy(() -> catalog.getSourceNodes(aLabel))
			.withMessage(expectedMessage);
	}

	@Test // GH-738
	void literalRetrievalShouldWork() {

		var literals = Cypher.match(Cypher.anyNode("n")).returning(Cypher.asterisk()).build().getCatalog().getLiterals();
		assertThat(literals).isEmpty();

		literals = Cypher.match(Cypher.anyNode("n").withProperties(Cypher.mapOf("a", Cypher.literalOf("A"))))
			.where(Cypher.name("n").property("prop").eq(Cypher.literalOf(42)))
			.and(Cypher.name("n").property("b").isFalse())
			.returning(Cypher.asterisk()).build().getCatalog().getLiterals();

		assertThat(literals)
			.map(Literal::asString)
			.containsExactlyInAnyOrder("'A'", "42", "false");

		var x = Cypher.name("x");
		var stmt = Cypher.usingPeriodicCommit()
			.loadCSV(URI.create("https://test.com/test.csv"))
			.as("x")
			.with("x")
			.merge(Cypher.anyNode("n").withProperties("x", x))
			.onCreate().set(x.property("y").to(Cypher.literalNull()), x.property("a").to(Cypher.literalOf("Hallo")))
			.onCreate().set(x.property("b").to(
					Cypher.subList(Cypher.listOf(Cypher.literalTrue()), 1, 2)
				)
			).returning(Cypher.raw("x"))
			.build();
		assertThat(stmt.getCypher()).isEqualTo("USING PERIODIC COMMIT LOAD CSV FROM 'https://test.com/test.csv' AS x WITH x MERGE (n {x: x}) ON CREATE SET x.y = NULL, x.a = 'Hallo' ON CREATE SET x.b = [true][1..2] RETURN x");
		assertThat(stmt.getCatalog().getLiterals())
			.map(Literal::asString)
			.containsExactlyInAnyOrder("1", "2", "NULL", "true", "'Hallo'");
	}

	@Test // GH-738
	void procedureNamesMustNotAppearAsLiterals() {

		var stmt = Cypher.call("dbms.routing.getRoutingTable").withArgs(Cypher.parameter("routingContext"), Cypher.parameter("databaseName"))
			.yieldStar()
			.build();
		assertThat(stmt.getCatalog().getLiterals()).isEmpty();
	}

	@Test // GH-674
	void retrievalOfRelationshipsShouldWork() {

		var n = Cypher.node("Person").withProperties("name", Cypher.literalOf("John Doe")).named("n");
		var m = Cypher.node("Person").named("m");

		var catalog = Cypher.match(n.relationshipTo(m, "IS_FRIEND_WITH"))
			.where(n.hasLabels("Active")).and(m.hasLabels("Kind", "Positive").and(m.property("name").eq(Cypher.literalOf("Jane"))))
			.returning(Cypher.asterisk())
			.build().getCatalog();

		var isFriendWith = StatementCatalog.Token.type("IS_FRIEND_WITH");
		assertThat(catalog.getTargetNodes(isFriendWith))
			.isNotEmpty();
		assertThat(catalog.getSourceNodes(isFriendWith))
			.isNotEmpty();

		var person = StatementCatalog.Token.label("Person");
		assertThat(catalog.getIncomingRelations(person))
			.containsExactlyInAnyOrder(isFriendWith);
		assertThat(catalog.getOutgoingRelations(person))
			.containsExactlyInAnyOrder(isFriendWith);
	}


	@RepeatedTest(value = 20)
	void random_order_test() {
		var rendererConfig = Configuration.newConfig().withDialect(Dialect.NEO4J_5).withPrettyPrint(true).build();
		var renderer = Renderer.getRenderer(rendererConfig);
		var person = Cypher.node("Person").named("n");
		var movie = Cypher.node("Movie").named("m");
		var rel = person.relationshipTo(movie).named("r");
		var innerStatement = Cypher.match(rel)
			.returning(person, rel, movie)
			.build();

		var graph_name = Cypher.name("__graph__name__");
		var statement = Cypher.unwind(Cypher.graphNames()).as(graph_name)
			.call(Cypher.use(Cypher.graphByName(graph_name), innerStatement))
			.returning(innerStatement.getCatalog().getIdentifiableExpressions())
			.build();

		assertThat(renderer.render(statement))
			.isEqualTo("""
				UNWIND graph.names() AS __graph__name__
				CALL {
				  USE graph.byName(__graph__name__)
				  MATCH (n:Person)-[r]->(m:Movie)
				  RETURN n, r, m
				}
				RETURN n, r, m""");
	}

	@Test // GH-785
	void nullParametersMustBeAllowed() {

		var statement = Cypher.match(Cypher.anyNode("n")).where(Cypher.property("n", "whatever").isEqualTo(Cypher.parameter("foo", null))).returning(Cypher.asterisk()).build();
		assertThat(statement.getCypher()).isEqualTo("MATCH (n) WHERE n.whatever = $foo RETURN *");
		assertThat(statement.getCatalog().getParameters())
			.containsEntry("foo", null);
	}
}
