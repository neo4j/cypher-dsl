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
package org.neo4j.cypherdsl.parser;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.StatementCatalog.Clause;
import org.neo4j.cypherdsl.core.StatementCatalog.Property;
import org.neo4j.cypherdsl.core.StatementCatalog.Token;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * The test does really not belong here, as the core is under test, not the parser, but as
 * Christophe already was so kind providing the test queries… ¯\_(ツ)_/¯
 *
 * @author Michael J. Simons
 * @author Christophe Willemsen
 */
class ExtractionIT {

	private static final Configuration CYPHER_RENDERER_CONFIGURATION = Configuration.newConfig()
		.alwaysEscapeNames(false)
		.build();

	private static final GeneralizedRenderer RENDERER = Renderer.getRenderer(CYPHER_RENDERER_CONFIGURATION,
			GeneralizedRenderer.class);

	private static final Token FOLLOWS = Token.type("FOLLOWS");

	private static final Token TRANSACTS_WITH = Token.type("TRANSACTS_WITH");

	private static final Token PERSON = Token.label("Person");

	private static final Token ACTIVE = Token.label("Active");

	private static final Token EVENT = Token.label("Event");

	private static final Token CHECKED_IN_EVENT = Token.type("CHECKED_IN_EVENT");

	private static final Token OFFICER = Token.label("Officer");

	private static final Token MOVIE = Token.label("Movie");

	private static final Property PERSON_NAME = new Property(PERSON, "name");

	private static final Property DATE = new Property("date");

	private static final Property LAST_SEEN_DATE = new Property("lastSeenDate");

	private static final Property NAME = new Property("name");

	private static final Property FOLLOWS_ID = new Property(FOLLOWS, "id");

	private static final Property PERSON_ID = new Property(PERSON, "id");

	private static final Property EVENT_POSITION = new Property(EVENT, "position");

	private static final Property OFFICER_NAME = new Property(OFFICER, "name");

	private static final Property PERSON_BORN = new Property(PERSON, "born");

	private static final Property MOVIE_RELEASED = new Property(MOVIE, "released");

	private static final Property MOVIE_TITLE = new Property(MOVIE, "title");
	static final List<TestData> TEST_DATA = List.of(
			new TestData("""
					MATCH (n:Person {name: $name})
					RETURN n, COUNT { (n)--() } AS degree
					""", List.of(PERSON), List.of(), List.of(PERSON_NAME),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_NAME, "n.name", Operator.EQUALITY, "$name",
							Set.of("name")))),
			new TestData("""
					MATCH (n:Person) WHERE n.name = $name
					RETURN n, COUNT { (n)--() } AS degree
					""", List.of(PERSON), List.of(), List.of(PERSON_NAME),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_NAME, "n.name", Operator.EQUALITY, "$name",
							Set.of("name")))),
			new TestData("""
					MATCH (n) WHERE id(n) = $id
					MATCH (n)-[:CHECKED_IN_EVENT]->(e)
					WHERE e.date CONTAINS "-"
					WITH n, e, date(e.date) AS date
					WITH n, e ORDER BY date
					WITH n, head(collect(e)) AS event
					RETURN datetime(event.date + 'T23:59:59Z') AS lastSeenDate, $id AS id
					""", List.of(), List.of(CHECKED_IN_EVENT), List.of(DATE),
					List.of(new TestDataComparison(Clause.MATCH, DATE, "e.date", Operator.CONTAINS, "'-'", Set.of()))),
			new TestData("""
					MATCH (n) WHERE id(n) = $id
					WITH n
					MATCH (n)-[:CHECKED_IN_EVENT]->(e)
					WHERE e.date CONTAINS "-"
					WITH n, e, date(e.date) AS date
					WITH n, e ORDER BY date
					WITH n, head(collect(e)) AS event
					RETURN datetime(event.date + 'T23:59:59Z') AS lastSeenDate, $id AS id
					""", List.of(), List.of(CHECKED_IN_EVENT), List.of(DATE),
					List.of(new TestDataComparison(Clause.MATCH, DATE, "e.date", Operator.CONTAINS, "'-'", Set.of()))),
			new TestData("""
					MATCH (n) WHERE n.name = $name
					AND n.lastSeenDate > datetime() - duration({hours: 12})
					RETURN n
					""", List.of(), List.of(), List.of(NAME, LAST_SEEN_DATE), List.of(
					new TestDataComparison(Clause.MATCH, NAME, "n.name", Operator.EQUALITY, "$name", Set.of("name")),
					new TestDataComparison(Clause.MATCH, LAST_SEEN_DATE, "n.lastSeenDate", Operator.GREATER_THAN,
							"(datetime() - duration({hours: 12}))", Set.of()))),
			new TestData("""
					MATCH (n)-[r:FOLLOWS]->(v)
					WHERE r.id < 25
					MATCH (v)-[r2]->(x)
					WHERE r2.computed = false
					RETURN count(*) AS matches
					""", List.of(), List.of(FOLLOWS), List.of(FOLLOWS_ID, new Property("computed")), List.of(
					new TestDataComparison(Clause.MATCH, FOLLOWS_ID, "r.id", Operator.LESS_THAN, "25", Set.of()),
					new TestDataComparison(Clause.MATCH, new Property("computed"), "r2.computed", Operator.EQUALITY,
							"false", Set.of()))),
			new TestData("""
					MATCH (n)-[r:FOLLOWS]->(v)
					MATCH (v)-[r2:TRANSACTS_WITH]->(x)
					WHERE r2.computed = false
					RETURN count(*) AS matches
					""", List.of(), List.of(FOLLOWS, TRANSACTS_WITH), List.of(new Property(TRANSACTS_WITH, "computed")),
					List.of(new TestDataComparison(Clause.MATCH, new Property(TRANSACTS_WITH, "computed"),
							"r2.computed", Operator.EQUALITY, "false", Set.of()))),
			new TestData("""
					MATCH (n:Person {name: "John Doe"})
					WHERE n:Active
					RETURN n
					""", List.of(PERSON, ACTIVE), List.of(), List.of(PERSON_NAME),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_NAME, "n.name", Operator.EQUALITY, "'John Doe'",
							Set.of()))),
			new TestData("""
					MATCH (n:Person)
					WHERE 12 > n.id
					RETURN n
					""", List.of(PERSON), List.of(), List.of(PERSON_ID),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_ID, "12", Operator.GREATER_THAN, "n.id",
							Set.of()))),
			new TestData("""
					MATCH (n:Event)
					WHERE point.distance($point, n.position) < 1000
					RETURN n
					""", List.of(EVENT), List.of(), List.of(EVENT_POSITION),
					List.of(new TestDataComparison(Clause.MATCH, EVENT_POSITION, "point.distance($point, n.position)",
							Operator.LESS_THAN, "1000", Set.of("point")))),
			new TestData("""
					MATCH (n:Event)
					WHERE point.withinBox(n.position, $lowerLeft, $upperRight)
					RETURN n
					""", List.of(EVENT), List.of(), List.of(EVENT_POSITION),
					List.of(new TestDataComparison(Clause.MATCH, EVENT_POSITION,
							"point.withinBox(n.position, $lowerLeft, $upperRight)", null, null,
							Set.of("lowerLeft", "upperRight")))),
			new TestData("""
					UNWIND $names AS name
					MATCH (n:Person {name: name})
					RETURN n
					""", List.of(PERSON), List.of(), List.of(PERSON_NAME),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_NAME, "n.name", Operator.EQUALITY, "name",
							Set.of()))),
			new TestData("""
					MATCH (a:Officer),(b:Officer)
					WHERE a.name CONTAINS 'MR. ISAAC ELBAZ'
					AND b.name CONTAINS 'Stephanie J. Bridges'
					MATCH (a)-[r:OFFICER_OF|INTERMEDIARY_OF|REGISTERED_ADDRESS*..10]-(b)
					WHERE r.isActive = $activeFlag
					RETURN p
					LIMIT 50
					""", List.of(OFFICER),
					List.of(Token.type("OFFICER_OF"), Token.type("INTERMEDIARY_OF"), Token.type("REGISTERED_ADDRESS")),
					List.of(OFFICER_NAME,
							new Property(Set.of(Token.type("OFFICER_OF"), Token.type("INTERMEDIARY_OF"),
									Token.type("REGISTERED_ADDRESS")), "isActive")),
					List.of(new TestDataComparison(Clause.MATCH, OFFICER_NAME, "a.name", Operator.CONTAINS,
							"'MR. ISAAC ELBAZ'", Set.of()),
							new TestDataComparison(Clause.MATCH, OFFICER_NAME, "b.name", Operator.CONTAINS,
									"'Stephanie J. Bridges'", Set.of()),
							new TestDataComparison(Clause.MATCH,
									new Property(Set.of(Token.type("OFFICER_OF"), Token.type("INTERMEDIARY_OF"),
											Token.type("REGISTERED_ADDRESS")), "isActive"),
									"r.isActive", Operator.EQUALITY, "$activeFlag", Set.of("activeFlag")))),
			new TestData(" MATCH (p:Person) -[:ACTED_IN {as: 'Neo'}] -> (m:Movie)", List.of(PERSON, MOVIE),
					List.of(Token.type("ACTED_IN")), List.of(new Property(Token.type("ACTED_IN"), "as")),
					List.of(new TestDataComparison(Clause.MATCH, new Property(Token.type("ACTED_IN"), "as"), ".as",
							Operator.EQUALITY, "'Neo'", Set.of()))),
			new TestData("""
					match (n:Person)
					call {
					match (n:Movie {title: 'The Matrix'}) where n.released >= 1900 return n as m
					}
					return n.name
					""", List.of(PERSON, MOVIE), List.of(), List.of(PERSON_NAME, MOVIE_TITLE, MOVIE_RELEASED),
					List.of(new TestDataComparison(Clause.MATCH, MOVIE_TITLE, "n.title", Operator.EQUALITY,
							"'The Matrix'", Set.of()),
							new TestDataComparison(Clause.MATCH, MOVIE_RELEASED, "n.released",
									Operator.GREATER_THAN_OR_EQUAL_TO, "1900", Set.of()))),
			new TestData("""
					MATCH (n:Person {name: 'Tom Hanks'})
					CALL {
					  WITH n
					  MATCH (m:Movie)<-[:ACTED_IN]-(n)
					  WHERE (m.released >= 1900
					    AND n.born = 1956)
					  RETURN m
					}
					RETURN n.name, m.title
					""", List.of(PERSON, MOVIE), List.of(Token.type("ACTED_IN")),
					List.of(PERSON_NAME, PERSON_BORN, MOVIE_RELEASED, MOVIE_TITLE),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_NAME, "n.name", Operator.EQUALITY,
							"'Tom Hanks'", Set.of()),
							new TestDataComparison(Clause.MATCH, PERSON_BORN, "n.born", Operator.EQUALITY, "1956",
									Set.of()),
							new TestDataComparison(Clause.MATCH, MOVIE_RELEASED, "m.released",
									Operator.GREATER_THAN_OR_EQUAL_TO, "1900", Set.of()))),
			new TestData(null, Cypher
				.match(Cypher.node("Person").named("n").withProperties("name", Cypher.literalOf("Tom Hanks")))
				.call(Cypher.match(Cypher.node("Movie").named("m").relationshipFrom(Cypher.anyNode("n"), "ACTED_IN"))
					.where(Cypher.anyNode("m")
						.property("released")
						.gte(Cypher.literalOf(1900))
						.and(Cypher.anyNode("n").property("born").eq(Cypher.literalOf(1956))))
					.returning(Cypher.anyNode("m"))
					.build(), Cypher.anyNode("n"))
				.returning(Cypher.anyNode("n").property("name"), Cypher.anyNode("m").property("title"))
				.build(), List.of(PERSON, MOVIE), List.of(Token.type("ACTED_IN")),
					List.of(PERSON_NAME, PERSON_BORN, MOVIE_RELEASED, MOVIE_TITLE),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_NAME, "n.name", Operator.EQUALITY,
							"'Tom Hanks'", Set.of()),
							new TestDataComparison(Clause.MATCH, PERSON_BORN, "n.born", Operator.EQUALITY, "1956",
									Set.of()),
							new TestDataComparison(Clause.MATCH, MOVIE_RELEASED, "m.released",
									Operator.GREATER_THAN_OR_EQUAL_TO, "1900", Set.of()))),
			new TestData("""
					MATCH (n:Person WHERE n.name = 'Tom Hanks')
					CALL {
					  WITH n
					  MATCH (m:Movie)<-[:ACTED_IN]-(n)
					  WHERE (m.released >= 1900
					    AND n.born = 1956)
					  RETURN m
					}
					WITH n, m
					WHERE m.title = 'Apollo 13'
					RETURN n, m
					""", List.of(PERSON, MOVIE), List.of(Token.type("ACTED_IN")),
					List.of(PERSON_NAME, PERSON_BORN, MOVIE_RELEASED, MOVIE_TITLE),
					List.of(new TestDataComparison(Clause.MATCH, PERSON_NAME, "n.name", Operator.EQUALITY,
							"'Tom Hanks'", Set.of()),
							new TestDataComparison(Clause.MATCH, PERSON_BORN, "n.born", Operator.EQUALITY, "1956",
									Set.of()),
							new TestDataComparison(Clause.MATCH, MOVIE_RELEASED, "m.released",
									Operator.GREATER_THAN_OR_EQUAL_TO, "1900", Set.of()),
							new TestDataComparison(Clause.WITH, MOVIE_TITLE, "m.title", Operator.EQUALITY,
									"'Apollo 13'", Set.of()))),
			new TestData("CREATE (n:Foo {m: 'Bar'})", List.of(Token.label("Foo")), List.of(),
					List.of(new Property(Token.label("Foo"), "m")),
					List.of(new TestDataComparison(Clause.CREATE, new Property(Token.label("Foo"), "m"), "n.m",
							Operator.EQUALITY, "'Bar'", Set.of()))),
			new TestData("MATCH (n:Foo {m: 'Bar'}) DELETE n", List.of(Token.label("Foo")), List.of(),
					List.of(new Property(Token.label("Foo"), "m")),
					List.of(new TestDataComparison(Clause.MATCH, new Property(Token.label("Foo"), "m"), "n.m",
							Operator.EQUALITY, "'Bar'", Set.of()))),
			new TestData("MERGE (n:Foo {m: 'Bar'})", List.of(Token.label("Foo")), List.of(),
					List.of(new Property(Token.label("Foo"), "m")),
					List.of(new TestDataComparison(Clause.MERGE, new Property(Token.label("Foo"), "m"), "n.m",
							Operator.EQUALITY, "'Bar'", Set.of()))),
			new TestData(
					"MATCH (m:`Movie` {title: 'The Matrix'})<-[a:`ACTED_IN`]-(p:`Person`) WHERE p.born >= $born RETURN p",
					List.of(MOVIE, PERSON), List.of(Token.type("ACTED_IN")), List.of(MOVIE_TITLE, PERSON_BORN),
					List.of(new TestDataComparison(Clause.MATCH, MOVIE_TITLE, "m.title", Operator.EQUALITY,
							"'The Matrix'", Set.of()),
							new TestDataComparison(Clause.MATCH, PERSON_BORN, "p.born",
									Operator.GREATER_THAN_OR_EQUAL_TO, "$born", Set.of("born")))));

	static Stream<Arguments> extractionShouldWork() {
		return TEST_DATA.stream().map(Arguments::of);
	}

	@SuppressWarnings("removal")
	@ParameterizedTest
	@MethodSource
	void extractionShouldWork(TestData testData) {
		var statement = (testData.statement() == null) ? CypherParser.parse(testData.query()) : testData.statement();
		var catalog = statement.getCatalog();

		assertThat(catalog.getNodeLabels()).containsExactlyInAnyOrderElementsOf(testData.expectedLabels());
		assertThat(catalog.getRelationshipTypes()).containsExactlyInAnyOrderElementsOf(testData.expectedTypes());
		assertThat(catalog.getProperties()).containsExactlyInAnyOrderElementsOf(testData.expectedProperties());
		if (testData.expectedComparisons != null) {
			for (TestDataComparison expectedComparison : testData.expectedComparisons()) {
				assertThat(catalog.getFilters(expectedComparison.property()))
					.allMatch(v -> testData.expectedComparisons()
						.contains(new TestDataComparison(v.clause(), expectedComparison.property,
								(v.left() == null) ? null : RENDERER.render(v.left()), v.operator(),
								(v.right() == null) ? null : RENDERER.render(v.right()), v.parameterNames())));
			}
		}
	}

	record TestDataComparison(Clause clause, Property property, String left, Operator operator, String right,
			Set<String> parameterNames) {
	}

	record TestData(String query, Statement statement, List<Token> expectedLabels, List<Token> expectedTypes,
			List<Property> expectedProperties, List<TestDataComparison> expectedComparisons) {

		TestData(String query, List<Token> expectedLabels, List<Token> expectedTypes,
				List<Property> expectedProperties) {
			this(query, null, expectedLabels, expectedTypes, expectedProperties, null);
		}

		TestData(String query, List<Token> expectedLabels, List<Token> expectedTypes, List<Property> expectedProperties,
				List<TestDataComparison> expectedComparisons) {
			this(query, null, expectedLabels, expectedTypes, expectedProperties, expectedComparisons);
		}

		@Override
		public String toString() {
			return (this.query == null) ? "(Statement based)" : this.query;
		}
	}

}
