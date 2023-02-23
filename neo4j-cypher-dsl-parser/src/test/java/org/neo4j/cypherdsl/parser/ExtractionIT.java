/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.parser;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.fump.Property;
import org.neo4j.cypherdsl.core.fump.SomeGoodNameForANNonSTCComparison.Clause;
import org.neo4j.cypherdsl.core.fump.Token;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * The test does really not belong here, as the core is under test, not the parser, but as Christophe already was so kind
 * providing the test queries…  ¯\_(ツ)_/¯
 *
 * @author Michael J. Simons
 * @author Christophe Willemsen
 */
class ExtractionIT {

	private static final Configuration CYPHER_RENDERER_CONFIGURATION = Configuration.newConfig().alwaysEscapeNames(false).build();

	private static final GeneralizedRenderer RENDERER = Renderer.getRenderer(CYPHER_RENDERER_CONFIGURATION, GeneralizedRenderer.class);

	static Stream<Arguments> extractionShouldWork() {
		return TEST_DATA.stream().map(Arguments::of);
	}

	@SuppressWarnings("removal")
	@ParameterizedTest
	@MethodSource
	void extractionShouldWork(TestData testData) {
		var statement = testData.statement() == null ? CypherParser.parse(testData.query()) : testData.statement();
		var things = statement.getThings();

		assertThat(things.getNodeLabels()).containsExactlyInAnyOrderElementsOf(testData.expectedLabels());
		assertThat(things.getRelationshipTypes()).containsExactlyInAnyOrderElementsOf(testData.expectedTypes());
		assertThat(things.getProperties()).containsExactlyInAnyOrderElementsOf(testData.expectedProperties());
		if (testData.expectedComparisons != null) {
			assertThat(things.getComparisons()).allMatch(v -> testData.expectedComparisons().contains(
				new TestDataComparison(v.clause(), v.property(), v.left() == null ? null : RENDERER.render(v.left()), v.operator(), v.right() == null ? null : RENDERER.render(v.right()))
			));
		}
	}

	record TestDataComparison(
		Clause clause, Property property, String left, Operator operator, String right
	) {
	}

	record TestData(String query, Statement statement,
					List<Token> expectedLabels,
					List<Token> expectedTypes,
					List<Property> expectedProperties,
					List<TestDataComparison> expectedComparisons
	) {

		TestData(String query, List<Token> expectedLabels, List<Token> expectedTypes, List<Property> expectedProperties) {
			this(query, null, expectedLabels, expectedTypes, expectedProperties, null);
		}

		TestData(String query, List<Token> expectedLabels, List<Token> expectedTypes, List<Property> expectedProperties, List<TestDataComparison> expectedComparisons) {
			this(query, null, expectedLabels, expectedTypes, expectedProperties, expectedComparisons);
		}

		@Override
		public String toString() {
			return query == null ? "(Statement based)" : query;
		}
	}

	static final List<TestData> TEST_DATA = List.of(
		new TestData("""
			MATCH (n:Person {name: $name})
			RETURN n, COUNT { (n)--() } AS degree
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "name"), "n.name", Operator.EQUALITY, "$name")
			)
		),
		new TestData("""
			MATCH (n:Person) WHERE n.name = $name
			RETURN n, COUNT { (n)--() } AS degree
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "name"), "n.name", Operator.EQUALITY, "$name")
			)
		),
		new TestData("""
			MATCH (n) WHERE id(n) = $id
			MATCH (n)-[:CHECKED_IN_EVENT]->(e)
			WHERE e.date CONTAINS "-"
			WITH n, e, date(e.date) AS date
			WITH n, e ORDER BY date
			WITH n, head(collect(e)) AS event
			RETURN datetime(event.date + 'T23:59:59Z') AS lastSeenDate, $id AS id
			""",
			List.of(),
			List.of(Token.type("CHECKED_IN_EVENT")),
			List.of(new Property("date")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property("date"), "e.date", Operator.CONTAINS, "'-'")
			)
		),
		new TestData("""
			MATCH (n) WHERE id(n) = $id
			WITH n
			MATCH (n)-[:CHECKED_IN_EVENT]->(e)
			WHERE e.date CONTAINS "-"
			WITH n, e, date(e.date) AS date
			WITH n, e ORDER BY date
			WITH n, head(collect(e)) AS event
			RETURN datetime(event.date + 'T23:59:59Z') AS lastSeenDate, $id AS id
			""",
			List.of(),
			List.of(Token.type("CHECKED_IN_EVENT")),
			List.of(new Property("date")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property("date"), "e.date", Operator.CONTAINS, "'-'")
			)
		),
		new TestData("""
			MATCH (n) WHERE n.name = $name
			AND n.lastSeenDate > datetime() - duration({hours: 12})
			RETURN n
			""",
			List.of(), List.of(),
			List.of(new Property("name"), new Property("lastSeenDate")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property("name"), "n.name", Operator.EQUALITY, "$name"),
				new TestDataComparison(Clause.MATCH, new Property("lastSeenDate"), "n.lastSeenDate", Operator.GREATER_THAN, "(datetime() - duration({hours: 12}))")
			)
		),
		new TestData("""
			MATCH (n)-[r:FOLLOWS]->(v)
			WHERE r.id < 25
			MATCH (v)-[r2]->(x)
			WHERE r2.computed = false
			RETURN count(*) AS matches
			""",
			List.of(),
			List.of(Token.type("FOLLOWS")),
			List.of(new Property(Token.type("FOLLOWS"), "id"), new Property("computed")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.type("FOLLOWS"), "id"), "r.id", Operator.LESS_THAN, "25"),
				new TestDataComparison(Clause.MATCH, new Property("computed"), "r2.computed", Operator.EQUALITY, "false")
			)
		),
		new TestData("""
			MATCH (n)-[r:FOLLOWS]->(v)
			MATCH (v)-[r2:TRANSACTS_WITH]->(x)
			WHERE r2.computed = false
			RETURN count(*) AS matches
			""",
			List.of(),
			List.of(Token.type("FOLLOWS"), Token.type("TRANSACTS_WITH")),
			List.of(new Property(Token.type("TRANSACTS_WITH"), "computed")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.type("TRANSACTS_WITH"), "computed"), "r2.computed", Operator.EQUALITY, "false")
			)
		),
		new TestData("""
			MATCH (n:Person {name: "John Doe"})
			WHERE n:Active
			RETURN n
			""",
			List.of(Token.label("Person"), Token.label("Active")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "name"), "n.name", Operator.EQUALITY, "'John Doe'")
			)
		),
		new TestData("""
			MATCH (n:Person)
			WHERE 12 > n.id
			RETURN n
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "id")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "id"), "12", Operator.GREATER_THAN, "n.id")
			)
		),
		new TestData("""
			MATCH (n:Event)
			WHERE point.distance($point, n.position) < 1000
			RETURN n
			""",
			List.of(Token.label("Event")),
			List.of(),
			List.of(new Property(Token.label("Event"), "position")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Event"), "position"), "point.distance($point, n.position)", Operator.LESS_THAN, "1000")
			)
		),
		new TestData("""
			MATCH (n:Event)
			WHERE point.withinBox(n.position, $lowerLeft, $upperRight)
			RETURN n
			""",
			List.of(Token.label("Event")),
			List.of(),
			List.of(new Property(Token.label("Event"), "position")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Event"), "position"), "point.withinBox(n.position, $lowerLeft, $upperRight)", null, null)
			)
		),
		new TestData("""
			UNWIND $names AS name
			MATCH (n:Person {name: name})
			RETURN n
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name")),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "name"), "n.name", Operator.EQUALITY, "name")
			)
		),
		new TestData("""
			MATCH (a:Officer),(b:Officer)
			WHERE a.name CONTAINS 'MR. ISAAC ELBAZ'
			AND b.name CONTAINS 'Stephanie J. Bridges'
			MATCH (a)-[r:OFFICER_OF|INTERMEDIARY_OF|REGISTERED_ADDRESS*..10]-(b)
			WHERE r.isActive = $activeFlag
			RETURN p
			LIMIT 50
			""",
			List.of(Token.label("Officer")),
			List.of(Token.type("OFFICER_OF"), Token.type("INTERMEDIARY_OF"), Token.type("REGISTERED_ADDRESS")),
			List.of(
				new Property(Token.label("Officer"), "name"),
				new Property(Set.of(Token.type("OFFICER_OF"), Token.type("INTERMEDIARY_OF"), Token.type("REGISTERED_ADDRESS")), "isActive")
			),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Officer"), "name"), "a.name", Operator.CONTAINS, "'MR. ISAAC ELBAZ'"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Officer"), "name"), "b.name", Operator.CONTAINS, "'Stephanie J. Bridges'"),
				new TestDataComparison(Clause.MATCH, new Property(Set.of(Token.type("OFFICER_OF"), Token.type("INTERMEDIARY_OF"), Token.type("REGISTERED_ADDRESS")), "isActive"), "r.isActive", Operator.EQUALITY, "$activeFlag")
			)
		),
		new TestData(" MATCH (p:Person) -[:ACTED_IN {as: 'Neo'}] -> (m:Movie)",
			List.of(Token.label("Person"), Token.label("Movie")),
			List.of(Token.type("ACTED_IN")),
			List.of(new Property(Token.type("ACTED_IN"), "as")),
			List.of(new TestDataComparison(Clause.MATCH, new Property(Token.type("ACTED_IN"), "as"), ".as", Operator.EQUALITY, "'Neo'"))
		),
		new TestData(
			"""
			match (n:Person)
			call {
			match (n:Movie {title: 'The Matrix'}) where n.released >= 1900 return n as m
			}
			return n.name
			""",
			List.of(Token.label("Person"), Token.label("Movie")),
			List.of(),
			List.of(
				new Property(Token.label("Person"), "name"),
				new Property(Token.label("Movie"), "title"),
				new Property(Token.label("Movie"), "released")
			),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Movie"), "title"), "n.title", Operator.EQUALITY, "'The Matrix'"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Movie"), "released"), "n.released", Operator.GREATER_THAN_OR_EQUAL_TO, "1900")
			)
		),
		new TestData(
			"""
			MATCH (n:Person {name: 'Tom Hanks'})
			CALL {
			  WITH n
			  MATCH (m:Movie)<-[:ACTED_IN]-(n)
			  WHERE (m.released >= 1900
			    AND n.born = 1956)
			  RETURN m
			}
			RETURN n.name, m.title
			""",
			List.of(Token.label("Person"), Token.label("Movie")),
			List.of(Token.type("ACTED_IN")),
			List.of(
				new Property(Token.label("Person"), "name"),
				new Property(Token.label("Person"), "born"),
				new Property(Token.label("Movie"), "released"),
				new Property(Token.label("Movie"), "title")
			),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "name"), "n.name", Operator.EQUALITY, "'Tom Hanks'"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "born"), "n.born", Operator.EQUALITY, "1956"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Movie"), "released"), "m.released", Operator.GREATER_THAN_OR_EQUAL_TO, "1900")
			)
		),
		new TestData(
			null,
			Cypher
				.match(Cypher.node("Person").named("n").withProperties("name", Cypher.literalOf("Tom Hanks")))
				.call(
					Cypher.match(
							Cypher.node("Movie").named("m").relationshipFrom(Cypher.anyNode("n"), "ACTED_IN")
						).where(Cypher.anyNode("m").property("released").gte(Cypher.literalOf(1900)).and(Cypher.anyNode("n").property("born").eq(Cypher.literalOf(1956))))
						.returning(Cypher.anyNode("m"))
						.build(), Cypher.anyNode("n")
				)
				.returning(Cypher.anyNode("n").property("name"), Cypher.anyNode("m").property("title"))
				.build(),
			List.of(Token.label("Person"), Token.label("Movie")),
			List.of(Token.type("ACTED_IN")),
			List.of(
				new Property(Token.label("Person"), "name"),
				new Property(Token.label("Person"), "born"),
				new Property(Token.label("Movie"), "released"),
				new Property(Token.label("Movie"), "title")
			),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "name"), "n.name", Operator.EQUALITY, "'Tom Hanks'"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "born"), "n.born", Operator.EQUALITY, "1956"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Movie"), "released"), "m.released", Operator.GREATER_THAN_OR_EQUAL_TO, "1900")
			)
		),
		new TestData(
			"""
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
			""",
			List.of(Token.label("Person"), Token.label("Movie")),
			List.of(Token.type("ACTED_IN")),
			List.of(
				new Property(Token.label("Person"), "name"),
				new Property(Token.label("Person"), "born"),
				new Property(Token.label("Movie"), "released"),
				new Property(Token.label("Movie"), "title")
			),
			List.of(
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "name"), "n.name", Operator.EQUALITY, "'Tom Hanks'"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Person"), "born"), "n.born", Operator.EQUALITY, "1956"),
				new TestDataComparison(Clause.MATCH, new Property(Token.label("Movie"), "released"), "m.released", Operator.GREATER_THAN_OR_EQUAL_TO, "1900"),
				new TestDataComparison(Clause.WITH, new Property(Token.label("Movie"), "title"), "m.title", Operator.EQUALITY, "'Apollo 13'")
			)
		),
		new TestData(
			"CREATE (n:Foo {m: 'Bar'})",
			List.of(Token.label("Foo")),
			List.of(),
			List.of(new Property(Token.label("Foo"), "m")),
			List.of(new TestDataComparison(Clause.CREATE, new Property(Token.label("Foo"), "m"), "n.m", Operator.EQUALITY, "'Bar'"))
		),
		new TestData(
			"MATCH (n:Foo {m: 'Bar'}) DELETE n",
			List.of(Token.label("Foo")),
			List.of(),
			List.of(new Property(Token.label("Foo"), "m")),
			List.of(new TestDataComparison(Clause.MATCH, new Property(Token.label("Foo"), "m"), "n.m", Operator.EQUALITY, "'Bar'"))
		),
		new TestData(
			"MERGE (n:Foo {m: 'Bar'})",
			List.of(Token.label("Foo")),
			List.of(),
			List.of(new Property(Token.label("Foo"), "m")),
			List.of(new TestDataComparison(Clause.MERGE, new Property(Token.label("Foo"), "m"), "n.m", Operator.EQUALITY, "'Bar'"))
		)
	);
}
