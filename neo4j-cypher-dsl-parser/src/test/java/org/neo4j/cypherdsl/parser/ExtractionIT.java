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
import org.neo4j.cypherdsl.core.fump.Property;
import org.neo4j.cypherdsl.core.fump.Token;

/**
 * The test does really not belong here, as the core is under test, not the parser, but as Christophe already was so kind
 * providing the test queries…  ¯\_(ツ)_/¯
 *
 * @author Michael J. Simons
 * @author Christophe Willemsen
 */
class ExtractionIT {

	static Stream<Arguments> extractionShouldWork() {
		return TEST_DATA.stream().map(Arguments::of);
	}

	@SuppressWarnings("removal")
	@ParameterizedTest
	@MethodSource
	void extractionShouldWork(TestData testData) {
		var statement = CypherParser.parse(testData.query());
		var things = statement.getThings();
		assertThat(things.getNodeLabels()).containsExactlyInAnyOrderElementsOf(testData.expectedLabels());
		assertThat(things.getRelationshipTypes()).containsExactlyInAnyOrderElementsOf(testData.expectedTypes());
		assertThat(things.getProperties()).containsExactlyInAnyOrderElementsOf(testData.expectedProperties());
	}

	record TestData(String query, List<Token> expectedLabels, List<Token> expectedTypes, List<Property> expectedProperties) {
	}

	static final List<TestData> TEST_DATA = List.of(
		new TestData("""
			MATCH (n:Person {name: $name})
			RETURN n, COUNT { (n)--() } AS degree
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name"))
		),
		new TestData("""
			MATCH (n:Person) WHERE n.name = $name
			RETURN n, COUNT { (n)--() } AS degree
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name"))
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
			List.of(new Property("date"))
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
			List.of(new Property("date"))
		),
		new TestData("""
			MATCH (n) WHERE n.name = $name
			AND n.lastSeenDate > datetime() - duration({hours: 12})
			RETURN n
			""",
			List.of(), List.of(),
			List.of(new Property("name"), new Property("lastSeenDate"))
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
			List.of(new Property(Token.type("FOLLOWS"), "id"), new Property("computed"))
		),
		new TestData("""
			MATCH (n)-[r:FOLLOWS]->(v)
			MATCH (v)-[r2:TRANSACTS_WITH]->(x)
			WHERE r2.computed = false
			RETURN count(*) AS matches
			""",
			List.of(),
			List.of(Token.type("FOLLOWS"), Token.type("TRANSACTS_WITH")),
			List.of(new Property(Token.type("TRANSACTS_WITH"), "computed"))
		),
		new TestData("""
			MATCH (n:Person {name: "John Doe"})
			WHERE n:Active
			RETURN n
			""",
			List.of(Token.label("Person"), Token.label("Active")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name"))
		),
		new TestData("""
			MATCH (n:Person)
			WHERE 12 > n.id
			RETURN n
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "id"))
		),
		new TestData("""
			MATCH (n:Event)
			WHERE point.distance($point, n.position) < 1000
			RETURN n
			""",
			List.of(Token.label("Event")),
			List.of(),
			List.of(new Property(Token.label("Event"), "position"))
		),
		new TestData("""
			MATCH (n:Event)
			WHERE point.withinBox(n.position, $lowerLeft, $upperRight)
			RETURN n
			""",
			List.of(Token.label("Event")),
			List.of(),
			List.of(new Property(Token.label("Event"), "position"))
		),
		new TestData("""
			UNWIND $names AS name
			MATCH (n:Person {name: name})
			RETURN n
			""",
			List.of(Token.label("Person")),
			List.of(),
			List.of(new Property(Token.label("Person"), "name"))
		),
		new TestData("""
			MATCH (a:Officer),(b:Officer)
			WHERE a.name CONTAINS 'MR. ISAAC ELBAZ'\s
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
			)
		),
		new TestData("""
				MATCH (p:Person) -[:ACTED_IN {as: 'Neo'}] -> (m:Movie)
			""",
			List.of(Token.label("Person"), Token.label("Movie")),
			List.of(Token.type("ACTED_IN")),
			List.of(new Property(Token.type("ACTED_IN"), "as"))
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
			)
		)
	);
}
