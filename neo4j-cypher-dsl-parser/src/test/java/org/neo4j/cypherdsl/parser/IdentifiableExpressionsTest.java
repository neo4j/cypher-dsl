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
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.AliasedExpression;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * More of a test for the core itself, but it's easier to just parse the already existing queries here and use that
 * AST than recreate them manually.
 *
 * @author Christophe Willemsen
 * @author Michael J. Simons
 */
class IdentifiableExpressionsTest {

	static Stream<Arguments> testGetIdentifiableElementsFromQuery() {
		return TEST_DATA.stream().map(Arguments::of);
	}

	@ParameterizedTest
	@MethodSource
	void testGetIdentifiableElementsFromQuery(TestData testData) {
		var statement = CypherParser.parse(testData.query());
		var elements = statement.getIdentifiableExpressions().stream().map(e -> {
			if (e instanceof AliasedExpression) {
				return ((AliasedExpression) e).getAlias();
			}
			return ((SymbolicName) e).getValue();
		}).collect(Collectors.toList());
		assertThat(elements).hasSameElementsAs(testData.expected());
	}

	static final class TestData {

		private final String query;
		private final List<String> expected;

		TestData(String query, List<String> expected) {
			this.query = query;
			this.expected = expected;
		}

		public String query() {
			return query;
		}

		public List<String> expected() {
			return expected;
		}

		@Override
		public String toString() {
			return "TestData{query='" + query + "'}";
		}
	}

	static final List<TestData> TEST_DATA = List.of(
		new TestData("MATCH (n)-[:PING_EVENT]->(e)\n" +
			"WITH n, e WHERE e.date CONTAINS \"-\"\n" +
			"WITH n, e, date(e.date) AS date\n" +
			"WITH n, e ORDER BY date\n" +
			"WITH n, head(collect(e)) AS event\n" +
			"RETURN id(n) AS id, datetime(event.date + 'T23:59:59Z') AS lastSeenDate\n", List.of("id", "lastSeenDate")),
		new TestData("MATCH (p:Person)\n" +
			"where id(p) = $id\n" +
			"with p\n" +
			"MATCH (p)-[:KNOWS]-(kp:Person)\n" +
			"WITH p, count(distinct kp) AS knows_people_count\n" +
			"MATCH (p)-[:KNOWS]-(kpc:Person)-[:PARTY_TO]->(:Crime)\n" +
			"WITH p, knows_people_count, count(distinct kpc) as knows_criminals_count\n" +
			"WITH p, toInteger((100*knows_criminals_count/toFloat(knows_people_count))) as score\n" +
			"CALL apoc.when(\n" +
			"EXISTS((p)-[:PARTY_TO]->(:Crime)),\n" +
			"'RETURN 100 as score',\n" +
			"'RETURN score as score',\n" +
			"{p:p, score: score}) YIELD value\n" +
			"return id(p) as id,  value.score as criminalRiskScore\n", List.of("id", "criminalRiskScore")),
		new TestData("MATCH (p:Person)\n" +
			"where id(p) = $id\n" +
			"with p\n" +
			"MATCH (p)-[:KNOWS]-(kp:Person)\n" +
			"WITH p, count(distinct kp) AS knows_people_count\n" +
			"MATCH (p)-[:KNOWS]-(kpc:Person)-[:PARTY_TO]->(:Crime)\n" +
			"WITH p, knows_people_count, count(distinct kpc) as knows_criminals_count\n" +
			"WITH p, toInteger((100*knows_criminals_count/toFloat(knows_people_count))) as score\n" +
			"return id(p) as id,  score as criminalRiskScore\n", List.of("id", "criminalRiskScore")),
		new TestData("MATCH (p:Person)-[:PARTY_TO]->(c:Crime)\n" +
			"WHERE id(p) = $id\n" +
			"RETURN $id AS id, True AS criminal\n", List.of("id", "criminal")),
		new TestData("match (l:Location)<-[]-(c:Crime)<-[]-(v:Vehicle)\n" +
			"where id(v) = $id\n" +
			"return id(v) as id, l.geospatial as location\n", List.of("id", "location")),
		new TestData("MATCH (n) WHERE id(n) = $id\n" +
			"RETURN n.name STARTS WITH 'Ar' AS badLink, $id AS id\n", List.of("badLink", "id")),
		new TestData("MATCH (k:Keyword)-[:DESCRIBES]->(a)\n" +
			"WHERE id(k) = $id\n" +
			"WITH k, collect(DISTINCT a.time) AS timestamps\n" +
			"RETURN timestamps, id(k) as id\n", List.of("timestamps", "id")),
		new TestData("MATCH (k:Keyword)-[:DESCRIBES]->(a)\n" +
			"WHERE id(k) = $id\n" +
			"WITH k, size(collect(DISTINCT a.time)) AS numberOfRoles, [1, 2, 3] AS roles\n" +
			"RETURN apoc.coll.randomItems(roles, numberOfRoles, true) AS roles, id(k) AS id\n", List.of("roles", "id")),
		new TestData("MATCH (n) WHERE id(n) = $id + 10000\n" +
			"RETURN id(n) AS id, 'Reviewed' IN labels(n) AS reviewed\n", List.of("id", "reviewed")),
		new TestData("MATCH (n:Person)-[:HAS_PHONE]->(p:Phone)\n" +
			"where id(p) = $id\n" +
			"return id(p) as id, n.full_name as owner\n", List.of("id", "owner")),
		new TestData("MATCH (e:Entity)-[:IN_CLUSTER]->(c)\n" +
			"WHERE id(c) = $id\n" +
			"RETURN $id as id, point({latitude: avg(e.latitude), longitude:avg(e.longitude)}) as coordinates_cluster\n", List.of("id", "coordinates_cluster")),
		new TestData("MATCH (o)<-[:ORG_GROUP]-(:Organization)<-[:AWARDED_TO]-(g:Grant)\n" +
			"WHERE id(o)= $id\n" +
			"WITH distinct g, o\n" +
			"RETURN id(o) as id, sum(g.amount) as `grant_amount`\n", List.of("id", "grant_amount")),
		new TestData("MATCH (p:Person)-[:LIVES_IN]->(l:Location)\n" +
			"WHERE id(p) = $id\n" +
			"RETURN $id AS id, p.pleasant_temperature_threshold <= l.ga_day_temp as IsTemperatureOK\n", List.of("id", "IsTemperatureOK")),
		new TestData("MATCH (p:Person) WHERE p.email = $node.values.email\n" +
			"MATCH (c:Certification) WHERE c.name=\"Neo4j Certified Professional\"\n" +
			"RETURN $id as id, exists((p)-[:HAS_CERTIFICATION]->(c)) as neoCertified\n", List.of("id", "neoCertified")),
		new TestData("MATCH (n) WHERE id(n) = $id\n" +
			"RETURN 'https://www.google.com/search?q=' + replace(n.name,' ','+') AS googleSearchUrl, $id AS id\n", List.of("googleSearchUrl", "id"))
	);
}
