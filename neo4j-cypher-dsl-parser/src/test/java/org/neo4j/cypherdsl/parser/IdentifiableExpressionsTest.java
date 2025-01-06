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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
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
		var elements = statement.getCatalog().getIdentifiableExpressions().stream().map(e -> {
			if (e instanceof AliasedExpression) {
				return ((AliasedExpression) e).getAlias();
			}
			return ((SymbolicName) e).getValue();
		}).toList();
		assertThat(elements).hasSameElementsAs(testData.expected());
	}

	record TestData(String query, List<String> expected) {
	}

	static final List<TestData> TEST_DATA = List.of(
		new TestData("""
			MATCH (n)-[:PING_EVENT]->(e)
			WITH n, e WHERE e.date CONTAINS "-"
			WITH n, e, date(e.date) AS date
			WITH n, e ORDER BY date
			WITH n, head(collect(e)) AS event
			RETURN id(n) AS id, datetime(event.date + 'T23:59:59Z') AS lastSeenDate
			""", List.of("id", "lastSeenDate")),
		new TestData("""
			MATCH (p:Person)
			where id(p) = $id
			with p
			MATCH (p)-[:KNOWS]-(kp:Person)
			WITH p, count(distinct kp) AS knows_people_count
			MATCH (p)-[:KNOWS]-(kpc:Person)-[:PARTY_TO]->(:Crime)
			WITH p, knows_people_count, count(distinct kpc) as knows_criminals_count
			WITH p, toInteger((100*knows_criminals_count/toFloat(knows_people_count))) as score
			CALL apoc.when(
			EXISTS((p)-[:PARTY_TO]->(:Crime)),
			'RETURN 100 as score',
			'RETURN score as score',
			{p:p, score: score}) YIELD value
			return id(p) as id,  value.score as criminalRiskScore
			""", List.of("id", "criminalRiskScore")),
		new TestData("""
			MATCH (p:Person)
			where id(p) = $id
			with p
			MATCH (p)-[:KNOWS]-(kp:Person)
			WITH p, count(distinct kp) AS knows_people_count
			MATCH (p)-[:KNOWS]-(kpc:Person)-[:PARTY_TO]->(:Crime)
			WITH p, knows_people_count, count(distinct kpc) as knows_criminals_count
			WITH p, toInteger((100*knows_criminals_count/toFloat(knows_people_count))) as score
			return id(p) as id,  score as criminalRiskScore
			""", List.of("id", "criminalRiskScore")),
		new TestData("""
			MATCH (p:Person)-[:PARTY_TO]->(c:Crime)
			WHERE id(p) = $id
			RETURN $id AS id, True AS criminal
			""", List.of("id", "criminal")),
		new TestData("""
			match (l:Location)<-[]-(c:Crime)<-[]-(v:Vehicle)
			where id(v) = $id
			return id(v) as id, l.geospatial as location
			""", List.of("id", "location")),
		new TestData("""
			MATCH (n) WHERE id(n) = $id
			RETURN n.name STARTS WITH 'Ar' AS badLink, $id AS id
			""", List.of("badLink", "id")),
		new TestData("""
			MATCH (k:Keyword)-[:DESCRIBES]->(a)
			WHERE id(k) = $id
			WITH k, collect(DISTINCT a.time) AS timestamps
			RETURN timestamps, id(k) as id
			""", List.of("timestamps", "id")),
		new TestData("""
			MATCH (k:Keyword)-[:DESCRIBES]->(a)
			WHERE id(k) = $id
			WITH k, size(collect(DISTINCT a.time)) AS numberOfRoles, [1, 2, 3] AS roles
			RETURN apoc.coll.randomItems(roles, numberOfRoles, true) AS roles, id(k) AS id
			""", List.of("roles", "id")),
		new TestData("""
			MATCH (n) WHERE id(n) = $id + 10000
			RETURN id(n) AS id, 'Reviewed' IN labels(n) AS reviewed
			""", List.of("id", "reviewed")),
		new TestData("""
			MATCH (n) WHERE id(n) = $id
			RETURN $id AS id, COUNT { (n)-[:INVESTED]->() } AS totalInvestments
			""", List.of("id", "totalInvestments")),
		new TestData("""
			MATCH (n:Person)-[:HAS_PHONE]->(p:Phone)
			where id(p) = $id
			return id(p) as id, n.full_name as owner
			""", List.of("id", "owner")),
		new TestData("""
			MATCH (e:Entity)-[:IN_CLUSTER]->(c)
			WHERE id(c) = $id
			RETURN $id as id, point({latitude: avg(e.latitude), longitude:avg(e.longitude)}) as coordinates_cluster
			""", List.of("id", "coordinates_cluster")),
		new TestData("""
			MATCH (o)<-[:ORG_GROUP]-(:Organization)<-[:AWARDED_TO]-(g:Grant)
			WHERE id(o)= $id
			WITH distinct g, o
			RETURN id(o) as id, sum(g.amount) as `grant_amount`
			""", List.of("id", "grant_amount")),
		new TestData("""
			MATCH (p:Person)-[:LIVES_IN]->(l:Location)
			WHERE id(p) = $id
			RETURN $id AS id, p.pleasant_temperature_threshold <= l.ga_day_temp as IsTemperatureOK
			""", List.of("id", "IsTemperatureOK")),
		new TestData("""
			MATCH (p:Person) WHERE p.email = $node.values.email
			MATCH (c:Certification) WHERE c.name="Neo4j Certified Professional"
			RETURN $id as id, exists((p)-[:HAS_CERTIFICATION]->(c)) as neoCertified
			""", List.of("id", "neoCertified")),
		new TestData("""
			MATCH (n) WHERE id(n) = $id
			RETURN 'https://www.google.com/search?q=' + replace(n.name,' ','+') AS googleSearchUrl, $id AS id
			""", List.of("googleSearchUrl", "id"))
	);
}
