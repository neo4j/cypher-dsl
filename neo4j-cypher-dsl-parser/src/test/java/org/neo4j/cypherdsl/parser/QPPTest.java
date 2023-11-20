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

import java.util.stream.Stream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

class QPPTest {

	static Stream<Arguments> f() {
		return Stream.of(
			/*
			Arguments.of("""
					MATCH (n:Station {name: 'London Euston'})<-[:CALLS_AT]-(s1:Stop)
					  -[:NEXT]->(s2:Stop)-[:CALLS_AT]->(:Station {name: 'Coventry'})
					  <-[:CALLS_AT]-(s3:Stop)-[:NEXT]->(s4:Stop)-[:CALLS_AT]->(n)
					RETURN s1.departs+'-'+s2.departs AS outbound,
					  s3.departs+'-'+s4.departs AS `return`
					""",
				"""
					MATCH (n:Station {name: 'London Euston'})<-[:CALLS_AT]-(s1:Stop)-[:NEXT]->(s2:Stop)-[:CALLS_AT]->(:Station {name: 'Coventry'})<-[:CALLS_AT]-(s3:Stop)-[:NEXT]->(s4:Stop)-[:CALLS_AT]->(n)
					RETURN ((s1.departs + '-') + s2.departs) AS outbound, ((s3.departs + '-') + s4.departs) AS return
					"""),*/
			/*
			Arguments.of("""
					MATCH (:Station { name: 'Denmark Hill' })<-[:X]-(d:Stop)
					      ((:Stop)-[:NEXT]->(:Stop)){1,3}
					      (a:Stop)-[:Y]->(:Station { name: 'Clapham Junction' })
					RETURN d.departs AS departureTime, a.arrives AS arrivalTime
					""",
				"""
					MATCH (:Station { name: 'Denmark Hill' })<-[:CALLS_AT]-(d:Stop)
					      ((:Stop)-[:NEXT]->(:Stop)){1,3}
					      (a:Stop)-[:CALLS_AT]->(:Station { name: 'Clapham Junction' })
					RETURN d.departs AS departureTime, a.arrives AS arrivalTime
					""")*/
			Arguments.of("match (:A) (()-[r:R]->()){2,3} (:B) return *", "MATCH (:A) (()-[r:R]->()){2,3} (:B) RETURN *"),
			Arguments.of("MATCH p = ((person:`Person`)-[:`DIRECTED`]->(movie:`Movie`)) WHERE person.name = 'Walt Disney' RETURN p", "MATCH p = ((person:Person)-[:DIRECTED]->(movie:Movie)) WHERE person.name = 'Walt Disney' RETURN p")
		);
	}

	@ParameterizedTest
	/*
	@CsvSource(delimiterString = "%%", textBlock = """
		MATCH (n:(TrainStation&BusStation)|StationGroup)      %% MATCH (n:`TrainStation`&`BusStation`|`StationGroup`)
		MATCH (n:Station WHERE n.name STARTS WITH 'Preston')  %% MATCH (n:`Station` WHERE n.name STARTS WITH 'Preston')
		MATCH (n)-[{ distance: 0.24, duration: 'PT4M' }]->(m) %% MATCH (n)-[ {distance: 0.24, duration: 'PT4M'}]->(m)
		MATCH (n)-[r WHERE time() + duration(r.duration) < time('22:00') ]->(m) %% MATCH (n)-[r WHERE (time() + duration(r.duration)) < time('22:00')]->(m)
		MATCH (:Station)--()<--(m WHERE m.departs > time('12:00'))-->()-[:NEXT]->(n) RETURN * %% MATCH (:`Station`)--()<--(m WHERE m.departs > time('12:00'))-->()-[:NEXT]->(n) RETURN *
		""")*/
	@MethodSource("f")
	void parsingAndRenderingOfQPPShouldWork(String input, String expected) {
		var renderer = Renderer.getRenderer(Configuration.newConfig().alwaysEscapeNames(false).build());
		Assertions.assertThat(renderer.render(CypherParser.parse(input))).isEqualTo(expected.replace("\n", " ").trim());
	}
}
