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

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

class QPPTest {

	static Stream<Arguments> f() {
		return Stream.of(
		Arguments.of("MATCH ((n:A)-[:R]->(:B) WHERE EXISTS { (n)-->+(:X) }){2,3}", "MATCH ((n:A)-[:R]->(:B) WHERE EXISTS { (n)-->+(:X) }){2,3}"),
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
					"""),
			Arguments.of("""
					MATCH (:Station { name: 'Denmark Hill' })<-[:X]-(d:Stop)
					      ((:Stop)-[:NEXT]->(:Stop)){1,3}
					      (a:Stop)-[:Y]->(:Station { name: 'Clapham Junction' })
					RETURN d.departs AS departureTime, a.arrives AS arrivalTime
					""",
				"""
					MATCH (:Station {name: 'Denmark Hill'})<-[:X]-(d:Stop)
					((:Stop)-[:NEXT]->(:Stop)){1,3}
					(a:Stop)-[:Y]->(:Station {name: 'Clapham Junction'})
					RETURN d.departs AS departureTime, a.arrives AS arrivalTime
					"""),
			Arguments.of("match (:A) (()-[r:R]->()){2,3} (:B) return *", "MATCH (:A) (()-[r:R]->()){2,3} (:B) RETURN *"),
			Arguments.of("match (:A) (()-[r:R]->())+ (:B) return *", "MATCH (:A) (()-[r:R]->())+ (:B) RETURN *"),
			Arguments.of("match (:A) (()-[r:R]->())* (:B) return *", "MATCH (:A) (()-[r:R]->())* (:B) RETURN *"),
			Arguments.of("MATCH p = ((person:`Person`)-[:`DIRECTED`]->(movie:`Movie`)) WHERE person.name = 'Walt Disney' RETURN p", "MATCH p = ((person:Person)-[:DIRECTED]->(movie:Movie)) WHERE person.name = 'Walt Disney' RETURN p"),
			Arguments.of("MATCH ((:Station {name: 'Denmark Hill'})-[l:LINK]-(s:Station)){1,4} RETURN *", "MATCH ((:Station {name: 'Denmark Hill'})-[l:LINK]-(s:Station)){1,4} RETURN *"),
			Arguments.of("""
				MATCH p = (:Station {name: 'Denmark Hill'}) (()-[link:LINK]-())+ (:Station {name: 'Gatwick Airport'})
				RETURN reduce(acc = 0.0, l IN link | round(acc + l.distance, 2)) AS total,
				  [n in nodes(p) | n.name] AS stations
				ORDER BY total
				LIMIT 1
				""",
				"""
					MATCH p = (:Station {name: 'Denmark Hill'}) (()-[link:LINK]-())+ (:Station {name: 'Gatwick Airport'})
					RETURN reduce(acc = 0.0, l IN link | round((acc + l.distance), 2)) AS total,
					[n IN nodes(p) | n.name] AS stations
					ORDER BY total ASC
					LIMIT 1"""),
				Arguments.of("""
					MATCH p = (:Station {name: 'Denmark Hill'}) (()-[link:LINK]-()){1,28}\s
					            (:Station {name: 'Gatwick Airport'})
					RETURN size(relationships(p)) AS numStations, count(*) AS numRoutes
					ORDER BY numStations""",
					"""
						MATCH p = (:Station {name: 'Denmark Hill'}) (()-[link:LINK]-()){1,28} (:Station {name: 'Gatwick Airport'})
						RETURN size(relationships(p)) AS numStations, count(*) AS numRoutes
						ORDER BY numStations ASC
						"""),
			Arguments.of("""
				MATCH (gtw:Station {name: 'Gatwick Airport'})
				MATCH p = (:Station {name: 'Denmark Hill'})\s
				          ((l)-[link:LINK]-(r) WHERE point.distance(r.location, gtw.location)\s
				             - point.distance(l.location, gtw.location) < 1000)+ (gtw)
				RETURN reduce(acc = 0.0, l IN link | round(acc + l.distance, 2)) AS total,\s
				 [n in nodes(p) | n.name] AS stations
				ORDER BY total
				LIMIT 1
				""", """
				MATCH (gtw:Station {name: 'Gatwick Airport'})
				MATCH p = (:Station {name: 'Denmark Hill'}) ((l)-[link:LINK]-(r) WHERE (point.distance(r.location, gtw.location) - point.distance(l.location, gtw.location)) < 1000)+ (gtw)
				RETURN reduce(acc = 0.0, l IN link | round((acc + l.distance), 2)) AS total,
				[n IN nodes(p) | n.name] AS stations
				ORDER BY total ASC
				LIMIT 1
				"""),
			Arguments.of("""
				MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1a:CallingPoint)-[:NEXT]->+
				        (l1b)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2a:CallingPoint)-[:NEXT]->+
				        (l2b)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})
				""",
				"""
				MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1a:CallingPoint)-[:NEXT]->+(l1b)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2a:CallingPoint)-[:NEXT]->+(l2b)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})"""),
			Arguments.of("""
				MATCH (:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(r:CallingPoint)
				(()-[:NEXT]->())+
				(:CallingPoint)-[:CALLS_AT]->(:Station {name: 'Gatwick Airport'})
				RETURN r.routeName AS route
				""",
				"""
					MATCH (:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(r:CallingPoint)
					(()-[:NEXT]->())+
					(:CallingPoint)-[:CALLS_AT]->(:Station {name: 'Gatwick Airport'})
					RETURN r.routeName AS route"""),
			Arguments.of("""
				MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1:CallingPoint)
				(()-[:NEXT]->())+
				(:CallingPoint)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2:CallingPoint)
				(()-[:NEXT]->())+
				(:CallingPoint)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})
				RETURN l1.routeName AS leg1, x.name AS changeAt, l2.routeName AS leg2
				""",
				"""
					MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1:CallingPoint)
					(()-[:NEXT]->())+
					(:CallingPoint)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2:CallingPoint)
					(()-[:NEXT]->())+
					(:CallingPoint)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})
					RETURN l1.routeName AS leg1, x.name AS changeAt, l2.routeName AS leg2
					"""),
			Arguments.of("""
				MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1a:CallingPoint)-[:NEXT]->+
				        (l1b)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2a:CallingPoint)-[:NEXT]->+
				        (l2b)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})
				MATCH (l1a)-[:HAS]->(s1:Stop)-[:NEXT]->+(s2)<-[:HAS]-(l1b)
				        WHERE time('09:30') < s1.departs < time('10:00')
				MATCH (l2a)-[:HAS]->(s3:Stop)-[:NEXT]->+(s4)<-[:HAS]-(l2b)
				        WHERE s2.arrives < s3.departs < s2.arrives + duration('PT15M')
				RETURN s1.departs AS leg1Departs, s2.arrives AS leg1Arrives, x.name AS changeAt,
				         s3.departs AS leg2Departs, s4.arrives AS leg2Arrive,
				         duration.between(s1.departs, s4.arrives).minutes AS journeyTime
				ORDER BY leg2Arrive\s
				LIMIT 5
				""", """
				MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1a:CallingPoint)-[:NEXT]->+(l1b)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2a:CallingPoint)-[:NEXT]->+(l2b)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})
				MATCH (l1a)-[:HAS]->(s1:Stop)-[:NEXT]->+(s2)<-[:HAS]-(l1b)
				WHERE (time('09:30') < s1.departs AND s1.departs < time('10:00'))
				MATCH (l2a)-[:HAS]->(s3:Stop)-[:NEXT]->+(s4)<-[:HAS]-(l2b)
				WHERE (s2.arrives < s3.departs AND s3.departs < (s2.arrives + duration('PT15M')))
				RETURN s1.departs AS leg1Departs, s2.arrives AS leg1Arrives, x.name AS changeAt,
				s3.departs AS leg2Departs, s4.arrives AS leg2Arrive,
				duration.between(s1.departs, s4.arrives).minutes AS journeyTime
				ORDER BY leg2Arrive ASC
				LIMIT 5
				"""),
			Arguments.of("""
				MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1a:CallingPoint)
				        (()-[:NEXT]->(n)\s
				          WHERE NOT EXISTS { (n)-[:CALLS_AT]->(:Station:LondonGroup) })+
				        (l1b)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2a:CallingPoint)
				        (()-[:NEXT]->(m)
				          WHERE NOT EXISTS { (m)-[:CALLS_AT]->(:Station:LondonGroup) })+
				        (l2b)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})
				MATCH (l1a)-[:HAS]->(s1:Stop)-[:NEXT]->+(s2)<-[:HAS]-(l1b)
				WHERE time('09:30') < s1.departs < time('10:00')
				MATCH (l2a)-[:HAS]->(s3:Stop)-[:NEXT]->+(s4)<-[:HAS]-(l2b)
				WHERE s2.arrives < s3.departs < s2.arrives + duration('PT15M')
				RETURN s1.departs AS leg1Departs, s2.arrives AS leg1Arrives, x.name AS changeAt,
				        s3.departs AS leg2Departs, s4.arrives AS leg2Arrive,
				        duration.between(s1.departs, s4.arrives).minutes AS journeyTime
				ORDER BY leg2Arrive\s
				LIMIT 5""",
				"""
				MATCH (dmk:Station {name: 'Denmark Hill'})<-[:CALLS_AT]-(l1a:CallingPoint)
				(()-[:NEXT]->(n)
				WHERE NOT (EXISTS { (n)-[:CALLS_AT]->(:Station:LondonGroup) }))+
				(l1b)-[:CALLS_AT]->(x:Station)<-[:CALLS_AT]-(l2a:CallingPoint)
				(()-[:NEXT]->(m)
				WHERE NOT (EXISTS { (m)-[:CALLS_AT]->(:Station:LondonGroup) }))+
				(l2b)-[:CALLS_AT]->(gtw:Station {name: 'Gatwick Airport'})
				MATCH (l1a)-[:HAS]->(s1:Stop)-[:NEXT]->+(s2)<-[:HAS]-(l1b)
				WHERE (time('09:30') < s1.departs AND s1.departs < time('10:00'))
				MATCH (l2a)-[:HAS]->(s3:Stop)-[:NEXT]->+(s4)<-[:HAS]-(l2b)
				WHERE (s2.arrives < s3.departs AND s3.departs < (s2.arrives + duration('PT15M')))
				RETURN s1.departs AS leg1Departs, s2.arrives AS leg1Arrives, x.name AS changeAt,
				s3.departs AS leg2Departs, s4.arrives AS leg2Arrive,
				duration.between(s1.departs, s4.arrives).minutes AS journeyTime
				ORDER BY leg2Arrive ASC
				LIMIT 5""")
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
		var cypher = renderer.render(CypherParser.parse(input));
		assertThat(cypher).isEqualTo(expected.replace("\n", " ").trim());
		assertThat(renderer.render(CypherParser.parse(cypher))).isEqualTo(cypher);
	}
}
