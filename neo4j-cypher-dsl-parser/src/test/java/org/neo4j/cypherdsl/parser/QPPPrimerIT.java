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

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;
import org.neo4j.harness.Neo4j;
import org.neo4j.harness.Neo4jBuilders;

import com.opencsv.CSVReaderBuilder;
import com.opencsv.exceptions.CsvValidationException;

/**
 * @author Michael J. Simons
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class QPPPrimerIT {
	private static final Renderer RENDERER = Renderer.getRenderer(
		Configuration.newConfig().alwaysEscapeNames(false).build());

	private final Neo4j neo4j = Neo4jBuilders.newInProcessBuilder()
		.withDisabledServer()
		.withFixture("""
			CREATE (n1:Station {name: 'Denmark Hill'}),
			(n5:Station {name: 'Battersea Park'}),
			(n6:Station {name: 'Wandsworth Road'}),
			(n15:Station {name: 'Clapham High Street'}),
			(n16:Station {name: 'Peckham Rye'}),
			(n17:Station {name: 'Brixton'}),
			(n14:Station {name: 'London Victoria'}),
			(n18:Station {name: 'Clapham Junction'}),
			(p10:Stop {departs: time('22:37'), arrives: time('22:36')}),
			(p0:Stop {departs: time('22:41'), arrives: time('22:41')}),
			(p2:Stop {departs: time('22:43'), arrives: time('22:43')}),
			(p17:Stop {arrives: time('22:50'), departs: time('22:50')}),
			(p18:Stop {arrives: time('22:46'), departs: time('22:46')}),
			(p19:Stop {departs: time('22:33'), arrives: time('22:31')}),
			(p21:Stop {arrives: time('22:55')}),
			(p20:Stop {departs: time('22:44'), arrives: time('22:43')}),
			(p22:Stop {arrives: time('22:55')}),
			(p23:Stop {arrives: time('22:48')}),
			(n15)-[:LINK {distance: 1.96}]->(n1)-[:LINK {distance: 0.86}]->(n16),
			(n15)-[:LINK {distance: 0.39}]->(n6)<-[:LINK {distance: 0.7}]-(n5)-[:LINK {distance: 1.24}]->(n14), (n5)-[:LINK {distance: 1.45}]->(n18),
			(n14)<-[:LINK {distance: 3.18}]-(n17)-[:LINK {distance: 1.11}]->(n1),
			(p2)-[:CALLS_AT]->(n6), (p17)-[:CALLS_AT]->(n5), (p19)-[:CALLS_AT]->(n16),
			(p22)-[:CALLS_AT]->(n14), (p18)-[:CALLS_AT]->(n18), (p0)-[:CALLS_AT]->(n15), (p23)-[:CALLS_AT]->(n5), (p20)-[:CALLS_AT]->(n1),
			(p21)-[:CALLS_AT]->(n14), (p10)-[:CALLS_AT]->(n1), (p19)-[:NEXT]->(p10)-[:NEXT]->(p0)-[:NEXT]->(p2)-[:NEXT]->(p23),
			(p22)<-[:NEXT]-(p17)<-[:NEXT]-(p18), (p21)<-[:NEXT]-(p20)
			""")
		.build();

	@AfterAll
	void shutdownNeo4j() {
		neo4j.close();
	}

	static Stream<Arguments> primerStatementsShouldBeParsedAndProduceCorrectResults()
		throws CsvValidationException, IOException {

		List<Arguments> result = new ArrayList<>();
		try (var csvReader = new CSVReaderBuilder(
			new InputStreamReader(
				Objects.requireNonNull(
					CypherChallengeTest.class.getResourceAsStream("/qpp-primer.csv")))).withSkipLines(1)
			.build()) {
			String[] nextRecord;

			while ((nextRecord = csvReader.readNext()) != null) {
				result.add(Arguments.of(
					nextRecord[0],
					nextRecord[1].trim().replaceAll("\\s{2,}", " "),
					Integer.parseInt(nextRecord[2]),
					Boolean.parseBoolean(nextRecord[3]))
				);
			}
		}
		return result.stream();
	}

	@ParameterizedTest
	@MethodSource
	void primerStatementsShouldBeParsedAndProduceCorrectResults(String in, String expected, int expectedNumResults,
		boolean isCountQuery) {
		var cypher = RENDERER.render(CypherParser.parse(in));

		assertThat(cypher).isEqualTo(expected);
		assertThat(RENDERER.render(CypherParser.parse(cypher))).isEqualTo(cypher);
		try (var tx = neo4j.defaultDatabaseService().beginTx();
			var result = tx.execute(cypher)
		) {
			if (isCountQuery) {
				var cnt = (long) result.next().get(result.columns().get(0));
				assertThat(cnt).isEqualTo(expectedNumResults);
			} else {
				assertThat(result.stream().count()).isEqualTo(expectedNumResults);
			}
		}
	}
}
