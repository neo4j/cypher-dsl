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

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class QPPTest {


	@ParameterizedTest
	@CsvSource(delimiterString = "%%", textBlock = """
		MATCH (n:(TrainStation&BusStation)|StationGroup)      %% MATCH (n:`TrainStation`&`BusStation`|`StationGroup`)
		MATCH (n:Station WHERE n.name STARTS WITH 'Preston')  %% MATCH (n:`Station` WHERE n.name STARTS WITH 'Preston')
		MATCH (n)-[{ distance: 0.24, duration: 'PT4M' }]->(m) %% MATCH (n)-[ {distance: 0.24, duration: 'PT4M'}]->(m)
		MATCH (n)-[r WHERE time() + duration(r.duration) < time('22:00') ]->(m) %% MATCH (n)-[r WHERE (time() + duration(r.duration)) < time('22:00')]->(m)
		""")
	void parsingAndRenderingOfQPPShouldWork(String input, String expected) {
		System.out.println(CypherParser.parse(input).getCypher());
		Assertions.assertThat(CypherParser.parse(input).getCypher()).isEqualTo(expected.trim());
	}
}
