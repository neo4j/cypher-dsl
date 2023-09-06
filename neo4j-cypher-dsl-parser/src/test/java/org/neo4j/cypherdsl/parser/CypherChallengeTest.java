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

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import com.opencsv.CSVReaderBuilder;
import com.opencsv.exceptions.CsvValidationException;

/**
 * See <a href="https://github.com/tomasonjo/cypher-direction-competition">cypher-direction-competition</a>.
 *
 * @author Michael J. Simons
 */
class CypherChallengeTest {

	static Stream<Arguments> cypherFixRelChallenge() throws IOException, CsvValidationException {

		List<Arguments> result = new ArrayList<>();
		try (var csvReader = new CSVReaderBuilder(
			new InputStreamReader(
				Objects.requireNonNull(
					CypherChallengeTest.class.getResourceAsStream("/cypher-challenge.csv")))).withSkipLines(1)
			.build()) {
			String[] nextRecord;

			var p = Pattern.compile("\\(.*?\\)");
			while ((nextRecord = csvReader.readNext()) != null) {

				var builder = Configuration.newConfig();
				var m = p.matcher(nextRecord[1]);
				while (m.find()) {
					builder.withRelationshipDefinition(Configuration.relationshipDefinition(m.group(0)));
				}

				result.add(Arguments.of(
					builder.withEnforceSchema(true).build(),
					nextRecord[0],
					nextRecord[2]
				));
			}
		}

		return result.stream();
	}

	@ParameterizedTest(name = "{1}")
	@MethodSource
	void cypherFixRelChallenge(Configuration configuration, String input, String expected) {

		var statement = CypherParser.parse(input);
		var renderer = Renderer.getRenderer(configuration);
		var cypher = renderer.render(statement);
		var finalExpectation = expected.isBlank() ? "" : CypherParser.parse(expected).getCypher();
		assertThat(cypher).isEqualTo(finalExpectation);
	}
}
