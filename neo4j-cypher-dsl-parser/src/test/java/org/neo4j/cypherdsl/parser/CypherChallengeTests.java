/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.opencsv.CSVReaderBuilder;
import com.opencsv.exceptions.CsvValidationException;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * See <a href=
 * "https://github.com/tomasonjo/cypher-direction-competition">cypher-direction-competition</a>.
 *
 * @author Michael J. Simons
 */
class CypherChallengeTests {

	static Stream<Arguments> cypherFixRelChallenge() throws IOException, CsvValidationException {

		List<Arguments> result = new ArrayList<>();
		try (var csvReader = new CSVReaderBuilder(new InputStreamReader(
				Objects.requireNonNull(CypherChallengeTests.class.getResourceAsStream("/cypher-challenge.csv"))))
			.withSkipLines(1)
			.build()) {
			String[] nextRecord;

			var p = Pattern.compile("\\(.*?\\)");
			while ((nextRecord = csvReader.readNext()) != null) {

				var builder = Configuration.newConfig();
				var m = p.matcher(nextRecord[1]);
				while (m.find()) {
					builder.withRelationshipDefinition(Configuration.relationshipDefinition(m.group(0)));
				}

				result.add(Arguments.of(builder.withEnforceSchema(true).build(), nextRecord[0], nextRecord[2]));
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
