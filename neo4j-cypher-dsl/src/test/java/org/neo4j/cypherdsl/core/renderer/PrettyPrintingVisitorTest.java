/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
package org.neo4j.cypherdsl.core.renderer;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * @author Andreas Berger
 * @author Michael J. Simons
 */
class PrettyPrintingVisitorTest {

	private final PrettyPrintingVisitor prettyPrintingVisitor = new PrettyPrintingVisitor(null,
		false, Configuration.IndentStyle.TAB, 4);

	@ParameterizedTest
	@CsvSource({
		"ALabel, ALabel",
		"A Label, `A Label`",
		"A `Label, `A ``Label`",
		"`A `Label, ```A ``Label`",
		"Spring Data Neo4j⚡️RX, `Spring Data Neo4j⚡️RX`"
	})
	void shouldCorrectlyEscapeNames(String name, String expectedEscapedName) {

		assertThat(prettyPrintingVisitor.escapeName(name))
			.hasValue(expectedEscapedName);
	}

	@Test
	void shouldNotTryToEscapeNullNames() {

		assertThat(prettyPrintingVisitor.escapeName(null)).isEmpty();
	}

}
