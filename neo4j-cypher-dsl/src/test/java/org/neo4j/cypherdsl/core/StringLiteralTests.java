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
package org.neo4j.cypherdsl.core;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
class StringLiteralTests {

	@Test
	void shouldEscapeContent() {
		StringLiteral literal = new StringLiteral("A\\B\\\\Ca'bc123\\");

		assertThat(literal.asString()).isEqualTo("'A\\\\B\\\\\\\\Ca\\'bc123\\\\'");
	}

	@Test
	void shouldEscapeNull() {
		StringLiteral literal = new StringLiteral(null);

		assertThat(literal.asString()).isEqualTo("''");
	}

	@Test
	void shouldCorrectlyEscapeEmptyStrings() {

		for (String[] strings : new String[][] { { "", "" }, { " \t ", " \t " },
				{ "Nothing to escape", "Nothing to escape" }, { "' \" '", "\\' \\\" \\'" } }) {
			String string = strings[0];
			String expectedEscapedString = strings[1];
			assertThat(StringLiteral.escapeString(string)).hasValue(expectedEscapedString);
		}
	}

	@Test
	void shouldNotTryToEscapeNullStrings() {
		assertThat(StringLiteral.escapeString(null)).isEmpty();
	}

}
