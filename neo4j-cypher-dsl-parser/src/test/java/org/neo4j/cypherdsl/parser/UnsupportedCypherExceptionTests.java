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

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
class UnsupportedCypherExceptionTests {

	@Test
	void causeShouldBePresent() {
		var cause = new UnsupportedOperationException("booms");
		var unsupportedCypherException = new UnsupportedCypherException("This is invalid cypher", cause);
		assertThat(unsupportedCypherException.getCause()).isEqualTo(cause);
	}

	@Test
	void inputShouldBeRetrievable() {
		var cause = new UnsupportedOperationException("booms");
		var input = "This is invalid cypher";
		var unsupportedCypherException = new UnsupportedCypherException(input, cause);
		assertThat(unsupportedCypherException.getInput()).isEqualTo(input);
	}

	@Test
	void messageShouldBeFormatted() {
		var cause = new UnsupportedOperationException("booms");
		var input = "This is invalid cypher";
		var unsupportedCypherException = new UnsupportedCypherException(input, cause);

		assertThat(unsupportedCypherException.getMessage()).isEqualTo(
				"""
						You used one Cypher construct not yet supported by the Cypher-DSL:

						\tThis is invalid cypher

						Feel free to open an issue so that we might add support for it at https://github.com/neo4j-contrib/cypher-dsl/issues/new""");
	}

}
