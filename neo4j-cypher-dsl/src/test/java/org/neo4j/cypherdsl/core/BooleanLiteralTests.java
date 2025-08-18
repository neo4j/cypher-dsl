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

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
class BooleanLiteralTests {

	@ParameterizedTest
	@CsvSource(nullValues = "n/a", value = { "n/a,false", "false,false", "true,true" })
	void valueOfShouldWork(Boolean input, boolean expected) {
		Literal<Boolean> literal = BooleanLiteral.of(input);
		assertThat(literal).isInstanceOf(BooleanLiteral.class);
		BooleanLiteral booleanLiteral = (BooleanLiteral) literal;
		if (expected) {
			assertThat(booleanLiteral).isEqualTo(BooleanLiteral.TRUE);
		}
		else {
			assertThat(booleanLiteral).isEqualTo(BooleanLiteral.FALSE);
		}
	}

}
