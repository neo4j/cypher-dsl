/*
 * Copyright (c) 2019-2021 "Neo4j,"
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import org.junit.jupiter.api.Test;

/**
 * Generates a raw cypher literal. The factory method is able to replace {@code $E} placeholders with expressions passed
 * to it. To use a {@literal $E} escape it as {$literal \$E}.
 *
 * @author Michael J. Simons
 * @soundtrack Foo Fighters - Echoes, Silence, Patience &amp; Grace
 */
class RawLiteralTest {

	@Test
	void shouldWorkWithoutPlaceHolder() {

		String cypher = Cypher.returning(Cypher.raw("1 * 2").as("result")).build().getCypher();
		assertThat(cypher).isEqualTo("RETURN 1 * 2 AS result");
	}

	@Test
	void shouldUnescapeEscapedPlaceholders() {

		String cypher = Cypher.returning(Cypher.raw("\\$E * \\$E").as("result")).build().getCypher();
		assertThat(cypher).isEqualTo("RETURN $E * $E AS result");
	}

	@Test
	void noArguments() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.raw("$E + 23 * $E"))
			.withMessageStartingWith("Too few arguments for the raw literal format `");
	}

	@Test
	void tooFewArguments() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.raw("$E + 23 * $E", Cypher.literalOf(1)))
			.withMessageStartingWith("Too few arguments for the raw literal format `");
	}

	@Test
	void tooManyArguments() {

		assertThatIllegalArgumentException()
			.isThrownBy(() -> Cypher.raw("$E + 23 * $E", Cypher.literalOf(1), Cypher.literalOf(2), Cypher.literalOf(3)))
			.withMessageStartingWith("Too many arguments for the raw literal format `");
	}

	@Test
	void mixedArguments() {

		String cypher = Cypher.returning(Cypher.raw("($E + $E) + \\$E", Cypher.parameter("summand1"), 2).as("result"))
			.build().getCypher();
		assertThat(cypher).isEqualTo("RETURN ($summand1 + 2) + $E AS result");
	}

	@Test
	void stringLiteral() {

		String cypher = Cypher.returning(Cypher.raw("size($E)", "test").as("result"))
			.build().getCypher();
		assertThat(cypher).isEqualTo("RETURN size('test') AS result");
	}

	@Test
	void onlyExpressionArguments() {

		String cypher = Cypher.returning(Cypher.raw("$E + $E", Cypher.literalOf(1), Cypher.literalOf(2)).as("result"))
			.build().getCypher();
		assertThat(cypher).isEqualTo("RETURN 1 + 2 AS result");
	}
}
