/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import java.time.Duration;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * @author Michael J. Simons
 */
class DurationLiteralTest {

	static Stream<Arguments> asStringShouldWork() {
		return Stream.of(
			Arguments.of(Duration.ofDays(1), "duration('P1D')"),
			Arguments.of(Duration.ofHours(1), "duration('PT1H')"),
			Arguments.of(Duration.ofMinutes(61), "duration('PT1H1M')"),
			Arguments.of(Duration.ofSeconds(61), "duration('PT1M1S')"),
			Arguments.of(Duration.ofSeconds(61).plusMillis(1123), "duration('PT1M2.123S')"),
			Arguments.of(Duration.ofSeconds(61).plusNanos(1123), "duration('PT1M1.000001123S')"),
			Arguments.of(Duration.ofHours(23).plusMinutes(61).plusSeconds(120), "duration('P1DT3M')"),
			Arguments.of(
				Duration.ofDays(364)
					.plusHours(47)
					.plusMinutes(59)
					.plusSeconds(61)
					.plusMillis(1001), "duration('P366DT2.001S')")
		);
	}

	@ParameterizedTest
	@MethodSource
	void asStringShouldWork(Duration value, String expected) {

		var literal = DurationLiteral.of(value);
		assertThat(literal.asString()).isEqualTo(expected);
	}
}
