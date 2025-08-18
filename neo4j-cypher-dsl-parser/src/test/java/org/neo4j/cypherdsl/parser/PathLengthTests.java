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

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
class PathLengthTests {

	@ParameterizedTest
	@CsvSource(nullValues = "N/A", value = { "N/A, N/A", "N/A, 5", "5, N/A", "5, 10" })
	void length1(String minimum, String maximimum) {
		PathLength pathLength = PathLength.of(minimum, maximimum);
		if (minimum == null && maximimum == null) {
			assertThat(pathLength.isUnbounded()).isTrue();
		}

		if (minimum != null) {
			assertThat(pathLength.getMinimum()).isEqualTo(Integer.parseInt(minimum));
		}

		if (maximimum != null) {
			assertThat(pathLength.getMaximum()).isEqualTo(Integer.parseInt(maximimum));
		}
	}

}
