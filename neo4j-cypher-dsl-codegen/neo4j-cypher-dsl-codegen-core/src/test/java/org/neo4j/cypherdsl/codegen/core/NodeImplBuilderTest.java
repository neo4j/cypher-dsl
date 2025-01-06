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
package org.neo4j.cypherdsl.codegen.core;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * @author Michael J. Simons
 * @soundtrack In Flames - I, The Mask
 */
class NodeImplBuilderTest {

	@ParameterizedTest
	@CsvSource(nullValues = "n/a", value = {
		"n/a,n/a",
		"foo, Foo",
		"fOo, FOo",
		"Foo, n/a"
	})
	void capitalizeShouldWork(String in, String expected) {

		String result = NodeImplBuilder.capitalize(in);
		if (in == null) {
			assertThat(result).isNull();
		} else if (expected == null) {
			assertThat(result).isSameAs(in);
		} else {
			assertThat(result).isEqualTo(expected);
		}
	}
}
