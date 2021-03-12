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

import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledIf;

/**
 * @author Michael J. Simons
 */
@DisabledIf("requiredClassesAreMissing")
class ForeignAdapterFactoryTest {

	static boolean requiredClassesAreMissing() {
		try {
			Class.forName("com.querydsl.core.types.Expression");
		} catch (ClassNotFoundException e) {
			return true;
		}
		return false;
	}

	@Test
	void shouldNotAdaptNull() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.adapt(null))
			.withMessage("Cannot adapt literal NULL expressions.");
	}

	@Test
	void shouldFailOnUnadaptableThings() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.adapt(23))
			.withMessageMatching("Cannot adapt expressions of .+ to Cypher-DSL expressions\\.");
	}
}
