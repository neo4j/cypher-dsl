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
class InternalPropertyImplTests {

	@Test
	void simplePropertyGetNameShouldWork() {

		assertThat(InternalPropertyImpl.create(SymbolicName.unresolved(), "simple").getName()).isEqualTo("simple");
	}

	@Test
	void multiplePropertyGetNameShouldWork() {

		assertThat(InternalPropertyImpl.create(SymbolicName.unresolved(), "foo", "bar").getName()).isEqualTo("foo.bar");
	}

	@Test
	void externalReferenceHasPriority() {

		assertThat(InternalPropertyImpl.create(SymbolicName.unresolved(), "foo", "bar")
			.referencedAs("something")
			.getName()).isEqualTo("something");
	}

}
