/*
 * Copyright (c) 2019-2020 "Neo4j,"
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

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 * @soundtrack Genesis - We Can't Dance
 */
public class SymbolicNameTest {

	@Nested
	class ResolvedSymbolicNames {

		@Test
		void equalsShouldWorkSameValue() {

			SymbolicName name1 = SymbolicName.of("a");
			SymbolicName name2 = name1;
			SymbolicName name3 = SymbolicName.of("a");

			assertThat(name1).isEqualTo(name2);
			assertThat(name1).isEqualTo(name3);
		}

		@Test
		void equalsShouldWorkDifferentValue() {

			SymbolicName name1 = SymbolicName.of("a");
			SymbolicName name2 = SymbolicName.of("b");

			assertThat(name1).isNotEqualTo(name2);
		}

		@Test
		void shouldNotEqualUnresolved() {

			SymbolicName name1 = SymbolicName.of("a");

			assertThat(name1).isNotEqualTo(SymbolicName.unresolved());
		}

		@Test
		void sameResolvedNamesShouldHaveSameHashCodes() {

			SymbolicName name1 = SymbolicName.of("a");
			SymbolicName name2 = SymbolicName.of("a");
			assertThat(name1).hasSameHashCodeAs(name2);
		}

		@Test
		void differentResolvedNamesShouldHaveDifferentHashCodes() {

			SymbolicName name1 = SymbolicName.of("a");
			SymbolicName name2 = SymbolicName.of("b");
			assertThat(name1.hashCode()).isNotEqualTo(name2.hashCode());
		}
	}

	@Nested
	class UnresolvedSymbolicNames {

		@Test
		void equalsShouldWorkSameValue() {

			SymbolicName name1 = SymbolicName.unresolved();
			SymbolicName name2 = name1;
			SymbolicName name3 = SymbolicName.unresolved();

			assertThat(name1).isEqualTo(name2);
			assertThat(name1).isNotEqualTo(name3);
			assertThat(name2).isNotEqualTo(name3);
		}

		@Test
		void shouldNotEqualResolved() {

			SymbolicName name1 = SymbolicName.unresolved();

			assertThat(name1).isNotEqualTo(SymbolicName.of("a"));
		}

		@Test
		void differentUnresolvedNamesShouldHaveDifferentHashCodes() {

			SymbolicName name1 = SymbolicName.unresolved();
			SymbolicName name2 = SymbolicName.unresolved();
			assertThat(name1.hashCode()).isNotEqualTo(name2.hashCode());
		}
	}
}
