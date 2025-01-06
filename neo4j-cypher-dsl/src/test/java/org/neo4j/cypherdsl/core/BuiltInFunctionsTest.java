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

import java.util.Locale;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * @author Michael J. Simons
 * @soundtrack Emigrate - Silent So Long
 */
class BuiltInFunctionsTest {

	@Nested
	class MathematicalFunctionsTest {

		/**
		 * Sanity check that we didn't miss up the names. For the time being, the constants
		 * are equal to the actual implementation names.
		 */
		@ParameterizedTest
		@EnumSource(BuiltInFunctions.MathematicalFunctions.class)
		void functionNameSanityCheck(BuiltInFunctions.MathematicalFunctions function) {

			Assertions.assertEquals(function.getImplementationName(), function.name().toLowerCase(Locale.ROOT));
		}
	}
}
