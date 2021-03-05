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
package org.neo4j.cypherdsl.codegen.core;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * @author Michael J. Simons
 */
class ConstantFieldNamingStrategyTest {

	@ParameterizedTest
	@CsvSource({ "aName, A_NAME", "AName, A_NAME", "BAZ_BAR, BAZ_BAR", "aName, A_NAME",
		"ANumberedNam3, A_NUMBERED_NAM_3", "Foo3Bar, FOO_3_BAR", "Foo3BaR, FOO_3_BA_R",
		"foo3BaR, FOO_3_BA_R", "🖖someThing, SOME_THING", "$someThing, $_SOME_THING",
		"$$some33Thing, $_$_SOME_3_3_THING", "🧐someThing✋x, SOME_THING_X", "🧐someThing✋✋X, SOME_THING_X" })
	void toConstantFieldNameShouldWork(String name, String expectedEscapedName) {

		assertThat(FieldNameGenerator.Default.INSTANCE.generate(name)).isEqualTo(expectedEscapedName);
	}
}
