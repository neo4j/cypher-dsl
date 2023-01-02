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
package org.neo4j.cypherdsl.codegen.core;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * @author Michael J. Simons
 */
class RelationshipNameGeneratorTest {

	private final RelationshipNameGenerator defaultStrategy = new RelationshipNameGenerator();

	static Stream<Arguments> suggestedAndEscapedNames() {
		Stream.Builder<Arguments> arguments = Stream.builder();
		for (String pair : new String[] {
			"BOR_BOR_B, BorBorB", "BAZ_BOR, BazBor", "aName, AName", "ANumberedNam3, ANumberedNam3", "Foo3Bar, Foo3Bar",
			"Foo3BaR, Foo3BaR",
			"foo3BaR, Foo3BaR", "🖖someThing, SomeThing", "$someThing, $someThing",
			"$$some33Thing, $$some33Thing", "🧐someThing✋x, SomeThingx", "🧐someThing✋✋X, SomeThingX"
		}) {
			String[] suggestedAndExpectedName = pair.split(",");
			arguments.add(Arguments.of(suggestedAndExpectedName[0].trim(), suggestedAndExpectedName[1].trim()));
		}
		return arguments.build();
	}

	@ParameterizedTest
	@MethodSource("suggestedAndEscapedNames")
	void createTypeNameShouldWork(String name, String expectedEscapedName) {

		assertThat(defaultStrategy.generate(name)).isEqualTo(expectedEscapedName);
	}
}
