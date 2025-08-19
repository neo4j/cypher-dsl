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

import java.util.function.UnaryOperator;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
class ConfigurationTests {

	@Test // GH-829
	void fieldNameGeneratorShouldBeConfigurable() {
		Configuration configuration = Configuration.newConfig().withFieldNameGenerator(name -> "Wurst" + name).build();
		assertThat(configuration.getConstantFieldNameGenerator().generate("salat")).isEqualTo("Wurstsalat");
	}

	@Nested
	class TypeNameDecoratorTest {

		@Test
		void typeNameDecoratorShouldDefaultToIdentity() {
			Configuration configuration = Configuration.newConfig().withSuffix(null).build();
			assertThat(configuration.getTypeNameDecorator()).isSameAs(UnaryOperator.identity());
		}

		@Test
		void prefixShouldBeApplied() {
			Configuration configuration = Configuration.newConfig().withPrefix("p").build();
			assertThat(configuration.getTypeNameDecorator().apply("v")).isEqualTo("pv_");
		}

		@Test
		void suffixShouldBeNullable() {
			Configuration configuration = Configuration.newConfig().withPrefix("p").withSuffix(null).build();
			assertThat(configuration.getTypeNameDecorator().apply("v")).isEqualTo("pv");
		}

		@Test
		void suffixShouldBeApplied() {
			Configuration configuration = Configuration.newConfig().withSuffix("s").build();
			assertThat(configuration.getTypeNameDecorator().apply("v")).isEqualTo("vs");
		}

	}

}
