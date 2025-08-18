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
package org.neo4j.cypherdsl.core.renderer;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 *
 */
class ConfigurationTests {

	@Test
	void dialectShouldBeIncludedInEquals() {

		Configuration cfg0 = Configuration.newConfig().build();
		Configuration cfg1 = Configuration.newConfig().withDialect(Dialect.NEO4J_4).build();
		Configuration cfg2 = Configuration.newConfig().withDialect(Dialect.NEO4J_5).build();

		assertThat(cfg0).isEqualTo(cfg1).isNotEqualTo(cfg2);
	}

	@Test
	void dialectShouldBeIncludedInHash() {

		Configuration cfg0 = Configuration.newConfig().build();
		Configuration cfg1 = Configuration.newConfig().withDialect(Dialect.NEO4J_4).build();
		Configuration cfg2 = Configuration.newConfig().withDialect(Dialect.NEO4J_5).build();

		assertThat(cfg0).hasSameHashCodeAs(cfg1).doesNotHaveSameHashCodeAs(cfg2);
	}

}
