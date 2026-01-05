/*
 * Copyright (c) 2019-2026 "Neo4j,"
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
package org.neo4j.cypherdsl.core.internal;

import java.net.URI;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
class LoadCSVTests {

	@Test
	void witherShouldWorkWithNullsOnBothEnds() {

		LoadCSV a = new LoadCSV(URI.create("file:///f.csv"), true, "a");
		LoadCSV b = a.withFieldTerminator(null);
		assertThat(a).isSameAs(b);
	}

	@Test
	void witherShouldWorkWithEmptyValues() {

		LoadCSV a = new LoadCSV(URI.create("file:///f.csv"), true, "a");
		LoadCSV b = a.withFieldTerminator("  \t");
		assertThat(a).isSameAs(b);
		assertThat(a.getFieldTerminator()).isNull();
	}

	@Test
	void witherShouldWorkWithNonEmptyValues() {

		LoadCSV a = new LoadCSV(URI.create("file:///f.csv"), true, "a");
		LoadCSV b = a.withFieldTerminator(";");
		assertThat(a).isNotSameAs(b);
		assertThat(a.getFieldTerminator()).isNull();
		assertThat(b.getFieldTerminator()).isEqualTo(";");
	}

}
