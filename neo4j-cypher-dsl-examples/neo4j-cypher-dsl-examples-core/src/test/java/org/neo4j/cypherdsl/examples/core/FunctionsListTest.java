/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
package org.neo4j.cypherdsl.examples.core;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
// tag::functions-list-range-imports[]
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Functions;
// end::functions-list-range-imports[]
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 * @soundtrack Danger Dan - Dinkelbrot & Ölsardinen
 */
class FunctionsListTest {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	// tag::functions-list-operator[]
	@Test
	void valueAtExample() {

		// tag::functions-list-range[]
		var range = Functions.range(0, 10);
		// end::functions-list-range[]

		var statement = Cypher.returning(Cypher.valueAt(range, 3)).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN range(0, 10)[3]");
	}

	@Test
	void subListUntilExample() {

		var range = Functions.range(Cypher.literalOf(0), Cypher.literalOf(10));

		var statement = Cypher.returning(Cypher.subListUntil(range, 3)).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN range(0, 10)[..3]");
	}

	@Test
	void subListFromExample() {

		// tag::functions-list-range-step[]
		var range = Functions.range(0, 10, 1);
		// end::functions-list-range-step[]

		var statement = Cypher.returning(Cypher.subListFrom(range, -3)).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN range(0, 10, 1)[-3..]");
	}

	@Test
	void subListExample() {

		var range = Functions.range(Cypher.literalOf(0), Cypher.literalOf(10), Cypher.literalOf(1));

		var statement = Cypher.returning(Cypher.subList(range, 2, 4)).build();
		assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN range(0, 10, 1)[2..4]");
	}
	// end::functions-list-operator[]
}
