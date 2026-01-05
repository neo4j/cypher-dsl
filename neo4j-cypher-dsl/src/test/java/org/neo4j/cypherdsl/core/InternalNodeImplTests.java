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
package org.neo4j.cypherdsl.core;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Stream;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;

/**
 * This tests focus on the internal node implementation.
 *
 * @author Michael J. Simons
 */
class InternalNodeImplTests {

	@Test
	void preconditionsShouldBeAsserted() {
		String expectedMessage = "A primary label is required.";

		assertThatIllegalArgumentException().isThrownBy(() -> new InternalNodeImpl("")).withMessage(expectedMessage);
		assertThatIllegalArgumentException().isThrownBy(() -> new InternalNodeImpl(" \t")).withMessage(expectedMessage);
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.node("")).withMessage(expectedMessage);
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.node(" \t")).withMessage(expectedMessage);
	}

	@Test
	void shouldNotAddEmptyAdditionalLabels() {

		assertThatIllegalArgumentException().isThrownBy(() -> new InternalNodeImpl("primary", " ", "\t "))
			.withMessage("An empty label is not allowed.");
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.node("primary", " ", "\t "))
			.withMessage("An empty label is not allowed.");
	}

	@Test
	void shouldCreateNodes() {

		Node node = new InternalNodeImpl("primary", "secondary");
		List<String> labels = new ArrayList<>();
		node.accept(new Visitor() {
			@Override
			public void enter(Visitable segment) {

				if (segment instanceof NodeLabel) {
					labels.add(((NodeLabel) segment).getValue());
				}

			}
		});
		assertThat(labels).contains("primary", "secondary");
	}

	@Nested
	class MutationsAndSetShouldRequireName {

		@Test
		void mutationWithParameter() {

			Node node = Cypher.anyNode();
			Parameter<?> parameter = Cypher.parameter("x");
			assertThatIllegalStateException().isThrownBy(() -> node.mutate(parameter))
				.withMessage("A property container must be named to be mutated.");
		}

		@Test
		void mutationWithMap() {

			Node node = Cypher.anyNode();
			MapExpression mapExpression = Cypher.mapOf("a", Cypher.literalTrue());
			assertThatIllegalStateException().isThrownBy(() -> node.mutate(mapExpression))
				.withMessage("A property container must be named to be mutated.");
		}

		@Test
		void setWithParameter() {

			Node node = Cypher.anyNode();
			Parameter<?> parameter = Cypher.parameter("x");
			assertThatIllegalStateException().isThrownBy(() -> node.set(parameter))
				.withMessage("A property container must be named to be mutated.");
		}

		@Test
		void setWithMap() {

			Node node = Cypher.anyNode();
			MapExpression mapExpression = Cypher.mapOf("a", Cypher.literalTrue());
			assertThatIllegalStateException().isThrownBy(() -> node.set(mapExpression))
				.withMessage("A property container must be named to be mutated.");
		}

	}

	@Nested
	@TestInstance(Lifecycle.PER_CLASS)
	class PropertiesShouldBeHandled {

		private Stream<Arguments> createNodesWithProperties() {
			return Stream.of(
					Arguments.of(new InternalNodeImpl("N").named("n").withProperties("p", Cypher.literalTrue())),
					Arguments.of(new InternalNodeImpl("N").named("n")
						.withProperties(MapExpression.create(false, "p", Cypher.literalTrue()))));
		}

		@ParameterizedTest
		@MethodSource("createNodesWithProperties")
		void shouldAddProperties(Node node) {

			AtomicBoolean failTest = new AtomicBoolean(true);
			node.accept(new Visitor() {
				Class<?> expectedTypeOfNextSegment = null;

				@Override
				public void enter(Visitable segment) {
					if (segment instanceof SymbolicName) {
						assertThat(((SymbolicName) segment).getValue()).isEqualTo("n");
					}
					else if (segment instanceof NodeLabel) {
						assertThat(((NodeLabel) segment).getValue()).isEqualTo("N");
					}
					else if (segment instanceof KeyValueMapEntry) {
						assertThat(((KeyValueMapEntry) segment).getKey()).isEqualTo("p");
						this.expectedTypeOfNextSegment = BooleanLiteral.class;
					}
					else if (this.expectedTypeOfNextSegment != null) {
						assertThat(segment).isInstanceOf(this.expectedTypeOfNextSegment);
						failTest.getAndSet(false);
					}
				}
			});
			assertThat(failTest).isFalse();
		}

		@Test
		void shouldCreateProperty() {

			Node node = new InternalNodeImpl("N").named("n");
			Property property = node.property("p");

			java.util.Set<Object> expected = new HashSet<>();
			expected.addAll(property.getNames());
			expected.add(node.getRequiredSymbolicName());
			expected.add(property);

			property.accept(expected::remove);

			assertThat(expected).isEmpty();
		}

	}

}
