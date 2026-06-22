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
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * See <a href=
 * "https://neo4j.com/docs/cypher-manual/current/values-and-types/vector/">VECTOR</a>. The
 * API is very much aligned with the Neo4j-JDBC driver.
 *
 * @author Michael J. Simons
 * @since 2025.3.0
 */
public final class VectorLiteral implements Literal<List<? extends Number>> {

	private static final String MSG_NULL_CHECK = "Vector elements must not be literal null";

	/**
	 * The maximum size of a vector property supported by Neo4j as of Neo4j 2026.05.
	 */
	private static final int MAX_VECTOR_SIZE = 4096;

	private static void assertSize(int size) {

		if (size <= 0 || (size > MAX_VECTOR_SIZE)) {
			throw new IllegalArgumentException(
					"'%d' is not a valid value. Must be a %s in the range %d to %d (GQL 42N31)".formatted(size,
							"number", 1, MAX_VECTOR_SIZE));
		}
	}

	/**
	 * Creates a vector composed of {@link ElementType#INTEGER8}.
	 * @param elements the elements for the new vector
	 * @return a new {@link VectorLiteral}
	 */
	static VectorLiteral of(byte[] elements) {
		assertSize(Objects.requireNonNull(elements, MSG_NULL_CHECK).length);
		var content = new ArrayList<Byte>();
		for (var element : elements) {
			content.add(element);
		}
		return new VectorLiteral(ElementType.INTEGER8, content);
	}

	/**
	 * Creates a vector composed of {@link ElementType#INTEGER16}.
	 * @param elements the elements for the new vector
	 * @return a new {@link VectorLiteral}
	 */
	static VectorLiteral of(short[] elements) {
		assertSize(Objects.requireNonNull(elements, MSG_NULL_CHECK).length);
		var content = new ArrayList<Short>();
		for (var element : elements) {
			content.add(element);
		}
		return new VectorLiteral(ElementType.INTEGER16, content);
	}

	/**
	 * Creates a vector composed of {@link ElementType#INTEGER32}.
	 * @param elements the elements for the new vector
	 * @return a new {@link VectorLiteral}
	 */
	static VectorLiteral of(int[] elements) {
		assertSize(Objects.requireNonNull(elements, MSG_NULL_CHECK).length);
		return new VectorLiteral(ElementType.INTEGER32, Arrays.stream(elements).boxed().toList());
	}

	/**
	 * Creates a vector composed of {@link ElementType#INTEGER}.
	 * @param elements the elements for the new vector
	 * @return a new {@link VectorLiteral}
	 */
	static VectorLiteral of(long[] elements) {
		assertSize(Objects.requireNonNull(elements, MSG_NULL_CHECK).length);
		return new VectorLiteral(ElementType.INTEGER, Arrays.stream(elements).boxed().toList());
	}

	/**
	 * Creates a vector composed of {@link ElementType#FLOAT32}.
	 * @param elements the elements for the new vector
	 * @return a new {@link VectorLiteral}
	 */
	static VectorLiteral of(float[] elements) {
		assertSize(Objects.requireNonNull(elements, MSG_NULL_CHECK).length);
		var content = new ArrayList<Float>();
		for (var element : elements) {
			content.add(element);
		}
		return new VectorLiteral(ElementType.FLOAT32, content);
	}

	/**
	 * Creates a vector composed of {@link ElementType#FLOAT}.
	 * @param elements the elements for the new vector
	 * @return a new {@link VectorLiteral}
	 */
	static VectorLiteral of(double[] elements) {
		assertSize(Objects.requireNonNull(elements, MSG_NULL_CHECK).length);
		return new VectorLiteral(ElementType.FLOAT, Arrays.stream(elements).boxed().toList());
	}

	private final ElementType type;

	private final List<? extends Number> content;

	private VectorLiteral(ElementType type, List<? extends Number> content) {
		this.type = type;
		this.content = content;
	}

	@Override
	public String asString() {
		var value = this.content.stream().map(Number::toString).collect(Collectors.joining(", ", "[", "]"));
		return "vector(%s, %d, %s NOT NULL)".formatted(value, this.content.size(), this.type);
	}

	@Override
	public List<? extends Number> getContent() {
		return this.content;
	}

	private enum ElementType {

		/**
		 * Neo4j INTEGER8 type.
		 */
		INTEGER8,
		/**
		 * Neo4j INTEGER16 type.
		 */
		INTEGER16,
		/**
		 * Neo4j INTEGER32 type.
		 */
		INTEGER32,
		/**
		 * Neo4j INTEGER64 type (Cypher CIP-200 normalises INTEGER64 to INTEGER, and we
		 * want the Cypher-DSL to be aligned here).
		 */
		INTEGER,
		/**
		 * Neo4j FLOAT32 type.
		 */
		FLOAT32,
		/**
		 * Neo4j FLOAT64 type (Cypher CIP-200 normalises FLOAT64 to FLOAT, and we want the
		 * Cypher-DSL to be aligned here).
		 */
		FLOAT

	}

}
