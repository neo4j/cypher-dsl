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

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatNullPointerException;

class VectorTests {

	@Test
	void integer8ShouldCheckNull() {
		assertThatNullPointerException().isThrownBy(() -> VectorLiteral.of((byte[]) null))
			.withMessage("Vector elements must not be literal null");
	}

	@Test
	void integer8ShouldCheckMinSize() {
		var elements = new byte[0];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'0' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integer8ShouldCheckMaxSize() {
		var elements = new byte[5000];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'5000' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integer8ShouldWork() {
		assertThat(VectorLiteral.of(new byte[] { 1, 2, 3 }).asString())
			.isEqualTo("vector([1, 2, 3], 3, INTEGER8 NOT NULL)");
	}

	@Test
	void integer16ShouldCheckNull() {
		assertThatNullPointerException().isThrownBy(() -> VectorLiteral.of((short[]) null))
			.withMessage("Vector elements must not be literal null");
	}

	@Test
	void integer16ShouldCheckMinSize() {
		var elements = new short[0];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'0' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integer16ShouldCheckMaxSize() {
		var elements = new short[5000];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'5000' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integer16ShouldWork() {
		assertThat(VectorLiteral.of(new short[] { 1, 2, 3 }).asString())
			.isEqualTo("vector([1, 2, 3], 3, INTEGER16 NOT NULL)");
	}

	@Test
	void integer32ShouldCheckNull() {
		assertThatNullPointerException().isThrownBy(() -> VectorLiteral.of((int[]) null))
			.withMessage("Vector elements must not be literal null");
	}

	@Test
	void integer32ShouldCheckMinSize() {
		var elements = new int[0];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'0' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integer32ShouldCheckMaxSize() {
		var elements = new int[5000];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'5000' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integer32ShouldWork() {
		assertThat(VectorLiteral.of(new int[] { 1, 2, 3 }).asString())
			.isEqualTo("vector([1, 2, 3], 3, INTEGER32 NOT NULL)");
	}

	@Test
	void integerShouldCheckNull() {
		assertThatNullPointerException().isThrownBy(() -> VectorLiteral.of((long[]) null))
			.withMessage("Vector elements must not be literal null");
	}

	@Test
	void integerShouldCheckMinSize() {
		var elements = new long[0];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'0' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integerShouldCheckMaxSize() {
		var elements = new long[5000];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'5000' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void integerShouldWork() {
		assertThat(VectorLiteral.of(new long[] { 1, 2, 3 }).asString())
			.isEqualTo("vector([1, 2, 3], 3, INTEGER NOT NULL)");
	}

	@Test
	void float32ShouldCheckNull() {
		assertThatNullPointerException().isThrownBy(() -> VectorLiteral.of((float[]) null))
			.withMessage("Vector elements must not be literal null");
	}

	@Test
	void float32ShouldCheckMinSize() {
		var elements = new float[0];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'0' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void float32ShouldCheckMaxSize() {
		var elements = new float[5000];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'5000' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void float32ShouldWork() {
		assertThat(VectorLiteral.of(new float[] { 1, 2, 3 }).asString())
			.isEqualTo("vector([1.0, 2.0, 3.0], 3, FLOAT32 NOT NULL)");
	}

	@Test
	void floatShouldCheckNull() {
		assertThatNullPointerException().isThrownBy(() -> VectorLiteral.of((double[]) null))
			.withMessage("Vector elements must not be literal null");
	}

	@Test
	void floatShouldCheckMinSize() {
		var elements = new double[0];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'0' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void floatShouldCheckMaxSize() {
		var elements = new double[5000];
		assertThatIllegalArgumentException().isThrownBy(() -> VectorLiteral.of(elements))
			.withMessage("'5000' is not a valid value. Must be a number in the range 1 to 4096 (GQL 42N31)");
	}

	@Test
	void floatShouldWork() {
		assertThat(VectorLiteral.of(new double[] { 1, 2, 3 }).asString())
			.isEqualTo("vector([1.0, 2.0, 3.0], 3, FLOAT NOT NULL)");
	}

}
