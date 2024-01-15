/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.util.Collection;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 */
class DefaultStatementBuilderTest {

	@Test
	void matchPreconditionsShouldBeAsserted() {

		DefaultStatementBuilder builder = new DefaultStatementBuilder();
		assertThatIllegalArgumentException().isThrownBy(() -> builder.match((PatternElement[]) null))
			.withMessage("Patterns to match are required.");
		assertThatIllegalArgumentException().isThrownBy(() -> builder.match())
			.withMessage("At least one pattern to match is required.");
	}

	@Nested
	class MessageBundles {

		@Test
		void shouldUseMessageBundleOnNullExpressions() {
			DefaultStatementBuilder builder = new DefaultStatementBuilder();

			assertThatIllegalArgumentException().isThrownBy(() -> builder.returning((Collection<Expression>) null))
				.withMessage("Expressions to return are required.");

			assertThatIllegalArgumentException().isThrownBy(() -> builder.with((Collection<Expression>) null))
				.withMessage("Expressions to return are required.");

			StatementBuilder.BuildableMatchAndUpdate update = builder.set(Cypher.node("N").named("n"), "x");
			assertThatIllegalArgumentException().isThrownBy(() -> update.returning((Collection<Expression>) null))
				.withMessage("Expressions to return are required.");
		}

		@Test
		void shouldUseMessageBundleOnEmptyExpressions() {
			DefaultStatementBuilder builder = new DefaultStatementBuilder();

			assertThatIllegalArgumentException().isThrownBy(() -> builder.returning((Expression) null))
				.withMessage("At least one expressions to return is required.");

			assertThatIllegalArgumentException().isThrownBy(() -> builder.with((Expression) null))
				.withMessage("At least one expressions to return is required.");

			StatementBuilder.BuildableMatchAndUpdate update = builder.set(Cypher.node("N").named("n"), "x");
			assertThatIllegalArgumentException().isThrownBy(() -> update.returning((Expression) null))
				.withMessage("At least one expressions to return is required.");
		}
	}

	@Nested
	class ReturningPreconditionsShouldBeAsserted {

		@Test
		void forExpressions() {

			DefaultStatementBuilder builder = new DefaultStatementBuilder();
			assertThatIllegalArgumentException().isThrownBy(() -> builder.returning((Expression[]) null))
				.withMessage("Expressions to return are required.");
			assertThatIllegalArgumentException().isThrownBy(() -> builder.returning(new Expression[0]))
				.withMessage("At least one expressions to return is required.");
		}
	}
}
