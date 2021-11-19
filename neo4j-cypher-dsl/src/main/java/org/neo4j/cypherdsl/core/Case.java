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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.internal.CaseWhenThen;
import org.neo4j.cypherdsl.core.utils.CheckReturnValue;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/CaseExpression.html">CaseExpression</a>.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface Case extends Expression, ProvidesAffixes {

	/**
	 * Creates a new case/when expression with an additional {@code WHEN} block.
	 *
	 * @param nextExpression The next expression to use.
	 * @return An ongoing when builder.
	 */
	@NotNull @CheckReturnValue
	OngoingWhenThen when(Expression nextExpression);

	/**
	 * Extension the {@link Case} interface to support simple case with an initial expression / condition.
	 */
	@API(status = STABLE, since = "1.0")
	interface SimpleCase extends Case {
	}

	/**
	 * Extension of the {@link Case} interface to support generic case.
	 */
	@API(status = STABLE, since = "1.0")
	interface GenericCase extends Case {
	}

	static Case create(@Nullable Expression expression) {
		return expression == null ? new AbstractCase.GenericCaseImpl() : new AbstractCase.SimpleCaseImpl(expression);
	}

	/**
	 * Specification for a renderable, complete CASE statement
	 */
	@API(status = STABLE, since = "1.0")
	interface CaseEnding extends Case {

		/**
		 * Adds a new {@code WHEN} block.
		 *
		 * @param expression A new when expression.
		 * @return An ongoing when builder.
		 */
		@NotNull @CheckReturnValue
		OngoingWhenThen when(Expression expression);

		/**
		 * Ends this case expression with a default expression to evaluate.
		 *
		 * @param defaultExpression The new default expression
		 * @return An ongoing when builder.
		 */
		@NotNull @CheckReturnValue
		Case elseDefault(Expression defaultExpression);
	}

	/**
	 * Helper class to collect `when` expressions and create {@link CaseWhenThen} instances when the `then` is provided.
	 */
	@API(status = STABLE, since = "1.0")
	interface OngoingWhenThen {

		/**
		 * Ends this {@code WHEN} block with an expression.
		 *
		 * @param expression The expression for the ongoing {@code WHEN} block.
		 * @return An ongoing when builder.
		 */
		@NotNull @CheckReturnValue
		CaseEnding then(Expression expression);
	}
}
