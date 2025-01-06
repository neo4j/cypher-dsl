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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;

import java.util.Arrays;
import java.util.Collection;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;

/**
 * A step that exposes the {@code WITH} clause. This interface used to be part of the {@link StatementBuilder} and moved
 * out of it to unify the {@literal WITH} clause taking in {@link IdentifiableElement identifable elements}.
 *
 * @author Michael J. Simons
 * @since 2023.0.0
 */
@API(status = STABLE, since = "2023.0.0")
public interface ExposesWith {

	/**
	 * Starts a with clause by passing variables to it.
	 *
	 * @param variables The variables to pass on to the next part
	 * @return A match that can be build now
	 */
	@NotNull
	@CheckReturnValue
	@SuppressWarnings("deprecation")
	default OrderableOngoingReadingAndWithWithoutWhere with(String... variables) {
		return with(Expressions.createSymbolicNames(variables));
	}

	/**
	 * Create a match that returns one or more identifiable elements.
	 *
	 * @param elements The variables to pass on to the next part
	 * @return A match that can be build now
	 */
	@NotNull
	@CheckReturnValue
	default OrderableOngoingReadingAndWithWithoutWhere with(IdentifiableElement... elements) {
		return with(Arrays.asList(elements));
	}

	/**
	 * Create a match that returns one or more identifiable elements.
	 *
	 * @param elements The expressions to be returned. Must not be null and be at least one expression.
	 * @return A match that can be build now
	 */
	@NotNull
	@CheckReturnValue
	OrderableOngoingReadingAndWithWithoutWhere with(Collection<IdentifiableElement> elements);

	/**
	 * Create a match that returns the distinct set of one or more identifiable elements.
	 *
	 * @param variables The variables to pass on to the next part
	 * @return A match that can be build now
	 * @see #withDistinct(IdentifiableElement...)
	 */
	@NotNull
	@CheckReturnValue
	@SuppressWarnings("deprecation")
	default OrderableOngoingReadingAndWithWithoutWhere withDistinct(String... variables) {
		return withDistinct(Expressions.createSymbolicNames(variables));
	}

	/**
	 * Create a match that returns the distinct set of one or more identifiable elements.
	 *
	 * @param elements The variables to pass on to the next part
	 * @return A match that can be build now
	 * @see #withDistinct(IdentifiableElement...)
	 */
	@NotNull
	@CheckReturnValue
	default OrderableOngoingReadingAndWithWithoutWhere withDistinct(IdentifiableElement... elements) {
		return withDistinct(Arrays.asList(elements));
	}

	/**
	 * Create a match that returns the distinct set of one or more expressions.
	 *
	 * @param expressions The expressions to be returned. Must not be null and be at least one expression.
	 * @return A match that can be build now
	 */
	@NotNull
	@CheckReturnValue
	OrderableOngoingReadingAndWithWithoutWhere withDistinct(Collection<IdentifiableElement> expressions);
}
