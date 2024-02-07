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
package org.neo4j.cypherdsl.parser;

import static org.apiguardian.api.API.Status.STABLE;

import java.util.List;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.SortItem;

/**
 * A value object containing the necessary pieces for creating a {@link org.neo4j.cypherdsl.core.Return RETURN clause}.
 * One possible producer after fiddling with the elements is {@link org.neo4j.cypherdsl.core.Clauses#returning(boolean, List, List, Expression, Expression)}.
 *
 * @author Michael J. Simons
 * @soundtrack Various - Just The Best 90s
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class ReturnDefinition {

	private final boolean distinct;
	private final List<Expression> expressions;
	private final @Nullable List<SortItem> optionalSortItems;
	private final @Nullable Expression optionalSkip;
	private final @Nullable Expression optionalLimit;

	ReturnDefinition(boolean distinct, List<Expression> expressions,
		@Nullable List<SortItem> optionalSortItems,
		@Nullable Expression optionalSkip,
		@Nullable Expression optionalLimit) {
		this.distinct = distinct;
		this.expressions = expressions;
		this.optionalSortItems = optionalSortItems;
		this.optionalSkip = optionalSkip;
		this.optionalLimit = optionalLimit;
	}

	/**
	 * @return {@literal true} to indicate this return clause should return only distinct elements
	 */
	public boolean isDistinct() {
		return distinct;
	}

	/**
	 * @return List of expressions to be returned.
	 */
	public List<Expression> getExpressions() {
		return expressions;
	}

	/**
	 * @return List of expressions to sort the result by.
	 */
	public List<SortItem> getOptionalSortItems() {
		return optionalSortItems;
	}

	/**
	 * @return Numerical expression how many items to skip
	 */
	public Expression getOptionalSkip() {
		return optionalSkip;
	}

	/**
	 * @return Numerical expression how many items to return
	 */
	public Expression getOptionalLimit() {
		return optionalLimit;
	}
}
