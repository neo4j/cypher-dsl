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
package org.neo4j.cypherdsl.parser;

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.SortItem;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A value object containing the necessary pieces for creating a
 * {@link org.neo4j.cypherdsl.core.Return RETURN clause}. One possible producer after
 * fiddling with the elements is
 * {@link org.neo4j.cypherdsl.core.Clauses#returning(boolean, List, List, Expression, Expression)}.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class ReturnDefinition {

	private final boolean distinct;

	private final List<Expression> expressions;

	private final List<SortItem> optionalSortItems;

	private final Expression optionalSkip;

	private final Expression optionalLimit;

	ReturnDefinition(boolean distinct, List<Expression> expressions, List<SortItem> optionalSortItems,
			Expression optionalSkip, Expression optionalLimit) {
		this.distinct = distinct;
		this.expressions = expressions;
		this.optionalSortItems = optionalSortItems;
		this.optionalSkip = optionalSkip;
		this.optionalLimit = optionalLimit;
	}

	/**
	 * Returns <code>true</code> to indicate this return clause should return only
	 * distinct elements.
	 * @return <code>true</code> if the <code>DISTINCT</code> clause is present
	 */
	public boolean isDistinct() {
		return this.distinct;
	}

	/**
	 * {@return list of expressions to be returned}
	 */
	public List<Expression> getExpressions() {
		return this.expressions;
	}

	/**
	 * {@return list of expressions to sort the result by}
	 */
	public List<SortItem> getOptionalSortItems() {
		return this.optionalSortItems;
	}

	/**
	 * {@return numerical expression how many items to skip}
	 */
	public Expression getOptionalSkip() {
		return this.optionalSkip;
	}

	/**
	 * {@return numerical expression how many items to return}
	 */
	public Expression getOptionalLimit() {
		return this.optionalLimit;
	}

}
