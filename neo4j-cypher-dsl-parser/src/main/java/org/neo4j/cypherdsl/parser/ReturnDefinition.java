/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
