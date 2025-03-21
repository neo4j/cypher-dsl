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

import java.util.Optional;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.utils.Assertions;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * A sort item can be used in an {@code ORDER BY} clause and changes the order of the items being returned from a query.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class SortItem implements Visitable {

	private final Expression expression;
	private final Direction direction;

	static SortItem create(Expression expression, Direction direction) {

		Assertions.notNull(expression, "Expression to sort must not be null.");
		return new SortItem(expression, Optional.ofNullable(direction).orElse(SortItem.Direction.UNDEFINED));
	}

	private SortItem(Expression expression, Direction direction) {
		this.expression = expression;
		this.direction = direction;
	}

	/**
	 * Creates a new sort item from {@literal this} instance, setting the sort direction to ascending.
	 * @return A new sort item.
	 */
	@NotNull @Contract(pure = true)
	public SortItem ascending() {
		return new SortItem(this.expression, Direction.ASC);
	}

	/**
	 * Creates a new sort item from {@literal this} instance, setting the sort direction to descending.
	 * @return A new sort item.
	 */
	@NotNull @Contract(pure = true)
	public SortItem descending() {
		return new SortItem(this.expression, Direction.DESC);
	}

	@Override
	@SuppressWarnings("deprecation")
	public void accept(Visitor visitor) {

		visitor.enter(this);
		Expressions.nameOrExpression(this.expression).accept(visitor);

		if (this.direction != Direction.UNDEFINED) {
			this.direction.accept(visitor);
		}
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	/**
	 * Sort direction.
	 * @since 1.0
	 */
	@API(status = STABLE)
	public enum Direction implements Visitable {
		/** Undefined direction. */
		UNDEFINED(""),
		/** Ascending order. */
		ASC("ASC"),
		/** Descending order. */
		DESC("DESC");

		private final String symbol;

		Direction(String symbol) {
			this.symbol = symbol;
		}

		/**
		 * @return The database internal symbol for a direction.
		 */
		public String getSymbol() {
			return this.symbol;
		}

		@Override
		public String toString() {
			return RendererBridge.render(this);
		}
	}
}
