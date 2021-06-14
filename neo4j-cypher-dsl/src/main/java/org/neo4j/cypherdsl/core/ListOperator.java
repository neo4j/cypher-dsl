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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Optional;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.internal.LiteralBase;
import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Represents a range literal applied to another expression.
 *
 * @author Michael J. Simons
 * @soundtrack Danger Dan - Reflexionen aus dem beschönigten Leben
 * @since 2020.1.0
 */
@API(status = API.Status.EXPERIMENTAL, since = "2020.1.0")
public final class ListOperator implements Expression {

	/**
	 * A literal for the dots.
	 */
	private static final Literal<String> DOTS = new LiteralBase<String>("..") {
		@NotNull
		@Override
		public String asString() {
			return super.getContent();
		}
	};

	@API(status = INTERNAL, since = "1.0")
	static final class Details implements Visitable, ProvidesAffixes {

		/**
		 * An optional start for the range (inclusive if given).
		 */
		private final Expression optionalStart;

		/**
		 * Optional dots between the start and end.
		 */
		private final Literal<String> dots;

		/**
		 * An optional end for the range (exclusive if given).
		 */
		private final Expression optionalEnd;

		Details(Expression optionalStart, Literal<String> dots, Expression optionalEnd) {
			this.optionalStart = optionalStart;
			this.dots = dots;
			this.optionalEnd = optionalEnd;
		}

		@Override
		public void accept(Visitor visitor) {

			visitor.enter(this);
			Visitable.visitIfNotNull(this.optionalStart, visitor);
			Visitable.visitIfNotNull(this.dots, visitor);
			Visitable.visitIfNotNull(this.optionalEnd, visitor);
			visitor.leave(this);
		}

		@Override
		public Optional<String> getPrefix() {
			return Optional.of("[");
		}

		@Override
		public Optional<String> getSuffix() {
			return Optional.of("]");
		}
	}

	/**
	 * The target expression to which the literal should be applied.
	 */
	private final Expression targetExpression;

	/**
	 * The actual operator's details.
	 */
	private final Details details;

	/**
	 * Creates a closed range with given boundaries.
	 *
	 * @param targetExpression The target expression for the range
	 * @param start            The inclusive start
	 * @param end              The exclusive end
	 * @return A range literal.
	 */
	static ListOperator subList(Expression targetExpression, Expression start, Expression end) {

		Assertions.notNull(targetExpression, "The range's target expression must not be null.");
		Assertions.notNull(start, "The start of the range must not be null.");
		Assertions.notNull(end, "The end of the range must not be null.");

		return new ListOperator(targetExpression, start, DOTS, end);
	}

	/**
	 * Creates an open range starting at {@code start}.
	 *
	 * @param targetExpression The target expression for the range
	 * @param start            The inclusive start
	 * @return A range literal.
	 */
	static ListOperator subListFrom(Expression targetExpression, Expression start) {

		Assertions.notNull(targetExpression, "The range's target expression must not be null.");
		Assertions.notNull(start, "The start of the range must not be null.");

		return new ListOperator(targetExpression, start, DOTS, null);
	}

	/**
	 * Creates an open range starting at {@code start}.
	 *
	 * @param targetExpression The target expression for the range
	 * @param end              The exclusive end
	 * @return A range literal.
	 */
	static ListOperator subListUntil(Expression targetExpression, Expression end) {

		Assertions.notNull(targetExpression, "The range's target expression must not be null.");
		Assertions.notNull(end, "The end of the range must not be null.");

		return new ListOperator(targetExpression, null, DOTS, end);
	}

	/**
	 * Creates a single valued range at {@code index}.
	 *
	 * @param targetExpression The target expression for the range
	 * @param index            The index of the range
	 * @return A range literal.
	 */
	static ListOperator valueAt(Expression targetExpression, Expression index) {

		Assertions.notNull(targetExpression, "The range's target expression must not be null.");
		Assertions.notNull(index, "The index of the range must not be null.");

		return new ListOperator(targetExpression, index, null, null);
	}

	private ListOperator(Expression targetExpression, Expression optionalStart,
		Literal<String> dots, Expression optionalEnd) {

		this.targetExpression = targetExpression;
		this.details = new Details(optionalStart, dots, optionalEnd);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.targetExpression.accept(visitor);
		this.details.accept(visitor);
		visitor.leave(this);
	}
}
