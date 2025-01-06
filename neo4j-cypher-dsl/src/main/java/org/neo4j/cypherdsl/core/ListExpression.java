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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Represents a list expression as in {@code [expression1, expression2, ..., expressionN]}
 *
 * @author Michael J. Simons
 * @soundtrack Queen - Jazz
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class ListExpression implements Expression {

	static Expression listOrSingleExpression(Expression... expressions) {

		Assertions.notNull(expressions, "Expressions are required.");
		Assertions.notEmpty(expressions, "At least one expression is required.");

		if (expressions.length == 1) {
			return expressions[0];
		} else {
			return ListExpression.create(expressions);
		}
	}

	static ListExpression create(Expression... expressions) {

		return new ListExpression(new ExpressionList(expressions));
	}

	private final ExpressionList content;

	private ListExpression(ExpressionList content) {
		this.content = content;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.content.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}
}
