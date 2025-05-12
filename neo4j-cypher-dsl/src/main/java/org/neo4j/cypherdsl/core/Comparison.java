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

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.ast.EnterResult;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A concrete condition representing a comparision between two expressions.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Comparison implements Condition {

	static Comparison create(Operator operator, Expression expression) {

		Assertions.isTrue(operator.isUnary(), "Operator must be unary.");
		Assertions.notNull(expression, "Expression must not be null.");

		return switch (operator.getType()) {
			case PREFIX -> new Comparison(null, operator, expression);
			case POSTFIX -> new Comparison(expression, operator, null);
			default -> throw new IllegalArgumentException("Invalid operator type " + operator.getType());
		};
	}

	static Comparison create(Expression lhs, Operator operator, Expression rhs) {

		Assertions.notNull(lhs, "Left expression must not be null.");
		Assertions.notNull(operator, "Operator must not be empty.");
		Assertions.notNull(rhs, "Right expression must not be null.");

		return new Comparison(lhs, operator, rhs);
	}

	private static Expression nestedIfCondition(Expression expression) {
		return expression instanceof Condition ? new NestedExpression(expression) : expression;
	}

	private final Expression left;
	private final Operator comparator;
	private final Expression right;

	private Comparison(Expression left, Operator operator, Expression right) {

		this.left = nestedIfCondition(left);
		this.comparator = operator;
		this.right = nestedIfCondition(right);
	}

	@Override
	public void accept(Visitor visitor) {

		EnterResult result = visitor.enterWithResult(this);
		if (result == EnterResult.CONTINUE) {
			if (left != null) {
				Expressions.nameOrExpression(left).accept(visitor);
			}
			comparator.accept(visitor);
			if (right != null) {
				Expressions.nameOrExpression(right).accept(visitor);
			}
		}
		visitor.leave(this);
	}

	@API(status = INTERNAL, since = "2024.6.1")
	public Expression getLeft() {
		return this.left;
	}

	@API(status = INTERNAL, since = "2024.6.1")
	public Operator getComparator() {
		return this.comparator;
	}

	@API(status = INTERNAL, since = "2024.6.1")
	public Expression getRight() {
		return this.right;
	}


	@NotNull
	@Override
	public Condition not() {

		if (this.comparator == Operator.IS_NULL) {
			return new Comparison(left, Operator.IS_NOT_NULL, right);
		} else if (this.comparator == Operator.IS_NOT_NULL) {
			return new Comparison(left, Operator.IS_NULL, right);
		}
		return Condition.super.not();
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}
}

