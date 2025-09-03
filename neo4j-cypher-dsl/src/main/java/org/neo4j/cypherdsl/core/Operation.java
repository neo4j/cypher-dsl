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

import java.util.Arrays;
import java.util.EnumSet;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * A binary operation.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Operation implements Expression {

	/**
	 * A set of operators triggering operations on labels.
	 */
	private static final EnumSet<Operator> LABEL_OPERATORS = EnumSet.of(Operator.SET_LABEL, Operator.REMOVE_LABEL);

	private static final EnumSet<Operator.Type> NEEDS_GROUPING_BY_TYPE = EnumSet
		.complementOf(EnumSet.of(Operator.Type.PROPERTY, Operator.Type.LABEL));

	private static final EnumSet<Operator> DONT_GROUP = EnumSet.of(Operator.EXPONENTIATION, Operator.PIPE,
			Operator.UNARY_MINUS, Operator.UNARY_PLUS);

	private final Expression left;

	private final Operator operator;

	private final Visitable right;

	Operation(Expression left, Operator operator, Expression right) {

		this.left = left;
		this.operator = operator;
		this.right = right;
	}

	Operation(Expression left, Operator operator, Visitable right) {

		this.left = left;
		this.operator = operator;
		this.right = right;
	}

	static Operation create(Operator operator, Expression expression) {

		Assertions.notNull(operator, "Operator must not be null.");
		Assertions.isTrue(operator.isUnary(), "Operator must be unary.");
		Assertions.notNull(expression, "The expression must not be null.");

		return switch (operator.getType()) {
			case PREFIX -> new Operation(null, operator, expression);
			case POSTFIX -> new Operation(expression, operator, (Expression) null);
			default -> throw new IllegalArgumentException("Invalid operator type " + operator.getType());
		};
	}

	static Operation create(Expression op1, Operator operator, Expression op2) {

		Assertions.notNull(op1, "The first operand must not be null.");
		Assertions.notNull(operator, "Operator must not be null.");
		Assertions.notNull(op2, "The second operand must not be null.");

		return new Operation(op1, operator, op2);
	}

	static Operation create(Node op1, Operator operator, String... nodeLabels) {

		Assertions.notNull(op1, "The first operand must not be null.");
		Assertions.isTrue(op1.getSymbolicName().isPresent(), "The node must have a name.");
		Assertions.isTrue(LABEL_OPERATORS.contains(operator),
				String.format("Only operators %s can be used to modify labels", LABEL_OPERATORS));
		Assertions.notEmpty(nodeLabels, "The labels cannot be empty.");

		var listOfNodeLabels = new TypedSubtree<>(Arrays.stream(nodeLabels).map(NodeLabel::new).toList()) {
			@Override
			public String separator() {
				return "";
			}
		};
		return new Operation(op1.getRequiredSymbolicName(), operator, listOfNodeLabels);
	}

	static Operation create(Node op1, Operator operator, Labels labels) {

		Assertions.notNull(op1, "The first operand must not be null.");
		Assertions.isTrue(op1.getSymbolicName().isPresent(), "The node must have a name.");
		Assertions.isTrue(LABEL_OPERATORS.contains(operator),
				String.format("Only operators %s can be used to modify labels", LABEL_OPERATORS));
		Assertions.notNull(labels, "The labels cannot be empty.");
		Assertions.isTrue(labels.canBeUsedInUpdate(),
				"Only a single dynamic label expression or a set of static labels might be used in an updating clause");

		return new Operation(op1.getRequiredSymbolicName(), operator, labels);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		if (this.left != null) {
			Expressions.nameOrExpression(this.left).accept(visitor);
		}
		this.operator.accept(visitor);
		Visitable.visitIfNotNull(this.right, visitor);
		visitor.leave(this);
	}

	/**
	 * Checks, whether this operation needs grouping.
	 * @return true, if this operation needs grouping.
	 */
	public boolean needsGrouping() {
		return NEEDS_GROUPING_BY_TYPE.contains(this.operator.getType()) && !DONT_GROUP.contains(this.operator);
	}

	@API(status = INTERNAL)
	Operator getOperator() {
		return this.operator;
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
