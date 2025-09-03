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

import java.util.Set;

import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A set of operations.
 *
 * @author Michael J. Simons
 * @since 1.0 be accessible.
 */
final class Operations {

	private static final java.util.Set<Class<? extends Expression>> VALID_MUTATORS = Set.of(MapExpression.class,
			Parameter.class, MapProjection.class, SymbolicName.class, FunctionInvocation.class);

	/**
	 * Not to be instantiated.
	 */
	private Operations() {
	}

	/**
	 * Creates a unary minus operation.
	 * @param e the expression to which the unary minus should be applied. We don't check
	 * if it's a numeric expression, but in hindsight to generate semantically correct
	 * Cypher, it's recommended that is one.
	 * @return an unary minus operation.
	 * @since 2021.2.3
	 */
	static Operation minus(Expression e) {

		return Operation.create(Operator.UNARY_MINUS, e);
	}

	/**
	 * Creates an unary plus operation.
	 * @param e the expression to which the unary plus should be applied. We don't check
	 * if it's a numeric expression, but in hindsight to generate semantically correct
	 * Cypher, it's recommended that is one.
	 * @return an unary plus operation.
	 * @since 2021.2.3
	 */
	static Expression plus(Expression e) {

		return Operation.create(Operator.UNARY_PLUS, e);
	}

	static Operation concat(Expression op1, Expression op2) {

		return Operation.create(op1, Operator.CONCAT, op2);
	}

	static Operation add(Expression op1, Expression op2) {

		return Operation.create(op1, Operator.ADDITION, op2);
	}

	static Operation subtract(Expression op1, Expression op2) {

		return Operation.create(op1, Operator.SUBTRACTION, op2);
	}

	static Operation multiply(Expression op1, Expression op2) {

		return Operation.create(op1, Operator.MULTIPLICATION, op2);
	}

	static Operation divide(Expression op1, Expression op2) {

		return Operation.create(op1, Operator.DIVISION, op2);
	}

	static Operation remainder(Expression op1, Expression op2) {

		return Operation.create(op1, Operator.MODULO_DIVISION, op2);
	}

	static Operation pow(Expression op1, Expression op2) {

		return Operation.create(op1, Operator.EXPONENTIATION, op2);
	}

	/**
	 * Creates a {@code =} operation. The left hand side should resolve to a property or
	 * to something which has labels or types to modify and the right hand side should
	 * either be new properties or labels.
	 * @param target the target that should be modified
	 * @param value the new value of the target
	 * @return a new operation.
	 * @since 2021.2.3
	 */
	static Operation set(Expression target, Expression value) {

		return Operation.create(target, Operator.SET, value);
	}

	/**
	 * Creates a {@code +=} operation. The left hand side must resolve to a container
	 * (either a node or a relationship) of properties and the right hand side must be a
	 * map of new or updated properties
	 * @param target the target container that should be modified
	 * @param value the new properties
	 * @return a new operation.
	 * @since 2020.1.5
	 */
	static Operation mutate(Expression target, MapExpression value) {

		return Operation.create(target, Operator.MUTATE, value);
	}

	/**
	 * Creates a {@code +=} operation. The left hand side must resolve to a container
	 * (either a node or a relationship) of properties and the right hand side must be a
	 * map of new or updated properties
	 * @param target the target container that should be modified
	 * @param value the new properties
	 * @return a new operation.
	 * @since 2020.1.5
	 */
	static Operation mutate(Expression target, Expression value) {

		Assertions.notNull(value, "New properties value must not be null");
		Assertions.isTrue(
				Property.class.isAssignableFrom(value.getClass()) || VALID_MUTATORS.contains(value.getClass()),
				"A property container can only be mutated by a map, or a parameter or property pointing to a map.");

		return Operation.create(target, Operator.MUTATE, value);
	}

	/**
	 * Creates an operation adding one or more labels from a given {@link Node node}.
	 * @param target the target of the new labels
	 * @param label the labels to be added
	 * @return a set operation
	 * @since 2021.2.3
	 */
	static Operation set(Node target, String... label) {

		return Operation.create(target, Operator.SET_LABEL, label);
	}

	/**
	 * Creates an operation adding one or more labels from a given {@link Node node}.
	 * @param target the target of the new labels
	 * @param labels the labels to be added
	 * @return a set operation
	 * @since 2025.1.0
	 */
	static Operation set(Node target, Labels labels) {

		return Operation.create(target, Operator.SET_LABEL, labels);
	}

	/**
	 * Creates an operation removing one or more labels from a given {@link Node node}.
	 * @param target the target of the remove operation
	 * @param label the labels to be removed
	 * @return a remove operation
	 * @since 2021.2.3
	 */
	static Operation remove(Node target, String... label) {

		return Operation.create(target, Operator.REMOVE_LABEL, label);
	}

	/**
	 * Creates an operation removing one or more labels from a given {@link Node node}.
	 * @param node the node from which the labels should be removed
	 * @param labels the labels to be removed
	 * @return a remove operation
	 * @since 2025.1.0
	 */
	static Expression remove(Node node, Labels labels) {

		return Operation.create(node, Operator.REMOVE_LABEL, labels);
	}

}
