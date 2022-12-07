/*
 * Copyright (c) 2019-2022 "Neo4j,"
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

import java.util.Set;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A set of operations.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Operations {

	private static final java.util.Set<Class<? extends Expression>> VALID_MUTATORS =
		Set.of(MapExpression.class, Parameter.class, MapProjection.class, SymbolicName.class, FunctionInvocation.class);

	/**
	 * Creates an unary minus operation.
	 *
	 * @param e The expression to which the unary minus should be applied. We don't check if it's a numeric expression,
	 *          but in hindsight to generate semantically correct Cypher, it's recommended that is one.
	 * @return An unary minus operation.
	 * @since 2021.2.3
	 */
	public static Operation minus(Expression e) {

		return Operation.create(Operator.UNARY_MINUS, e);
	}

	/**
	 * Creates an unary plus operation.
	 *
	 * @param e The expression to which the unary plus should be applied. We don't check if it's a numeric expression,
	 *          but in hindsight to generate semantically correct Cypher, it's recommended that is one.
	 * @return An unary plus operation.
	 * @since 2021.2.3
	 */
	public static Expression plus(Expression e) {

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
	 * Creates a {@code =} operation. The left hand side should resolve to a property or to something which has labels
	 * or types to modify and the right hand side should either be new properties or labels.
	 *
	 * @param target The target that should be modified
	 * @param value  The new value of the target
	 * @return A new operation.
	 * @since 2021.2.3
	 */
	public static Operation set(Expression target, Expression value) {

		return Operation.create(target, Operator.SET, value);
	}

	/**
	 * Creates a {@code +=} operation. The left hand side must resolve to a container (either a node or a relationship)
	 * of properties and the right hand side must be a map of new or updated properties
	 *
	 * @param target The target container that should be modified
	 * @param value  The new properties
	 * @return A new operation.
	 * @since 2020.1.5
	 */
	public static Operation mutate(Expression target, MapExpression value) {

		return Operation.create(target, Operator.MUTATE, value);
	}

	/**
	 * Creates a {@code +=} operation. The left hand side must resolve to a container (either a node or a relationship)
	 * of properties and the right hand side must be a map of new or updated properties
	 *
	 * @param target The target container that should be modified
	 * @param value  The new properties
	 * @return A new operation.
	 * @since 2020.1.5
	 */
	public static Operation mutate(Expression target, Expression value) {

		Assertions.notNull(value, "New properties value must not be null");
		Assertions.isTrue(Property.class.isAssignableFrom(value.getClass()) || VALID_MUTATORS.contains(value.getClass()),
			"A property container can only be mutated by a map, or a parameter or property pointing to a map.");

		return Operation.create(target, Operator.MUTATE, value);
	}

	/**
	 * Creates an operation adding one or more labels from a given {@link Node node}.
	 *
	 * @param target The target of the new labels
	 * @param label  The labels to be added
	 * @return A set operation
	 * @since 2021.2.3
	 */
	public static Operation set(Node target, String... label) {

		return Operation.create(target, Operator.SET_LABEL, label);
	}

	/**
	 * Creates an operation removing one or more labels from a given {@link Node node}.
	 *
	 * @param target The target of the remove operation
	 * @param label  The labels to be removed
	 * @return A remove operation
	 * @since 2021.2.3
	 */
	public static Operation remove(Node target, String... label) {

		return Operation.create(target, Operator.REMOVE_LABEL, label);
	}

	/**
	 * Not to be instantiated.
	 */
	private Operations() {
	}
}
