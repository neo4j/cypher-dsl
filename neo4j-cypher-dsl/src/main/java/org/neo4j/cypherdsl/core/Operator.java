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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;
import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * An operator. See <a href="https://neo4j.com/docs/cypher-manual/current/syntax/operators/#query-operators-summary">Operators</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public enum Operator implements Visitable {

	// Mathematical operators
	/**
	 * Standard addition operator.
	 */
	ADDITION("+"),
	/**
	 * Standard subtraction operator.
	 */
	SUBTRACTION("-"),
	/**
	 * Unary minus operator.
	 */
	UNARY_MINUS("-", Type.PREFIX),
	/**
	 * Unary plus operator.
	 */
	UNARY_PLUS("+", Type.PREFIX),
	/**
	 * Standard multiplication operator.
	 */
	MULTIPLICATION("*"),
	/**
	 * Standard division operator.
	 */
	DIVISION("/"),
	/**
	 * Standard modulo operator.
	 */
	MODULO_DIVISION("%"),
	/**
	 * Operator for exponentiation.
	 */
	EXPONENTIATION("^"),

	// Comparison operators
	/**
	 * Comparison for equality.
	 */
	EQUALITY("="),
	/**
	 * Comparison for inequality.
	 */
	INEQUALITY("<>"),
	/**
	 * &lt; comparison.
	 */
	LESS_THAN("<"),
	/**
	 * &gt; comparison.
	 */
	GREATER_THAN(">"),
	/**
	 * &le; comparison.
	 */
	LESS_THAN_OR_EQUAL_TO("<="),
	/**
	 * &ge; comparison.
	 */
	GREATER_THAN_OR_EQUAL_TO(">="),
	/**
	 * {@code IS NULL} comparison.
	 */
	IS_NULL("IS NULL", Type.POSTFIX),
	/**
	 * {@code IS NOT NULL} comparison.
	 */
	IS_NOT_NULL("IS NOT NULL", Type.POSTFIX),

	/**
	 * String operator for {@code STARTS WITH}.
	 */
	STARTS_WITH("STARTS WITH"),
	/**
	 * String operator for {@code ENDS WITH}.
	 */
	ENDS_WITH("ENDS WITH"),
	/**
	 * String operator for {@code CONTAINS}.
	 */
	CONTAINS("CONTAINS"),

	// Boolean operators
	/**
	 * The AND operator.
	 */
	AND("AND"),
	/**
	 * The OR operator.
	 */
	OR("OR"),
	/**
	 * The XOR operator.
	 */
	XOR("XOR"),
	/**
	 * The NOT operator.
	 */
	NOT("NOT", Type.PREFIX),

	// String operators
	/**
	 * The string concatenating operator.
	 */
	CONCAT("+"),
	/**
	 * The string matching operator.
	 */
	MATCHES("=~"),

	// List operators
	/**
	 * {@code IN} operator.
	 */
	IN("IN"),

	// Property operators
	/**
	 * Property operator for assigning properties.
	 */
	SET("=", Type.PROPERTY),
	/**
	 * Property operator for retrieving properties.
	 */
	GET(".", Type.PROPERTY),
	/**
	 * Property operator for modifying properties.
	 */
	MUTATE("+=", Type.PROPERTY),

	// Node operators
	/**
	 * The label operator adding labels.
	 */
	SET_LABEL("", Type.LABEL),
	/**
	 * The label operator removing labels.
	 */
	REMOVE_LABEL("", Type.LABEL),

	// Misc
	/**
	 * The assigment operator (Read as in `p := (a)--&gt;(b)`)
	 */
	ASSIGMENT("="),
	/**
	 * The pipe operator
	 */
	PIPE("|");

	private final String representation;

	private final Type type;

	Operator(String representation) {
		this(representation, Type.BINARY);
	}

	Operator(String representation, Type type) {
		this.representation = representation;
		this.type = type;
	}

	/**
	 * @return The operators textual representation.
	 */
	@API(status = INTERNAL)
	public String getRepresentation() {
		return representation;
	}

	/**
	 * @return True if this is a unary operator.
	 */
	boolean isUnary() {
		return type != Type.BINARY;
	}

	/**
	 * @return The type of this operator.
	 */
	@API(status = INTERNAL)
	public Type getType() {
		return type;
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	/**
	 * {@link Operator} type.
	 *
	 * @since 1.0
	 */
	public enum Type {
		/**
		 * Describes a binary operator (An operator with to operands).
		 */
		BINARY,
		/**
		 * Describes a unary prefix operator (An operator with one operand after the operator).
		 */
		PREFIX,
		/**
		 * Describes a unary postfix operator (An operator with one operand before the operator).
		 */
		POSTFIX,
		/**
		 * Describes an operator working with properties of entities.
		 */
		PROPERTY,
		/**
		 * The binary operator modifying labels of nodes.
		 */
		LABEL
	}
}
