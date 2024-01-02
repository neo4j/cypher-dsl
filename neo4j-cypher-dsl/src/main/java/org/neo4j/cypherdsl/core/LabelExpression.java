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

import java.util.List;

import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * Represents a label expression, usable in {@link Cypher#node(LabelExpression)}.
 *
 * @author Michael J. Simons
 * @param type    whether this is a leaf or another node
 * @param negated a flag if this subtree is negated
 * @param value   a single value or a list of value for {@link Type#COLON_DISJUNCTION} or {@link Type#COLON_CONJUNCTION}
 * @param lhs     the left hand site of this tree
 * @param rhs     the right hand site of this tree
 * @soundtrack The Prodigy - Music For The Jilted Generation
 * @since 2023.0.2
 */
public record LabelExpression(
	Type type,
	boolean negated, List<String> value,
	LabelExpression lhs,
	LabelExpression rhs
) implements Visitable {

	/**
	 * Creates an immutable label expression.
	 *
	 * @param type    whether this is a leaf or another node
	 * @param negated a flag if this subtree is negated
	 * @param value   a single value or a list of value for {@link Type#COLON_DISJUNCTION} or {@link Type#COLON_CONJUNCTION}
	 * @param lhs     the left hand site of this tree
	 * @param rhs     the right hand site of this tree
	 */
	public LabelExpression {
		value = value == null ? null : List.copyOf(value);
	}

	/**
	 * Creates a leaf expression from a string
	 *
	 * @param value the leaf value
	 */
	public LabelExpression(String value) {
		this(Type.LEAF, false, List.of(value), null, null);
	}

	/**
	 * Create a conjunction
	 *
	 * @param next the expression to add
	 * @return a new expression
	 */
	public LabelExpression and(LabelExpression next) {
		return new LabelExpression(Type.CONJUNCTION, false, null, this, next);
	}

	/**
	 * Create a disjunction
	 *
	 * @param next the expression to add
	 * @return a new expression
	 */
	public LabelExpression or(LabelExpression next) {
		return new LabelExpression(Type.DISJUNCTION, false, null, this, next);
	}

	/**
	 * Negates this expressio
	 *
	 * @return a new expression
	 */
	public LabelExpression negate() {
		return new LabelExpression(this.type, !this.negated, this.value, this.lhs, this.rhs);
	}

	/**
	 * Type of this expression
	 */
	public enum Type {
		/**
		 * A leaf
		 */
		LEAF(""),
		/**
		 * A list of values, conjugated
		 */
		COLON_CONJUNCTION(":"),
		/**
		 * A list of values, disjoined
		 */
		COLON_DISJUNCTION(":"),
		/**
		 * A conjunction
		 */
		CONJUNCTION("&"),
		/**
		 * A disjunction
		 */
		DISJUNCTION("|");

		private final String value;

		Type(String value) {
			this.value = value;
		}

		/**
		 * @return a representation of this type
		 */
		public String getValue() {
			return value;
		}
	}
}
