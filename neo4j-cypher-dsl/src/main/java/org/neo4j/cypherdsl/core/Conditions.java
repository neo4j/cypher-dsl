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

import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Builder for various conditions.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @author Aakash Sorathiya
 * @since 1.0
 */
final class Conditions {

	/**
	 * Not to be instantiated.
	 */
	private Conditions() {
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} includes all elements
	 * present in {@code rhs}.
	 * @param lhs argument that is tested whether it contains all values in {@code rhs} or
	 * not
	 * @param rhs the reference collection
	 * @return an "includesAll" comparison
	 * @since 2022.7.0
	 */
	static Condition includesAll(Expression lhs, Expression rhs) {
		SymbolicName x = SymbolicName.of("x");
		return Predicates.all(x).in(rhs).where(x.in(lhs));
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} includes any element
	 * present in {@code rhs}.
	 * @param lhs argument that is tested whether it contains any values in {@code rhs} or
	 * not
	 * @param rhs the reference collection
	 * @return a "not_includes" comparison
	 * @since 2022.7.0
	 */
	static Condition includesAny(Expression lhs, Expression rhs) {
		SymbolicName x = SymbolicName.of("x");
		return Predicates.any(x).in(rhs).where(x.in(lhs));
	}

	/**
	 * Creates a condition that checks whether this matches a given relationship pattern.
	 * @param relationshipPattern the pattern being evaluated in a condition
	 * @return a new condition matching the given pattern
	 */
	static Condition matching(RelationshipPattern relationshipPattern) {
		return RelationshipPatternCondition.of(relationshipPattern);
	}

	/**
	 * Creates a condition that matches if the right hand side is a regular expression
	 * that matches the left hand side via {@code =~}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "matches" comparison
	 */
	static Condition matches(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.MATCHES, rhs);
	}

	/**
	 * Creates a condition that matches if both expressions are equals according to
	 * {@code =}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return an "equals" comparison
	 */
	static Condition isEqualTo(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.EQUALITY, rhs);
	}

	/**
	 * Creates a condition that matches if both expressions are equals according to
	 * {@code <>}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "not equals" comparison
	 */
	static Condition isNotEqualTo(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.INEQUALITY, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is less than the right hand
	 * side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return an "less than" comparison
	 */
	static Condition lt(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.LESS_THAN, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is less than or equal the
	 * right hand side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "less than or equal" comparison
	 */
	static Condition lte(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.LESS_THAN_OR_EQUAL_TO, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is greater than or equal the
	 * right hand side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a "greater than or equal" comparison
	 */
	static Condition gte(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.GREATER_THAN_OR_EQUAL_TO, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is greater than the right
	 * hand side.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return an "greater than" comparison
	 */
	static Condition gt(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.GREATER_THAN, rhs);
	}

	/**
	 * Negates the given condition.
	 * @param condition the condition to negate. Must not be null.
	 * @return the negated condition.
	 */
	static Condition not(Condition condition) {

		Assertions.notNull(condition, "Condition to negate must not be null.");
		return condition.not();
	}

	/**
	 * Negates the given pattern element: The pattern must not matched to be included in
	 * the result.
	 * @param pattern the pattern to negate. Must not be null.
	 * @return a condition that evaluates to true when the pattern does not match.
	 */
	static Condition not(RelationshipPattern pattern) {

		return RelationshipPatternCondition.not(pattern);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} starts with the
	 * {@code rhs}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a new condition.
	 */
	static Condition startsWith(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.STARTS_WITH, rhs);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} contains with the
	 * {@code rhs}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a new condition.
	 */
	static Condition contains(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.CONTAINS, rhs);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} ends with the {@code rhs}.
	 * @param lhs the left hand side of the comparison
	 * @param rhs the right hand side of the comparison
	 * @return a new condition.
	 */
	static Condition endsWith(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.ENDS_WITH, rhs);
	}

	/**
	 * Creates a placeholder condition which is not rendered in the final statement but is
	 * useful while chaining conditions together.
	 * @return a placeholder condition.
	 */
	static Condition noCondition() {

		return CompoundCondition.empty();
	}

	/**
	 * Creates a condition that checks whether the {@code expression} is {@literal null}.
	 * @param expression the expression to check for {@literal null}
	 * @return a new condition.
	 */
	static Condition isNull(Expression expression) {

		return Comparison.create(Operator.IS_NULL, expression);
	}

	/**
	 * Creates a condition that checks whether the {@code expression} is not
	 * {@literal null}.
	 * @param expression the expression to check for {@literal null}
	 * @return a new condition.
	 */
	static Condition isNotNull(Expression expression) {

		return Comparison.create(Operator.IS_NOT_NULL, expression);
	}

	/**
	 * {@return a condition that is always true}
	 */
	static Condition isTrue() {

		return ConstantCondition.TRUE;
	}

	/**
	 * {@return a condition that is always false}
	 */
	static Condition isFalse() {

		return ConstantCondition.FALSE;
	}

	/**
	 * Creates condition testing the existing of labels or types.
	 * @param symbolicName reference to the entity that should be checked for labels or
	 * types
	 * @param labelsOrTypes the list of labels or types to check for
	 * @return a condition that checks whether a node has a set of given labels or a
	 * relationship a set of given types.
	 * @since 2021.3.0
	 */
	static Condition hasLabelsOrType(SymbolicName symbolicName, String... labelsOrTypes) {

		return HasLabelCondition.create(symbolicName, labelsOrTypes);
	}

}
