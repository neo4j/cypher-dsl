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

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
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
@API(status = STABLE, since = "1.0")
public final class Conditions {

	/**
	 * Creates a condition that checks whether the {@code lhs} includes all elements present in {@code rhs}.
	 * @param lhs Argument that is tested whether it contains all values in {@code rhs} or not
	 * @param rhs The reference collection
	 * @return An "includesAll" comparison
	 * @since 2022.7.0
	 */
	static Condition includesAll(Expression lhs, Expression rhs) {
		SymbolicName x = SymbolicName.of("x");
		return Predicates.all(x).in(rhs).where(x.in(lhs));
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} includes any element present in {@code rhs}.
	 * @param lhs Argument that is tested whether it contains any values in {@code rhs} or not
 	 * @param rhs The reference collection
	 * @return A "not_includes" comparison
	 * @since 2022.7.0
	 */
	static Condition includesAny(Expression lhs, Expression rhs) {
		SymbolicName x = SymbolicName.of("x");
		return Predicates.any(x).in(rhs).where(x.in(lhs));
	}

	/**
	 * Creates a condition that matches if the right hand side is a regular expression that matches the the left hand side via
	 * {@code =~}.
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return A "matches" comparison
	 */
	static Condition matches(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.MATCHES, rhs);
	}

	/**
	 * Creates a condition that matches if both expressions are equals according to {@code =}.
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return An "equals" comparison
	 */
	static Condition isEqualTo(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.EQUALITY, rhs);
	}

	/**
	 * Creates a condition that matches if both expressions are equals according to {@code <>}.
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return An "not equals" comparison
	 */
	static Condition isNotEqualTo(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.INEQUALITY, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is less than the right hand side..
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return An "less than" comparison
	 */
	static Condition lt(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.LESS_THAN, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is less than or equal the right hand side..
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return An "less than or equal" comparison
	 */
	static Condition lte(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.LESS_THAN_OR_EQUAL_TO, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is greater than or equal the right hand side..
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return An "greater than or equal" comparison
	 */
	static Condition gte(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.GREATER_THAN_OR_EQUAL_TO, rhs);
	}

	/**
	 * Creates a condition that matches if the left hand side is greater than the right hand side..
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return An "greater than" comparison
	 */
	static Condition gt(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.GREATER_THAN, rhs);
	}

	/**
	 * Negates the given condition.
	 *
	 * @param condition The condition to negate. Must not be null.
	 * @return The negated condition.
	 */
	@NotNull @Contract(pure = true)
	public static Condition not(@NotNull Condition condition) {

		Assertions.notNull(condition, "Condition to negate must not be null.");
		return condition.not();
	}

	/**
	 * Negates the given pattern element: The pattern must not matched to be included in the result.
	 *
	 * @param pattern The pattern to negate. Must not be null.
	 * @return A condition that evaluates to true when the pattern does not match.
	 */
	@NotNull @Contract(pure = true)
	public static Condition not(@NotNull RelationshipPattern pattern) {

		return RelationshipPatternCondition.not(pattern);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} starts with the {@code rhs}.
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return A new condition.
	 */
	static Condition startsWith(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.STARTS_WITH, rhs);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} contains with the {@code rhs}.
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return A new condition.
	 */
	static Condition contains(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.CONTAINS, rhs);
	}

	/**
	 * Creates a condition that checks whether the {@code lhs} ends with the {@code rhs}.
	 *
	 * @param lhs The left hand side of the comparison
	 * @param rhs The right hand side of the comparison
	 * @return A new condition.
	 */
	static Condition endsWith(Expression lhs, Expression rhs) {
		return Comparison.create(lhs, Operator.ENDS_WITH, rhs);
	}

	/**
	 * Creates a placeholder condition which is not rendered in the final statement but is useful while chaining
	 * conditions together.
	 *
	 * @return A placeholder condition.
	 */
	@NotNull @Contract(pure = true)
	public static Condition noCondition() {

		return CompoundCondition.empty();
	}

	/**
	 * Creates a condition that checks whether the {@code expression} is {@literal null}.
	 *
	 * @param expression The expression to check for {@literal null}
	 * @return A new condition.
	 */
	static Condition isNull(Expression expression) {

		return Comparison.create(Operator.IS_NULL, expression);
	}

	/**
	 * Creates a condition that checks whether the {@code expression} is not {@literal null}.
	 *
	 * @param expression The expression to check for {@literal null}
	 * @return A new condition.
	 */
	static Condition isNotNull(Expression expression) {

		return Comparison.create(Operator.IS_NOT_NULL, expression);
	}

	/**
	 * A condition that evaluates to true if a list or a string represented by {@code expression} is empty or has the length of 0.
	 *
	 * @param expression The expression to test for emptiness.
	 * @return A new condition.
	 */
	static Condition isEmpty(Expression expression) {

		return Functions.size(expression).isEqualTo(Cypher.literalOf(0L));
	}

	/**
	 * @return a condition that is always true.
	 */
	public static Condition isTrue() {

		return ConstantCondition.TRUE;
	}

	/**
	 * @return a condition that is always false.
	 */
	public static Condition isFalse() {

		return ConstantCondition.FALSE;
	}

	/**
	 * @param symbolicName  Reference to the entity that should be checked for labels or types
	 * @param labelsOrTypes the list of labels or types to check for
	 * @return A condition that checks whether a node has a set of given labels or a relationship a set of given types.
	 * @since 2021.3.0
	 */
	public static Condition hasLabelsOrType(SymbolicName symbolicName, String... labelsOrTypes) {

		return HasLabelCondition.create(symbolicName, labelsOrTypes);
	}

	/**
	 * Not to be instantiated.
	 */
	private Conditions() {
	}
}
