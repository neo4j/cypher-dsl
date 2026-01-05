/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * An expression can be used in many places, i.e. in return statements, pattern elements
 * etc.
 *
 * @author Michael J. Simons
 * @author Aakash Sorathiya
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface Expression extends Visitable, PropertyAccessor {

	/**
	 * Creates a condition that checks whether this {@code expression} includes all
	 * elements of {@code rhs}.
	 * @param rhs the other collection to compare to, must evaluate into a list during
	 * runtime.
	 * @return a new condition
	 * @since 2022.7.0
	 */
	default Condition includesAll(Expression rhs) {
		return Conditions.includesAll(this, rhs);
	}

	/**
	 * Creates a condition that checks whether this {@code expression} includes any
	 * element of {@code rhs}.
	 * @param rhs the other collection to compare to, must evaluate into a list during
	 * runtime.
	 * @return a new condition
	 * @since 2022.7.0
	 */
	default Condition includesAny(Expression rhs) {
		return Conditions.includesAny(this, rhs);
	}

	/**
	 * Creates an expression with an alias. This expression does not track which or how
	 * many aliases have been created.
	 * @param alias the alias to use
	 * @return an aliased expression.
	 */
	default AliasedExpression as(String alias) {

		Assertions.hasText(alias, "The alias may not be null or empty.");
		return new AliasedExpression(this, alias);
	}

	/**
	 * This creates a {@literal size(e)} expression from this expression. The Cypher
	 * output will semantically only be valid when this refers to a list (see <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size">size(list)</a>)
	 * or when the expression is a string (see <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/scalar/#functions-size-of-string">size()
	 * applied to string</a>).
	 * <p>
	 * Any other expression will produce Cypher that is either deprecated in Neo4j &ge;
	 * 4.4 or not supported at all.
	 * @return the size of this expression (Either the number of items in a list or the
	 * number of characters in a string expression).
	 * @since 2022.1.0
	 */
	default Expression size() {

		return Functions.size(this);
	}

	/**
	 * Takes the {@link #size()} expresssions and compares it for equality with the
	 * parameter {@code expectedSize}. The same restrictions as with {@link #size()}
	 * apply.
	 * @param expectedSize the expected size
	 * @return a condition
	 * @since 2022.1.0
	 * @see #size()
	 */
	default Condition hasSize(Expression expectedSize) {

		return Functions.size(this).isEqualTo(expectedSize);
	}

	/**
	 * Reuse an existing symbolic name to alias this expression.
	 * @param alias a symbolic name
	 * @return an aliased expression.
	 * @since 2021.0.2
	 */
	default AliasedExpression as(SymbolicName alias) {

		Assertions.notNull(alias, "The alias may not be null.");
		return as(alias.getValue());
	}

	/**
	 * Transform this expression into a condition.
	 * @return this expression as a condition. Will return the same instance if it is
	 * already a condition.
	 * @since 2021.2.2
	 */
	default Condition asCondition() {
		return (this instanceof Condition condition) ? condition : new ExpressionCondition(this);
	}

	/**
	 * Creates a {@code lhs = rhs} condition.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition isEqualTo(Expression rhs) {
		return Conditions.isEqualTo(this, rhs);
	}

	/**
	 * An alias for {@link #isEqualTo(Expression)}.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition eq(Expression rhs) {
		return isEqualTo(rhs);
	}

	/**
	 * Creates a {@code lhs <> rhs} condition.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition isNotEqualTo(Expression rhs) {
		return Conditions.isNotEqualTo(this, rhs);
	}

	/**
	 * An alias for {@link #isNotEqualTo(Expression)}.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition ne(Expression rhs) {
		return isNotEqualTo(rhs);
	}

	/**
	 * Creates a {@code lhs < rhs} condition.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition lt(Expression rhs) {
		return Conditions.lt(this, rhs);
	}

	/**
	 * Creates a {@code lhs <= rhs} condition.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition lte(Expression rhs) {
		return Conditions.lte(this, rhs);
	}

	/**
	 * Creates a {@code lhs > rhs} condition.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition gt(Expression rhs) {
		return Conditions.gt(this, rhs);
	}

	/**
	 * Creates a {@code lhs >= rhs} condition.
	 * @param rhs the right hand side of the condition
	 * @return a new condition
	 */
	default Condition gte(Expression rhs) {
		return Conditions.gte(this, rhs);
	}

	/**
	 * Creates a condition that checks whether this {@code expression} is {@literal true}.
	 * @return a new condition
	 */
	default Condition isTrue() {
		return Conditions.isEqualTo(this, Cypher.literalTrue());
	}

	/**
	 * Creates a condition that checks whether this {@code expression} is
	 * {@literal false}.
	 * @return a new condition
	 */
	default Condition isFalse() {
		return Conditions.isEqualTo(this, Cypher.literalFalse());
	}

	/**
	 * Creates a condition that checks whether this {@code expression} matches that
	 * {@code expression}.
	 * @param expression the expression to match against. Must evaluate into a string
	 * during runtime.
	 * @return a new condition.
	 */
	default Condition matches(Expression expression) {
		return Conditions.matches(this, expression);
	}

	/**
	 * Creates a condition that checks whether this {@code expression} matches the given
	 * {@code pattern}.
	 * @param pattern the pattern to match
	 * @return a new condition.
	 */
	default Condition matches(String pattern) {
		return Conditions.matches(this, Cypher.literalOf(pattern));
	}

	/**
	 * Creates a condition that checks whether this {@code expression} starts with that
	 * {@code expression}.
	 * @param expression the expression to match against. Must evaluate into a string
	 * during runtime.
	 * @return a new condition.
	 */
	default Condition startsWith(Expression expression) {
		return Conditions.startsWith(this, expression);
	}

	/**
	 * Creates a condition that checks whether this {@code expression} contains that
	 * {@code expression}.
	 * @param expression the expression to match against. Must evaluate into a string
	 * during runtime.
	 * @return a new condition.
	 */
	default Condition contains(Expression expression) {
		return Conditions.contains(this, expression);
	}

	/**
	 * Creates a condition that checks whether this {@code expression} ends with that
	 * {@code expression}.
	 * @param expression the expression to match against. Must evaluate into a string
	 * during runtime.
	 * @return a new condition.
	 */
	default Condition endsWith(Expression expression) {
		return Conditions.endsWith(this, expression);
	}

	/**
	 * Creates an expression concatenating two string or list expressions.
	 * @param expression the expression to concat to this expression.
	 * @return a new expression.
	 */
	default Operation concat(Expression expression) {
		return Operations.concat(this, expression);
	}

	/**
	 * Creates a {@code +} operation of this (the augend) and the {@code addend}.
	 * @param addend the addend
	 * @return a new operation.
	 * @since 1.0.1
	 */
	default Operation add(Expression addend) {
		return Operations.add(this, addend);
	}

	/**
	 * Creates a {@code -} operation of this (the minuend) and the {@code subtrahend}.
	 * @param subtrahend the subtrahend
	 * @return a new operation.
	 * @since 1.0.1
	 */
	default Operation subtract(Expression subtrahend) {
		return Operations.subtract(this, subtrahend);
	}

	/**
	 * Creates a {@code *} operation of this (the multiplier) and the
	 * {@code multiplicand}.
	 * @param multiplicand the multiplicand
	 * @return a new operation.
	 * @since 1.0.1
	 */
	default Operation multiply(Expression multiplicand) {
		return Operations.multiply(this, multiplicand);
	}

	/**
	 * Creates a {@code /} operation of this (the divisor) and the {@code dividend}.
	 * @param dividend the dividend
	 * @return a new operation.
	 * @since 1.0.1
	 */
	default Operation divide(Expression dividend) {
		return Operations.divide(this, dividend);
	}

	/**
	 * Returns the remainder of this value and the {@code dividend}.
	 * @param dividend the dividend
	 * @return a new operation.
	 */
	default Operation remainder(Expression dividend) {
		return Operations.remainder(this, dividend);
	}

	/**
	 * Returns the power of n of this value.
	 * @param n power to raise this {@code Expression} to.
	 * @return a new operation.
	 */
	default Operation pow(Expression n) {

		return Operations.pow(this, n);
	}

	/**
	 * Creates a {@code IS NULL} operation for this {@code expression}. The expression
	 * does not track the condition created here.
	 * @return a condition based on this expression that evaluates to true when this
	 * expression is null.
	 */
	default Condition isNull() {
		return Conditions.isNull(this);
	}

	/**
	 * Creates a {@code IS NOT NULL} operation for this {@code expression}. The expression
	 * does not track the condition created here.
	 * @return a condition based on this expression that evaluates to true when this
	 * expression is not null.
	 */
	default Condition isNotNull() {
		return Conditions.isNotNull(this);
	}

	/**
	 * Creates a {@code IN} operation for this expression and that {@code expression}. The
	 * expression does not track the condition created here.
	 * @param haystack the expression to search for this expression
	 * @return a new condition.
	 */
	default Condition in(Expression haystack) {
		return Comparison.create(this, Operator.IN, haystack);
	}

	/**
	 * Creates a condition that evaluates to true if this expression is empty.
	 * @return a new condition.
	 */
	default Condition isEmpty() {

		return Functions.size(this).isEqualTo(Cypher.literalOf(0L));
	}

	/**
	 * The property does not track the sort items created here.
	 * @return a sort item for this property in descending order
	 */
	default SortItem descending() {

		return SortItem.create(this, SortItem.Direction.DESC);
	}

	/**
	 * The property does not track the sort items created here.
	 * @return a sort item for this property in ascending order
	 */
	default SortItem ascending() {

		return SortItem.create(this, SortItem.Direction.ASC);
	}

	/**
	 * Creates a new sort item with the given direction.
	 * @param direction the direction to sort
	 * @return a new sort item.
	 * @since 2021.4.1
	 */
	default SortItem sorted(SortItem.Direction direction) {

		return SortItem.create(this, direction);
	}

	@Override
	default Property property(String... names) {

		return InternalPropertyImpl.create(this, names);
	}

	/**
	 * Creates a new {@link Property} associated with this property container. This
	 * property can be used as a lookup in other expressions. It does not add a value to
	 * the property.
	 * <p>
	 * The new {@link Property} object is a dynamic lookup, based on the
	 * {@code expression} passed to this method. The expression can be example another
	 * property, a function result or a Cypher parameter. A property defined in such a way
	 * will render as {@code p[expression]}.
	 * <p>
	 * Note: The property container does not track property creation and there is no
	 * possibility to enumerate all properties that have been created for this property
	 * container.
	 * @param lookup the expression that is evaluated to lookup this property.
	 * @return a new {@link Property} associated with this named container
	 * @since 2024.1.0
	 */
	@Override
	default Property property(Expression lookup) {
		return InternalPropertyImpl.create(this, lookup);
	}

}
