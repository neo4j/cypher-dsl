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

import java.util.List;

import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Factory methods for creating predicates.
 *
 * @author Michael J. Simons
 * @since 1.0 be accessible.
 */
final class Predicates {

	private Predicates() {
	}

	/**
	 * Creates a new condition based on a function invocation for the {@code exists()}
	 * function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-exists">exists</a>.
	 * @param property the property to be passed to {@code exists()}
	 * @return a function call for {@code exists()} for one property
	 */
	static Condition exists(Property property) {

		return new BooleanFunctionCondition(FunctionInvocation.create(BuiltInFunctions.Predicates.EXISTS, property));
	}

	/**
	 * Creates a new condition based on a function invocation for the {@code exists()}
	 * function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-exists">exists</a>.
	 * @param pattern the pattern to be passed to {@code exists()}
	 * @return a function call for {@code exists()} for one pattern
	 */
	static Condition exists(RelationshipPattern pattern) {

		return new BooleanFunctionCondition(FunctionInvocation.create(BuiltInFunctions.Predicates.EXISTS, pattern));
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a>. The statement may or may not have a {@literal  RETURN} clause. It
	 * must however not contain any updates. While it would render syntactically correct
	 * Cypher, Neo4j does not support updates inside existential sub-queries.
	 * @param statement the statement to be passed to {@code exists{}}
	 * @param imports optional imports to be used in the statement (will be imported with
	 * {@literal WITH})
	 * @return an existential sub-query.
	 * @since 2023.1.0
	 */
	static Condition exists(Statement statement, IdentifiableElement... imports) {

		return ExistentialSubquery.exists(statement, imports);
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a> based on the list of patterns.
	 * @param pattern the pattern that must exists
	 * @return an existential sub-query.
	 * @since 2023.9.0
	 */
	static Condition exists(PatternElement pattern) {

		return ExistentialSubquery.exists(List.of(pattern), null);
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a> based on the list of patterns.
	 * @param pattern the list of patterns that must exists
	 * @return an existential sub-query.
	 * @since 2023.9.0
	 */
	static Condition exists(List<PatternElement> pattern) {

		return ExistentialSubquery.exists(pattern, null);
	}

	/**
	 * Creates a new condition via an <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/syntax/expressions/#existential-subqueries">existential
	 * sub-query</a> based on the list of patterns and an optional {@link Where
	 * where-clause}.
	 * @param pattern the list of patterns that must exists
	 * @param where an optional where-clause
	 * @return an existential sub-query.
	 * @since 2023.9.0
	 */
	static Condition exists(List<PatternElement> pattern, Where where) {

		return ExistentialSubquery.exists(pattern, where);
	}

	/**
	 * Creates the {@literal ALL} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code all()} predicate function
	 * @since 1.1
	 * @see #all(SymbolicName)
	 */
	static OngoingListBasedPredicateFunction all(String variable) {

		return all(SymbolicName.of(variable));
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code all()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-all">exists</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code all()} predicate function
	 * @since 1.1
	 */
	static OngoingListBasedPredicateFunction all(SymbolicName variable) {

		return new Builder(BuiltInFunctions.Predicates.ALL, variable);
	}

	/**
	 * Creates the {@literal ANY} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code any()} predicate function
	 * @since 1.1
	 * @see #any(SymbolicName)
	 */
	static OngoingListBasedPredicateFunction any(String variable) {

		return any(SymbolicName.of(variable));
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code any()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-any">exists</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code any()} predicate function
	 * @since 1.1
	 */
	static OngoingListBasedPredicateFunction any(SymbolicName variable) {

		return new Builder(BuiltInFunctions.Predicates.ANY, variable);
	}

	/**
	 * Creates the {@literal NONE} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code none()} predicate function
	 * @since 1.1
	 * @see #none(SymbolicName)
	 */
	static OngoingListBasedPredicateFunction none(String variable) {

		return none(SymbolicName.of(variable));
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code none()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-none">exists</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code none()} predicate function
	 * @since 1.1
	 */
	static OngoingListBasedPredicateFunction none(SymbolicName variable) {

		return new Builder(BuiltInFunctions.Predicates.NONE, variable);
	}

	/**
	 * Creates the {@literal SINGLE} predicate.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code single()} predicate function
	 * @since 1.1
	 * @see #single(SymbolicName)
	 */
	static OngoingListBasedPredicateFunction single(String variable) {

		return single(SymbolicName.of(variable));
	}

	/**
	 * Starts building a new condition based on a function invocation for the
	 * {@code single()} function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-single">exists</a>.
	 * @param variable the variable referring to elements of a list
	 * @return a builder for the {@code single()} predicate function
	 * @since 1.1
	 */
	static OngoingListBasedPredicateFunction single(SymbolicName variable) {

		return new Builder(BuiltInFunctions.Predicates.SINGLE, variable);
	}

	/**
	 * Creates a new condition based on a function invocation for the {@code isEmpty()}
	 * function. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/current/functions/predicate/#functions-isempty">isEmpty</a>.
	 * <p>
	 * The argument {@code e} must refer to an expression that evaluates to a list for
	 * {@code isEmpty()} to work
	 * @param e an expression referring to a list
	 * @return a function call for {@code isEmpty()} for a list
	 * @since 2023.6.1
	 */
	static Condition isEmpty(Expression e) {

		return new BooleanFunctionCondition(FunctionInvocation.create(BuiltInFunctions.Predicates.IS_EMPTY, e));
	}

	private static class Builder
			implements OngoingListBasedPredicateFunction, OngoingListBasedPredicateFunctionWithList {

		private final BuiltInFunctions.Predicates predicate;

		private final SymbolicName name;

		private Expression listExpression;

		Builder(BuiltInFunctions.Predicates predicate, SymbolicName name) {

			Assertions.notNull(predicate, "The predicate is required");
			Assertions.notNull(name, "The name is required");
			this.predicate = predicate;
			this.name = name;
		}

		@Override
		public OngoingListBasedPredicateFunctionWithList in(Expression list) {

			Assertions.notNull(list, "The list expression is required");
			this.listExpression = list;
			return this;
		}

		@Override
		public Condition where(Condition condition) {

			Assertions.notNull(condition, "The condition is required");
			return new BooleanFunctionCondition(FunctionInvocation.create(this.predicate,
					new ListPredicate(this.name, this.listExpression, new Where(condition))));
		}

	}

}
