/*
 * Copyright (c) 2019-2020 "Neo4j,"
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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ProcedureCall.OngoingInQueryCallWithoutArguments;
import org.neo4j.cypherdsl.core.support.Neo4jVersion;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public interface StatementBuilder
	extends ExposesMatch, ExposesCreate, ExposesMerge, ExposesUnwind, ExposesReturning, ExposesSubqueryCall {

	/**
	 * Starts a with clause by passing variables to it.
	 *
	 * @param variables The variables to start the query with
	 * @return An ongoing read, exposing return and further matches.
	 * @since 2020.1.2
	 */
	OrderableOngoingReadingAndWithWithoutWhere with(String... variables);

	/**
	 * Starts a with clause by passing named expressions to it.
	 *
	 * @param expressions The expressions to start the query with
	 * @return An ongoing read, exposing return and further matches.
	 * @since 2020.1.2
	 */
	OrderableOngoingReadingAndWithWithoutWhere with(Named... expressions);

	/**
	 * Allows for queries starting with {@code with range(1,10) as x return x} or similar.
	 *
	 * @param expressions The expressions to start the query with
	 * @return An ongoing read, exposing return and further matches.
	 */
	OrderableOngoingReadingAndWithWithoutWhere with(Expression... expressions);

	/**
	 * An ongoing update statement that can be used to chain more update statements or add a with or return clause.
	 *
	 * @since 1.0
	 */
	interface OngoingUpdate
		extends BuildableStatement, ExposesCreate, ExposesMerge, ExposesDelete, ExposesReturning, ExposesWith,
		ExposesMergeAction {
	}

	/**
	 * A shared marker interface for things that can be turned into a subquery to be used inside the WHERE clause.
	 *
	 * @since 2020.1.2
	 */
	interface ExposesExistentialSubqueryCall {

		/**
		 * This can be used against a 4.x database to turn this ongoing match statement into a condition to be used
		 * in an existential subquery.
		 *
		 * @return An existential subquery.
		 * @neo4j.version 4.0.0
		 */
		@Neo4jVersion(minimum = "4.0.0")
		Condition asCondition();
	}

	/**
	 * A match that exposes {@code returning} and {@code where} methods to add required information.
	 * While the where clause is optional, an returning clause needs to be specified before the
	 * statement can be build.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingWithoutWhere extends OngoingReading, ExposesWhere, ExposesMatch, ExposesExistentialSubqueryCall {
	}

	/**
	 * A match that has a non-empty {@code where}-part. THe returning clause is still open.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingWithWhere extends OngoingReading, ExposesMatch,
		ExposesLogicalOperators<OngoingReadingWithWhere>, ExposesExistentialSubqueryCall {
	}

	/**
	 * A match that exposes {@code returning} and for which it is not decided whether the optional
	 * where part has been used or note.
	 *
	 * @since 1.0
	 */
	interface OngoingReading
		extends ExposesReturning, ExposesWith, ExposesUpdatingClause, ExposesUnwind, ExposesCreate,
		ExposesCall<OngoingInQueryCallWithoutArguments>, ExposesSubqueryCall {
	}

	/**
	 * Builder part for unwinding.
	 *
	 * @since 1.0
	 */
	interface OngoingUnwind {

		/**
		 * Adds an {@code AS} part that allows to define an alias for the iterable being unwound.
		 *
		 * @param variable The alias name
		 * @return A normal, ongoing read.
		 */
		OngoingReading as(String variable);
	}

	/**
	 * A match that knows what to return and which is ready to be build.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingAndReturn
		extends TerminalExposesOrderBy, TerminalExposesSkip, TerminalExposesLimit, BuildableStatement {
	}

	/**
	 * A match that knows what to pipe to the next part of a multi part query.
	 *
	 * @since 1.0
	 */
	interface OrderableOngoingReadingAndWithWithoutWhere extends OrderableOngoingReadingAndWith {

		/**
		 * Adds a where clause to this match.
		 *
		 * @param condition The new condition, must not be {@literal null}
		 * @return A match restricted by a where clause with no return items yet.
		 */
		OrderableOngoingReadingAndWithWithWhere where(Condition condition);

		/**
		 * Adds a where clause based on a path pattern to this match.
		 * See <a href="https://neo4j.com/docs/cypher-manual/4.0/clauses/where/#query-where-patterns">Using path patterns in WHERE</a>.
		 *
		 * @param pathPattern The path pattern to add to the where clause.
		 *                    This path pattern must not be {@literal null} and must
		 *                    not introduce new variables not available in the match.
		 * @return A match restricted by a where clause with no return items yet.
		 * @since 1.0.1
		 */
		default OrderableOngoingReadingAndWithWithWhere where(RelationshipPattern pathPattern) {

			Assertions.notNull(pathPattern, "The path pattern must not be null.");
			return this.where(new RelationshipPatternCondition(pathPattern));
		}
	}

	/**
	 * @see OrderableOngoingReadingAndWith
	 * @see ExposesLogicalOperators
	 * @since 1.0
	 */
	interface OrderableOngoingReadingAndWithWithWhere
		extends OrderableOngoingReadingAndWith, ExposesLogicalOperators<OrderableOngoingReadingAndWithWithWhere> {
	}

	/**
	 * Represents a reading statement ending in a with clause, potentially already having an order and not exposing
	 * order methods.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingAndWith extends OngoingReading, ExposesMatch {
	}

	/**
	 * @see OngoingReadingAndWith
	 * @since 1.0
	 */
	interface OrderableOngoingReadingAndWith
		extends ExposesOrderBy, ExposesSkip, ExposesLimit, OngoingReadingAndWith {
	}

	/**
	 * Combines the capabilities of skip, limit and adds additional expressions to the order-by items.
	 *
	 * @since 1.0
	 */
	interface OngoingMatchAndReturnWithOrder extends TerminalExposesSkip, TerminalExposesLimit, BuildableStatement {

		/**
		 * Adds another expression to the list of order items.
		 *
		 * @param expression The expression that is added with an {@literal AND}
		 * @return A new order specifying step.
		 */
		TerminalOngoingOrderDefinition and(Expression expression);
	}

	/**
	 * An intermediate step while defining the order of a result set. This definitional will eventually return a
	 * buildable statement and thus is terminal.
	 *
	 * @since 1.0
	 */
	interface TerminalOngoingOrderDefinition extends TerminalExposesSkip, TerminalExposesLimit, BuildableStatement {

		/**
		 * Specifies descending order and jumps back to defining the match and return statement.
		 *
		 * @param <T> The type of the next step
		 * @return The ongoing definition of a match
		 */
		<T extends TerminalExposesSkip & TerminalExposesLimit & OngoingMatchAndReturnWithOrder> T descending();

		/**
		 * Specifies ascending order and jumps back to defining the match and return statement.
		 *
		 * @param <T> The type of the next step
		 * @return The ongoing definition of a match
		 */
		<T extends TerminalExposesSkip & TerminalExposesLimit & OngoingMatchAndReturnWithOrder> T ascending();
	}

	/**
	 * Combines the capabilities of skip, limit and adds additional expressions to the order-by items.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingAndWithWithWhereAndOrder extends ExposesSkip, ExposesLimit,
		OngoingReadingAndWith {

		/**
		 * Adds another expression to the list of order items.
		 *
		 * @param expression The expression that is added with an {@literal AND}
		 * @return A new order specifying step.
		 */
		OngoingOrderDefinition and(Expression expression);
	}

	/**
	 * An intermediate step while defining the order of a with clause.
	 *
	 * @since 1.0
	 */
	interface OngoingOrderDefinition extends ExposesSkip, ExposesLimit {

		/**
		 * Specifies descending order and jumps back to defining the match and return statement.
		 *
		 * @param <T> The type of the next step
		 * @return The ongoing definition of a match
		 */
		<T extends ExposesSkip & ExposesLimit & OngoingReadingAndWithWithWhereAndOrder> T descending();

		/**
		 * Specifies ascending order and jumps back to defining the match and return statement.
		 *
		 * @param <T> The type of the next step
		 * @return The ongoing definition of a match
		 */
		<T extends ExposesSkip & ExposesLimit & OngoingReadingAndWithWithWhereAndOrder> T ascending();
	}

	/**
	 * A statement that has all information required to be build and exposes a build method.
	 *
	 * @since 1.0
	 */
	interface BuildableStatement {

		/**
		 * @return The statement ready to be used, i.e. in a renderer.
		 */
		Statement build();

		/**
		 * @return Creates a statement that returns an explain plan for the original statement.
		 * @since 2020.1.2
		 */
		default Statement explain() {

			return DecoratedQuery.explain(build());
		}

		/**
		 * @return Creates a profiled statement that includes both the result and the actually executed and profiled plan.
		 * @since 2020.1.2
		 */
		default Statement profile() {

			return DecoratedQuery.profile(build());
		}
	}

	/**
	 * A step that exposes the {@code WITH} clause.
	 *
	 * @since 1.0
	 */
	interface ExposesWith {

		/**
		 * @param variables The variables to pass on to the next part
		 * @return A match that can be build now
		 * @see #with(Expression...)
		 */
		default OrderableOngoingReadingAndWithWithoutWhere with(String... variables) {
			return with(Expressions.createSymbolicNames(variables));
		}

		/**
		 * @param variables The variables to pass on to the next part
		 * @return A match that can be build now
		 * @see #with(Expression...)
		 */
		default OrderableOngoingReadingAndWithWithoutWhere with(Named... variables) {
			return with(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Create a match that returns one or more expressions.
		 *
		 * @param expressions The expressions to be returned. Must not be null and be at least one expression.
		 * @return A match that can be build now
		 */
		OrderableOngoingReadingAndWithWithoutWhere with(Expression... expressions);

		/**
		 * @param variables The variables to pass on to the next part
		 * @return A match that can be build now
		 * @see #withDistinct(Expression...)
		 */
		default OrderableOngoingReadingAndWithWithoutWhere withDistinct(String... variables) {
			return withDistinct(Expressions.createSymbolicNames(variables));
		}

		/**
		 * @param variables The variables to pass on to the next part
		 * @return A match that can be build now
		 * @see #withDistinct(Expression...)
		 */
		default OrderableOngoingReadingAndWithWithoutWhere withDistinct(Named... variables) {
			return withDistinct(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Create a match that returns the distinct set of one or more expressions.
		 *
		 * @param expressions The expressions to be returned. Must not be null and be at least one expression.
		 * @return A match that can be build now
		 */
		OrderableOngoingReadingAndWithWithoutWhere withDistinct(Expression... expressions);
	}

	/**
	 * A step that exposes several methods to specify ordering. This is a terminal operation just before a statement
	 * is buildable.
	 *
	 * @since 1.0
	 */
	interface TerminalExposesOrderBy {

		/**
		 * Order the result set by one or more {@link SortItem sort items}. Those can be retrieved for
		 * all expression with {@link Cypher#sort(Expression)} or directly from properties.
		 *
		 * @param sortItem One or more sort items
		 * @return A build step that still offers methods for defining skip and limit
		 */
		OngoingMatchAndReturnWithOrder orderBy(SortItem... sortItem);

		/**
		 * Order the result set by an expression.
		 *
		 * @param expression The expression to order by
		 * @return A step that allows for adding more expression or fine-tuning the sort direction of the last expression
		 */
		TerminalOngoingOrderDefinition orderBy(Expression expression);
	}

	/**
	 * A step that exposes the {@link #skip(Number)} method.
	 *
	 * @since 1.0
	 */
	interface TerminalExposesSkip {

		/**
		 * Adds a skip clause, skipping the given number of records.
		 *
		 * @param number How many records to skip. If this is null, then no records are skipped.
		 * @param <T>    The type of the next step
		 * @return A step that only allows the limit of records to be specified.
		 */
		<T extends TerminalExposesLimit & BuildableStatement> T skip(Number number);

	}

	/**
	 * A step that exposes the {@link #limit(Number)} method.
	 *
	 * @since 1.0
	 */
	interface TerminalExposesLimit {

		/**
		 * Limits the number of returned records.
		 *
		 * @param number How many records to return. If this is null, all the records are returned.
		 * @return A buildable match statement.
		 */
		BuildableStatement limit(Number number);
	}

	/**
	 * See {@link TerminalExposesOrderBy}, but on a with clause.
	 *
	 * @since 1.0
	 */
	interface ExposesOrderBy {

		/**
		 * Order the result set by one or more {@link SortItem sort items}. Those can be retrieved for
		 * all expression with {@link Cypher#sort(Expression)} or directly from properties.
		 *
		 * @param sortItem One or more sort items
		 * @return A build step that still offers methods for defining skip and limit
		 */
		OrderableOngoingReadingAndWithWithWhere orderBy(SortItem... sortItem);

		/**
		 * Order the result set by an expression.
		 *
		 * @param expression The expression to order by
		 * @return A step that allows for adding more expression or fine-tuning the sort direction of the last expression
		 */
		OngoingOrderDefinition orderBy(Expression expression);
	}

	/**
	 * A step that exposes the {@link #skip(Number)} method.
	 *
	 * @since 1.0
	 */
	interface ExposesSkip {

		/**
		 * Adds a skip clause, skipping the given number of records.
		 *
		 * @param number How many records to skip. If this is null, then no records are skipped.
		 * @param <T>    The type of the next step
		 * @return A step that only allows the limit of records to be specified.
		 */
		<T extends ExposesLimit & OngoingReadingAndWith> T skip(Number number);

	}

	/**
	 * A step that exposes the {@link #limit(Number)} method.
	 *
	 * @since 1.0
	 */
	interface ExposesLimit {

		/**
		 * Limits the number of returned records.
		 *
		 * @param number How many records to return. If this is null, all the records are returned.
		 * @return A buildable match statement.
		 */
		OngoingReadingAndWith limit(Number number);
	}

	/**
	 * A step providing all the supported updating clauses (DELETE, SET)
	 *
	 * @since 1.0
	 */
	interface ExposesUpdatingClause extends ExposesDelete, ExposesMerge, ExposesSetAndRemove {
	}

	/**
	 * A step that exposes only the delete clause.
	 *
	 * @since 1.0
	 */
	interface ExposesDelete {

		/**
		 * Renders a {@code DELETE} clause targeting the given variables. NO checks are done whether they have been matched
		 * previously.
		 *
		 * @param variables Variables indicating the things to delete.
		 * @param <T> The type of the next step, can be used to build the statement or add further instructions.
		 * @return A match with a delete clause that can be build now
		 */
		default <T extends OngoingUpdate & BuildableStatement> T delete(String... variables) {
			return delete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Renders a {@code DELETE} clause targeting the given variables. NO checks are done whether they have been matched
		 * previously.
		 *
		 * @param variables Variables indicating the things to delete.
		 * @param <T> The type of the next step, can be used to build the statement or add further instructions.
		 * @return A match with a delete clause that can be build now
		 */
		default <T extends OngoingUpdate & BuildableStatement> T delete(Named... variables) {
			return delete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Creates a delete step with one or more expressions to be deleted.
		 *
		 * @param expressions The expressions to be deleted.
		 * @param <T>         The type of the next step
		 * @return A match with a delete clause that can be build now
		 */
		<T extends OngoingUpdate & BuildableStatement> T delete(Expression... expressions);

		/**
		 * Renders a {@code DETACH DELETE} clause targeting the given variables. NO checks are done whether they have
		 * been matched previously.
		 *
		 * @param variables Variables indicating the things to delete.
		 * @param <T> The type of the next step, can be used to build the statement or add further instructions.
		 * @return A match with a detach delete clause that can be build now
		 */
		default <T extends OngoingUpdate & BuildableStatement> T detachDelete(String... variables) {
			return detachDelete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Renders a {@code DETACH DELETE} clause targeting the given variables. NO checks are done whether they have
		 * been matched previously.
		 *
		 * @param variables Variables indicating the things to delete.
		 * @param <T> The type of the next step, can be used to build the statement or add further instructions.
		 * @return A match with a detach delete clause that can be build now
		 */
		default <T extends OngoingUpdate & BuildableStatement> T detachDelete(Named... variables) {
			return detachDelete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Starts building a delete step that will use {@code DETACH} to remove relationships.
		 *
		 * @param expressions The expressions to be deleted.
		 * @param <T>         The type of the next step
		 * @return A match with a delete clause that can be build now
		 */
		<T extends OngoingUpdate & BuildableStatement> T detachDelete(Expression... expressions);
	}

	/**
	 * Set part of a statement.
	 *
	 * @since 1.0
	 */
	interface ExposesSet {

		/**
		 * Adds a {@code SET} clause to the statement. The list of expressions must be even, each pair will be turned into
		 * SET operation.
		 *
		 * @param expressions The list of expressions to use in a set clause.
		 * @param <T>         The type of the next step
		 * @return An ongoing match and update
		 */
		<T extends OngoingMatchAndUpdate & BuildableStatement> T set(Expression... expressions);

		/**
		 * Adds a {@code SET} clause to the statement, modifying the given named thing with an expression.
		 *
		 * @param variable   The named thing to modify
		 * @param expression The modifying expression
		 * @param <T>        The type of the next step
		 * @return An ongoing match and update
		 */
		default <T extends OngoingMatchAndUpdate & BuildableStatement> T set(Named variable, Expression expression) {
			return set(variable.getRequiredSymbolicName(), expression);
		}
	}

	/**
	 * A step that exposes the set clause.
	 *
	 * @since 1.0
	 */
	interface ExposesSetAndRemove extends ExposesSet {

		/**
		 * Creates {@code SET} clause for setting the given labels to a node.
		 *
		 * @param node   The node who's labels are to be changed
		 * @param labels The labels to be set
		 * @param <T>    The type of the next step, can be used to build the statement or add further instructions.
		 * @return A match with a SET clause that can be build now
		 */
		<T extends OngoingMatchAndUpdate & BuildableStatement> T set(Node node, String... labels);

		/**
		 * Creates {@code SET} clause for removing the given labels from a node.
		 *
		 * @param node   The node who's labels are to be changed
		 * @param labels The labels to be removed
		 * @param <T>    The type of the next step, can be used to build the statement or add further instructions.
		 * @return A match with a REMOVE clause that can be build now
		 */
		<T extends OngoingMatchAndUpdate & BuildableStatement> T remove(Node node, String... labels);

		/**
		 * Creates {@code SET} clause for removing the enumerated properties
		 *
		 * @param properties The properties to be removed
		 * @param <T>        The type of the next step, can be used to build the statement or add further instructions.
		 * @return A match with a REMOVE clause that can be build now
		 */
		<T extends OngoingMatchAndUpdate & BuildableStatement> T remove(Property... properties);
	}

	/**
	 * A buildable step that will create a MATCH ... DELETE statement.
	 *
	 * @since 1.0
	 */
	interface OngoingMatchAndUpdate extends ExposesReturning, ExposesWith, ExposesUpdatingClause, ExposesCreate {
	}

	/**
	 * Provides a way to specify an action that happens after a {@code MERGE} clause.
	 *
	 * @since 2020.1.2
	 */
	interface ExposesMergeAction {

		/**
		 * This allows to specify the action that should happen when the merge clause lead to the creation of a new pattern.
		 *
		 * @return an ongoing definition of a merge action.
		 */
		OngoingMergeAction onCreate();

		/**
		 * This allows to specify the action that should happen when the pattern of the merge clause already existed and matched.
		 *
		 * @return an ongoing definition of a merge action.
		 */
		OngoingMergeAction onMatch();
	}

	/**
	 * A variant of {@link ExposesSet} that allows for further chaining of actions.
	 * @since 2020.1.2
	 */
	interface OngoingMergeAction {

		/**
		 * Adds a {@code SET} clause to the statement. The list of expressions must be even, each pair will be turned into
		 * SET operation.
		 *
		 * @param expressions The list of expressions to use in a set clause.
		 * @param <T>         The type of the next step
		 * @return An ongoing match and update
		 */
		<T extends OngoingMatchAndUpdate & BuildableStatement & ExposesMergeAction> T set(Expression... expressions);

		/**
		 * Adds a {@code SET} clause to the statement, modifying the given named thing with an expression.
		 *
		 * @param variable   The named thing to modify
		 * @param expression The modifying expression
		 * @param <T>        The type of the next step
		 * @return An ongoing match and update
		 */
		default <T extends OngoingMatchAndUpdate & BuildableStatement & ExposesMergeAction> T set(Named variable, Expression expression) {
			return set(variable.getRequiredSymbolicName(), expression);
		}
	}
}
