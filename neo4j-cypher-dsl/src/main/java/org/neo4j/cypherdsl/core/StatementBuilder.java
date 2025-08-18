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

import java.util.Collection;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;
import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * The statement builder is the union of several other builders that all expose various
 * clauses.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @author Andreas Berger
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface StatementBuilder extends ExposesMatch, ExposesCreate, ExposesMerge, ExposesUnwind, ExposesReturning,
		ExposesFinish, ExposesSubqueryCall, ExposesWith {

	/**
	 * An ongoing update statement that can be used to chain more update statements or add
	 * a with or return clause.
	 *
	 * @since 1.0
	 */
	interface OngoingUpdate extends BuildableStatement<Statement>, ExposesCreate, ExposesMerge, ExposesDelete,
			ExposesReturning, ExposesFinish, ExposesWith, ExposesSet, ExposesForeach {

	}

	/**
	 * An ongoing update statement that can be used to chain more updating statements,
	 * define actions on a merge or add a with or return clause.
	 *
	 * @since 2021.0.0
	 */
	interface OngoingMerge extends OngoingUpdate, ExposesMergeAction, ExposesSetAndRemove {

	}

	/**
	 * A shared marker interface for things that can be turned into a subquery to be used
	 * inside the WHERE clause.
	 *
	 * @since 2020.1.2
	 */
	interface ExposesExistentialSubqueryCall {

		/**
		 * This can be used against a 4.x database to turn this ongoing match statement
		 * into a condition to be used in an existential subquery.
		 * @return an existential subquery.
		 * @neo4j.version 4.0.0
		 */
		@Neo4jVersion(minimum = "4.0.0")
		Condition asCondition();

	}

	/**
	 * A match that exposes {@code returning} and {@code where} methods to add required
	 * information. While the where clause is optional, a returning clause needs to be
	 * specified before the statement can be built.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingWithoutWhere extends OngoingReading, ExposesHints,
			ExposesWhere<StatementBuilder.OngoingReadingWithWhere>, ExposesMatch, ExposesExistentialSubqueryCall {

	}

	/**
	 * A match that has a non-empty {@code where}-part. THe returning clause is still
	 * open.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingWithWhere extends OngoingReading, ExposesMatch,
			ExposesLogicalOperators<OngoingReadingWithWhere>, ExposesExistentialSubqueryCall {

	}

	/**
	 * A match that exposes {@code returning} and for which it is not decided whether the
	 * optional where part has been used or note.
	 *
	 * @since 1.0
	 */
	interface OngoingReading extends ExposesReturning, ExposesFinish, ExposesWith, ExposesUpdatingClause, ExposesUnwind,
			ExposesCreate, ExposesMatch, ExposesCall<OngoingInQueryCallWithoutArguments>, ExposesSubqueryCall {

	}

	/**
	 * Builder part for unwinding.
	 *
	 * @since 1.0
	 */
	interface OngoingUnwind {

		/**
		 * Adds an {@code AS} part that allows to define an alias for the iterable being
		 * unwound.
		 * @param variable the alias name
		 * @return a normal, ongoing read.
		 */
		@CheckReturnValue
		OngoingReading as(String variable);

		/**
		 * Reuse an existing symbolic name.
		 * @param variable a symbolic name
		 * @return a normal, ongoing read.
		 * @since 2021.0.2
		 */
		@CheckReturnValue
		default OngoingReading as(SymbolicName variable) {
			return as(variable.getValue());
		}

	}

	/**
	 * A match that knows what to return and which is ready to be build.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingAndReturn extends TerminalExposesOrderBy, TerminalExposesSkip, TerminalExposesLimit,
			BuildableStatement<ResultStatement> {

		/**
		 * Returns the set of identifiable expressions in the {@literal RETURN} clause,
		 * the final statement might have a different set.
		 * @return the set of identifiable expressions in the {@literal RETURN} clause
		 * @since 2021.3.2
		 */
		Collection<Expression> getIdentifiableExpressions();

	}

	/**
	 * A match that knows what to pipe to the next part of a multipart query.
	 *
	 * @since 1.0
	 */
	interface OrderableOngoingReadingAndWithWithoutWhere extends OrderableOngoingReadingAndWith {

		/**
		 * Adds a where clause to this match.
		 * @param condition the new condition, must not be {@literal null}
		 * @return a match restricted by a where clause with no return items yet.
		 */
		@CheckReturnValue
		OrderableOngoingReadingAndWithWithWhere where(Condition condition);

		/**
		 * Adds a where clause based on a path pattern to this match. See <a href=
		 * "https://neo4j.com/docs/cypher-manual/4.0/clauses/where/#query-where-patterns">Using
		 * path patterns in WHERE</a>.
		 * @param pathPattern the path pattern to add to the where clause. This path
		 * pattern must not be {@literal null} and must not introduce new variables not
		 * available in the match.
		 * @return a match restricted by a where clause with no return items yet.
		 * @since 1.0.1
		 */
		@CheckReturnValue
		default OrderableOngoingReadingAndWithWithWhere where(RelationshipPattern pathPattern) {

			Assertions.notNull(pathPattern, "The path pattern must not be null.");
			return this.where(RelationshipPatternCondition.of(pathPattern));
		}

	}

	/**
	 * Represents a reading statement ending in a combination of {@code WITH} and
	 * {@code WHERE} clauses.
	 *
	 * @since 1.0
	 * @see OrderableOngoingReadingAndWith
	 * @see ExposesLogicalOperators
	 */
	interface OrderableOngoingReadingAndWithWithWhere
			extends OrderableOngoingReadingAndWith, ExposesLogicalOperators<OrderableOngoingReadingAndWithWithWhere> {

	}

	/**
	 * Represents a reading statement ending in a with clause, potentially already having
	 * an order and not exposing order methods.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingAndWith extends OngoingReading, ExposesMatch, ExposesLoadCSV {

	}

	/**
	 * Represents a reading statement ending in a {@code WITH} clause.
	 *
	 * @since 1.0
	 * @see OngoingReadingAndWith
	 */
	interface OrderableOngoingReadingAndWith extends ExposesOrderBy, ExposesSkip, ExposesLimit, OngoingReadingAndWith {

		/**
		 * Returns the set of identifiable expressions in the {@literal WITH} clause. The
		 * final statement might have a different set.
		 * @return the set of identifiable expressions in the {@literal WITH} clause
		 * @since 2021.3.2
		 */
		Collection<Expression> getIdentifiableExpressions();

	}

	/**
	 * Combines the capabilities of skip, limit and adds additional expressions to the
	 * order-by items.
	 *
	 * @since 1.0
	 */
	interface OngoingMatchAndReturnWithOrder
			extends TerminalExposesSkip, TerminalExposesLimit, BuildableStatement<ResultStatement> {

		/**
		 * Adds another expression to the list of order items.
		 * @param expression the expression that is added with an {@literal AND}
		 * @return a new order specifying step.
		 */
		@CheckReturnValue
		TerminalOngoingOrderDefinition and(Expression expression);

	}

	/**
	 * An intermediate step while defining the order of a result set. This definitional
	 * will eventually return a buildable statement and thus is terminal.
	 *
	 * @since 1.0
	 */
	interface TerminalOngoingOrderDefinition
			extends TerminalExposesSkip, TerminalExposesLimit, BuildableStatement<ResultStatement> {

		/**
		 * Specifies descending order and jumps back to defining the match and return
		 * statement.
		 * @return the ongoing definition of a match
		 */
		@CheckReturnValue
		OngoingMatchAndReturnWithOrder descending();

		/**
		 * Specifies ascending order and jumps back to defining the match and return
		 * statement.
		 * @return the ongoing definition of a match
		 */
		@CheckReturnValue
		OngoingMatchAndReturnWithOrder ascending();

	}

	/**
	 * Combines the capabilities of skip, limit and adds additional expressions to the
	 * order-by items.
	 *
	 * @since 1.0
	 */
	interface OngoingReadingAndWithWithWhereAndOrder extends ExposesSkip, ExposesLimit, OngoingReadingAndWith {

		/**
		 * Adds another expression to the list of order items.
		 * @param expression the expression that is added with an {@literal AND}
		 * @return a new order specifying step.
		 */
		@CheckReturnValue
		OngoingOrderDefinition and(Expression expression);

	}

	/**
	 * An intermediate step while defining the order of a with clause.
	 *
	 * @since 1.0
	 */
	interface OngoingOrderDefinition extends ExposesSkip, ExposesLimit {

		/**
		 * Specifies descending order and jumps back to defining the match and return
		 * statement.
		 * @return the ongoing definition of a match
		 */
		@CheckReturnValue
		OngoingReadingAndWithWithWhereAndOrder descending();

		/**
		 * Specifies ascending order and jumps back to defining the match and return
		 * statement.
		 * @return the ongoing definition of a match
		 */
		@CheckReturnValue
		OngoingReadingAndWithWithWhereAndOrder ascending();

	}

	/**
	 * A statement that has all information required to be build and exposes a build
	 * method.
	 *
	 * @param <T> the type of the statement that is returned
	 * @since 1.0
	 */
	interface BuildableStatement<T extends Statement> {

		/**
		 * {@return the statement ready to be used, i.e. in a renderer}
		 */
		T build();

		/**
		 * Returns a statement decorated with a {@code EXPLAIN} clause.
		 * @return a decorated statement
		 * @since 2020.1.2
		 */
		default Statement explain() {

			return DecoratedQuery.explain(build());
		}

		/**
		 * Returns a statement decorated with a {@code PROFILE} clause.
		 * @return a decorated statement
		 * @since 2020.1.2
		 */
		default Statement profile() {

			return DecoratedQuery.profile(build());
		}

	}

	/**
	 * A step that exposes several methods to specify ordering. This is a terminal
	 * operation just before a statement is buildable.
	 *
	 * @since 1.0
	 */
	interface TerminalExposesOrderBy {

		/**
		 * Order the result set by one or more {@link SortItem sort items}. Those can be
		 * retrieved for all expression with {@link Cypher#sort(Expression)} or directly
		 * from properties.
		 * @param sortItem one or more sort items
		 * @return a build step that still offers methods for defining skip and limit
		 */
		@CheckReturnValue
		OngoingMatchAndReturnWithOrder orderBy(SortItem... sortItem);

		/**
		 * Order the result set by one or more {@link SortItem sort items}. Those can be
		 * retrieved for all expression with {@link Cypher#sort(Expression)} or directly
		 * from properties.
		 * @param sortItem one or more sort items
		 * @return a build step that still offers methods for defining skip and limit
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		OngoingMatchAndReturnWithOrder orderBy(Collection<SortItem> sortItem);

		/**
		 * Order the result set by an expression.
		 * @param expression the expression to order by
		 * @return a step that allows for adding more expression or fine-tuning the sort
		 * direction of the last expression
		 */
		@CheckReturnValue
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
		 * @param number how many records to skip. If this is null, then no records are
		 * skipped.
		 * @return a step that only allows the limit of records to be specified.
		 */
		@CheckReturnValue
		TerminalExposesLimit skip(Number number);

		/**
		 * Adds a skip clause.
		 * @param expression the expression to skip by
		 * @return a step that only allows the limit of records to be specified.
		 * @since 2021.0.0
		 */
		@CheckReturnValue
		TerminalExposesLimit skip(Expression expression);

	}

	/**
	 * A step that exposes the {@link #limit(Number)} method.
	 *
	 * @since 1.0
	 */
	interface TerminalExposesLimit extends BuildableStatement<ResultStatement> {

		/**
		 * Limits the number of returned records.
		 * @param number how many records to return. If this is null, all the records are
		 * returned.
		 * @return a buildable match statement.
		 */
		@CheckReturnValue
		BuildableStatement<ResultStatement> limit(Number number);

		/**
		 * Limits the number of returned records.
		 * @param expression how many records to return. If this is null, all the records
		 * are returned.
		 * @return a buildable match statement.
		 * @since 2021.0.0
		 */
		@CheckReturnValue
		BuildableStatement<ResultStatement> limit(Expression expression);

	}

	/**
	 * Terminal operation that only allows access to {@link BuildableStatement}. A marker
	 * interface to clarify the intention instead of just exposing the
	 * {@code BuildableStatement}.
	 *
	 * @since 2024.3.0
	 */
	interface Terminal extends BuildableStatement<Statement> {

	}

	/**
	 * See {@link TerminalExposesOrderBy}, but on a with clause.
	 *
	 * @since 1.0
	 */
	interface ExposesOrderBy {

		/**
		 * Order the result set by one or more {@link SortItem sort items}. Those can be
		 * retrieved for all expression with {@link Cypher#sort(Expression)} or directly
		 * from properties.
		 * @param sortItem one or more sort items
		 * @return a build step that still offers methods for defining skip and limit
		 */
		@CheckReturnValue
		OrderableOngoingReadingAndWithWithWhere orderBy(SortItem... sortItem);

		/**
		 * Order the result set by one or more {@link SortItem sort items}. Those can be
		 * retrieved for all expression with {@link Cypher#sort(Expression)} or directly
		 * from properties.
		 * @param sortItem one or more sort items
		 * @return a build step that still offers methods for defining skip and limit
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		OrderableOngoingReadingAndWithWithWhere orderBy(Collection<SortItem> sortItem);

		/**
		 * Order the result set by an expression.
		 * @param expression the expression to order by
		 * @return a step that allows for adding more expression or fine-tuning the sort
		 * direction of the last expression
		 */
		@CheckReturnValue
		OngoingOrderDefinition orderBy(Expression expression);

	}

	/**
	 * The union type of an ongoing reading with a WITH and a SKIP clause.
	 *
	 * @since 2021.0.0
	 */
	interface OngoingReadingAndWithWithSkip extends OngoingReadingAndWith, ExposesLimit {

	}

	/**
	 * A step that exposes the {@link #skip(Number)} method.
	 *
	 * @since 1.0
	 */
	interface ExposesSkip {

		/**
		 * Adds a skip clause, skipping the given number of records.
		 * @param number how many records to skip. If this is null, then no records are
		 * skipped.
		 * @return a step that only allows the limit of records to be specified.
		 */
		@CheckReturnValue
		OngoingReadingAndWithWithSkip skip(Number number);

		/**
		 * Adds a skip clause.
		 * @param expression how many records to skip. If this is null, then no records
		 * are skipped.
		 * @return a step that only allows the limit of records to be specified.
		 * @since 2021.0.0
		 */
		@CheckReturnValue
		OngoingReadingAndWithWithSkip skip(Expression expression);

	}

	/**
	 * A step that exposes the {@link #limit(Number)} method.
	 *
	 * @since 1.0
	 */
	interface ExposesLimit {

		/**
		 * Limits the number of returned records.
		 * @param number how many records to return. If this is null, all the records are
		 * returned.
		 * @return a buildable match statement.
		 */
		@CheckReturnValue
		OngoingReadingAndWith limit(Number number);

		/**
		 * Limits the number of returned records.
		 * @param expression how many records to return. If this is null, all the records
		 * are returned.
		 * @return a buildable match statement.
		 * @since 2021.0.0
		 */
		@CheckReturnValue
		OngoingReadingAndWith limit(Expression expression);

	}

	/**
	 * Steps for building a {@link Foreach} clause.
	 *
	 * @since 2023.4.0
	 */
	interface ExposesForeach {

		/**
		 * Starts defining a {@link Foreach} clause.
		 * @param variable the variable to use in the iterator
		 * @return a step for selecting the source of iteration
		 * @since 2023.4.0
		 */
		ForeachSourceStep foreach(SymbolicName variable);

	}

	/**
	 * Initial step of defining a {@link Foreach FOREACH-clause}.
	 *
	 * @since 2023.4.0
	 */
	sealed interface ForeachSourceStep permits DefaultStatementBuilder.ForeachBuilder {

		/**
		 * Defines the source to be iterated by {@code FOREACH}. Must evaluate to
		 * something iterable, for example something like {@link Cypher#nodes(NamedPath)}
		 * for
		 * @param list the expression to iterate on
		 * @return the next step.
		 */
		ForeachUpdateStep in(Expression list);

	}

	/**
	 * Second step of defining a {@link Foreach FOREACH-clause} in which the updating
	 * clause is defined.
	 *
	 * @since 2023.4.0
	 */
	sealed interface ForeachUpdateStep permits DefaultStatementBuilder.ForeachBuilder {

		/**
		 * Defines the updating clause that to will be applied to every item in the source
		 * expression.
		 * @param updatingClauses the updating clauses to apply
		 * @return the final {@link Foreach} clause
		 */
		OngoingUpdate apply(UpdatingClause... updatingClauses);

	}

	/**
	 * A step providing all the supported updating clauses (DELETE, SET).
	 *
	 * @since 1.0
	 */
	interface ExposesUpdatingClause extends ExposesDelete, ExposesMerge, ExposesSetAndRemove, ExposesForeach {

	}

	/**
	 * A step that exposes only the {@code DELETE} clause.
	 *
	 * @since 1.0
	 */
	interface ExposesDelete {

		/**
		 * Renders a {@code DELETE} clause targeting the given variables. NO checks are
		 * done whether they have been matched previously.
		 * @param variables variables indicating the things to delete.
		 * @return a match with a {@literal DELETE} clause that can be build now
		 */
		@CheckReturnValue
		default OngoingUpdate delete(String... variables) {
			return delete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Renders a {@code DELETE} clause targeting the given variables. NO checks are
		 * done whether they have been matched previously.
		 * @param variables variables indicating the things to delete.
		 * @return a match with a {@literal DELETE} clause that can be build now
		 */
		@CheckReturnValue
		default OngoingUpdate delete(Named... variables) {
			return delete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Creates a delete step with one or more expressions to be deleted.
		 * @param expressions the expressions to be deleted.
		 * @return a match with a {@literal DELETE} clause that can be build now
		 */
		@CheckReturnValue
		OngoingUpdate delete(Expression... expressions);

		/**
		 * Creates a delete step with one or more expressions to be deleted.
		 * @param expressions the expressions to be deleted.
		 * @return a match with a {@literal DELETE} clause that can be build now
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		OngoingUpdate delete(Collection<? extends Expression> expressions);

		/**
		 * Renders a {@code DETACH DELETE} clause targeting the given variables. NO checks
		 * are done whether they have been matched previously.
		 * @param variables variables indicating the things to delete.
		 * @return a match with a {@literal DETACH DELETE} clause that can be build now
		 */
		@CheckReturnValue
		default OngoingUpdate detachDelete(String... variables) {
			return detachDelete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Renders a {@code DETACH DELETE} clause targeting the given variables. NO checks
		 * are done whether they have been matched previously.
		 * @param variables variables indicating the things to delete.
		 * @return a match with a {@literal DETACH DELETE} clause that can be build now
		 */
		@CheckReturnValue
		default OngoingUpdate detachDelete(Named... variables) {
			return detachDelete(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Starts building a delete step that will use {@code DETACH} to remove
		 * relationships.
		 * @param expressions the expressions to be deleted.
		 * @return a match with {@literal DETACH DELETE} clause that can be build now
		 */
		@CheckReturnValue
		OngoingUpdate detachDelete(Expression... expressions);

		/**
		 * Starts building a delete step that will use {@code DETACH} to remove
		 * relationships.
		 * @param expressions the expressions to be deleted.
		 * @return a match with {@literal DETACH DELETE} clause that can be build now
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		OngoingUpdate detachDelete(Collection<? extends Expression> expressions);

	}

	/**
	 * Set part of a statement.
	 *
	 * @since 1.0
	 */
	interface ExposesSet {

		/**
		 * Adds a {@code SET} clause to the statement. The list of expressions must be
		 * even, each pair will be turned into SET operation.
		 * @param expressions the list of expressions to use in a set clause.
		 * @return an ongoing match and update
		 */
		@CheckReturnValue
		BuildableMatchAndUpdate set(Expression... expressions);

		/**
		 * Adds a {@code SET} clause to the statement. The list of expressions must be
		 * even, each pair will be turned into SET operation.
		 * @param expressions the list of expressions to use in a set clause.
		 * @return an ongoing match and update
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		BuildableMatchAndUpdate set(Collection<? extends Expression> expressions);

		/**
		 * Adds a {@code SET} clause to the statement, modifying the given named thing
		 * with an expression.
		 * @param variable the named thing to modify
		 * @param expression the modifying expression
		 * @return an ongoing match and update
		 */
		@CheckReturnValue
		default BuildableMatchAndUpdate set(Named variable, Expression expression) {
			return set(variable.getRequiredSymbolicName(), expression);
		}

		/**
		 * Creates a {@code +=} operation. The left hand side must resolve to a container
		 * (either a node or a relationship) of properties and the right hand side must be
		 * a map of new or updated properties
		 * @param target the target container that should be modified
		 * @param properties the new properties
		 * @return an ongoing match and update
		 * @since 2020.1.5
		 */
		@CheckReturnValue
		BuildableMatchAndUpdate mutate(Expression target, Expression properties);

		/**
		 * Creates a {@code +=} operation. The left hand side must resolve to a container
		 * (either a node or a relationship) of properties and the right hand side must be
		 * a map of new or updated properties
		 * @param variable the named thing to modify
		 * @param properties the new properties
		 * @return an ongoing match and update
		 * @since 2020.1.5
		 */
		@CheckReturnValue
		default BuildableMatchAndUpdate mutate(Named variable, Expression properties) {
			return mutate(variable.getRequiredSymbolicName(), properties);
		}

	}

	/**
	 * Exposes node mutations.
	 *
	 * @param <R> the type of the next step
	 * @since 2023.5.0
	 */
	interface ExposesSetLabel<R> {

		/**
		 * Creates {@code SET} clause for setting the given labels to a node.
		 * @param node the node whose labels are to be changed
		 * @param labels the labels to be set
		 * @return a match with a SET clause that can be build now
		 */
		@CheckReturnValue
		R set(Node node, String... labels);

		/**
		 * Creates {@code SET} clause for setting the given labels to a node.
		 * @param node the node whose labels are to be changed
		 * @param labels the labels to be set
		 * @return a match with a SET clause that can be build now
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		R set(Node node, Collection<String> labels);

	}

	/**
	 * A step that exposes the set clause.
	 *
	 * @since 1.0
	 */
	interface ExposesSetAndRemove extends ExposesSet, ExposesSetLabel<BuildableMatchAndUpdate> {

		/**
		 * Creates {@code SET} clause for removing the given labels from a node.
		 * @param node the node whose labels are to be changed
		 * @param labels the labels to be removed
		 * @return a match with a REMOVE clause that can be build now
		 */
		@CheckReturnValue
		BuildableMatchAndUpdate remove(Node node, String... labels);

		/**
		 * Creates {@code SET} clause for removing the given labels from a node.
		 * @param node the node whose labels are to be changed
		 * @param labels the labels to be removed
		 * @return a match with a REMOVE clause that can be build now
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		BuildableMatchAndUpdate remove(Node node, Collection<String> labels);

		/**
		 * Creates {@code SET} clause for removing the enumerated properties.
		 * @param properties the properties to be removed
		 * @return a match with a REMOVE clause that can be build now
		 */
		@CheckReturnValue
		BuildableMatchAndUpdate remove(Property... properties);

		/**
		 * Creates {@code SET} clause for removing the enumerated properties.
		 * @param properties the properties to be removed
		 * @return a match with a REMOVE clause that can be build now
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		BuildableMatchAndUpdate remove(Collection<Property> properties);

	}

	/**
	 * After a MATCH..UPDATE chain has been established, a RETURN can be added, a pipeline
	 * with WITH can be started or more mutating steps can be added.
	 *
	 * @since 1.0
	 */
	interface OngoingMatchAndUpdate
			extends ExposesReturning, ExposesFinish, ExposesWith, ExposesUpdatingClause, ExposesCreate {

	}

	/**
	 * A buildable ongoing MATCH and UPDATE.
	 *
	 * @since 2021.0.0
	 */
	interface BuildableMatchAndUpdate extends OngoingMatchAndUpdate, BuildableStatement<Statement> {

	}

	/**
	 * Provides a way to specify an action that happens after a {@code MERGE} clause.
	 *
	 * @since 2020.1.2
	 */
	interface ExposesMergeAction {

		/**
		 * This allows to specify the action that should happen when the merge clause lead
		 * to the creation of a new pattern.
		 * @return an ongoing definition of a merge action.
		 */
		@CheckReturnValue
		OngoingMergeAction onCreate();

		/**
		 * This allows to specify the action that should happen when the pattern of the
		 * merge clause already existed and matched.
		 * @return an ongoing definition of a merge action.
		 */
		@CheckReturnValue
		OngoingMergeAction onMatch();

	}

	/**
	 * An interface combining a buildable MATCH and UPDATE with the possibility to add
	 * actions after a MERGE clause.
	 *
	 * @since 2021.0.0
	 */
	interface BuildableOngoingMergeAction extends BuildableMatchAndUpdate, ExposesMergeAction {

	}

	/**
	 * A variant of {@link ExposesSet} that allows for further chaining of actions.
	 *
	 * @since 2020.1.2
	 */
	interface OngoingMergeAction extends ExposesSetLabel<BuildableOngoingMergeAction> {

		/**
		 * Adds a {@code SET} clause to the statement. The list of expressions must be
		 * even, each pair will be turned into SET operation.
		 * @param expressions the list of expressions to use in a set clause.
		 * @return an ongoing match and update
		 */
		@CheckReturnValue
		BuildableOngoingMergeAction set(Expression... expressions);

		/**
		 * Adds a {@code SET} clause to the statement. The list of expressions must be
		 * even, each pair will be turned into SET operation.
		 * @param expressions the list of expressions to use in a set clause.
		 * @return an ongoing match and update
		 * @since 2021.2.2
		 */
		@CheckReturnValue
		BuildableOngoingMergeAction set(Collection<? extends Expression> expressions);

		/**
		 * Adds a {@code SET} clause to the statement, modifying the given named thing
		 * with an expression.
		 * @param variable the named thing to modify
		 * @param expression the modifying expression
		 * @return an ongoing match and update
		 */
		@CheckReturnValue
		default BuildableOngoingMergeAction set(Named variable, Expression expression) {
			return set(variable.getRequiredSymbolicName(), expression);
		}

		/**
		 * Creates a {@code +=} operation. The left hand side must resolve to a container
		 * (either a node or a relationship) of properties and the right hand side must be
		 * a map of new or updated properties
		 * @param target the target container that should be modified
		 * @param properties the new properties
		 * @return an ongoing match and update
		 * @since 2020.1.5
		 */
		@CheckReturnValue
		BuildableOngoingMergeAction mutate(Expression target, Expression properties);

		/**
		 * Creates a {@code +=} operation. The left hand side must resolve to a container
		 * (either a node or a relationship) of properties and the right hand side must be
		 * a map of new or updated properties
		 * @param variable the named thing to modify
		 * @param properties the new properties
		 * @return an ongoing match and update
		 * @since 2020.1.5
		 */
		@CheckReturnValue
		default BuildableOngoingMergeAction mutate(Named variable, Expression properties) {
			return mutate(variable.getRequiredSymbolicName(), properties);
		}

	}

	/**
	 * A trait for an ongoing standalone call to expose all of its results via an
	 * asterisk.
	 *
	 * @since 2022.8.3
	 */
	interface ExposesYieldStar {

		/**
		 * Mostly a helper method to indicate the overload as
		 * {@link org.neo4j.cypherdsl.core.ExposesCall.ExposesYield} uses vargs for all
		 * overloads, and that would not work nicely without arguments on this one here.
		 *
		 * Allows to use a {@literal *} in this standalone call.
		 * @param asterisk the actual * ;)
		 * @return the ongoing standalone call to be configured.
		 * @since 2022.8.0
		 */
		OngoingStandaloneCallWithReturnFields yield(Asterisk asterisk);

		/**
		 * Convenience method to yield all items of this standalone call.
		 * @return the ongoing standalone call to be configured.
		 * @since 2022.8.0
		 */
		default OngoingStandaloneCallWithReturnFields yieldStar() {
			return this.yield(Cypher.asterisk());
		}

	}

	/**
	 * The union of a buildable statement and call exposing new arguments and yields.
	 */
	interface OngoingStandaloneCallWithoutArguments extends StatementBuilder.BuildableStatement<Statement>,
			ExposesCall.ExposesWithArgs<OngoingStandaloneCallWithArguments>,
			ExposesCall.ExposesYield<OngoingStandaloneCallWithReturnFields>, ExposesCall.AsFunction, ExposesYieldStar {

		/**
		 * Turn this call into a void call to continue with querying.
		 * @return the call, continue with a normal query from here.
		 * @since 2022.4.0
		 */
		VoidCall withoutResults();

	}

	/**
	 * The union of a buildable statement and call exposing yields.
	 */
	interface OngoingStandaloneCallWithArguments extends StatementBuilder.BuildableStatement<Statement>,
			ExposesCall.ExposesYield<OngoingStandaloneCallWithReturnFields>, ExposesCall.AsFunction, ExposesYieldStar {

		/**
		 * Turn this call into a void call to continue with querying.
		 * @return the call, continue with a normal query from here.
		 * @since 2022.4.0
		 */
		VoidCall withoutResults();

	}

	/**
	 * A buildable statement exposing where and return clauses.
	 */
	sealed interface OngoingStandaloneCallWithReturnFields extends StatementBuilder.BuildableStatement<Statement>,
			ExposesMatch, ExposesWhere<StatementBuilder.OngoingReadingWithWhere>, ExposesReturning, ExposesFinish,
			ExposesWith, ExposesSubqueryCall, ExposesAndThen<OngoingStandaloneCallWithReturnFields, Statement>
			permits DefaultStatementBuilder.YieldingStandaloneCallBuilder {

	}

	/**
	 * The union of an in-query call exposing new arguments and yields.
	 */
	interface OngoingInQueryCallWithoutArguments extends ExposesCall.ExposesWithArgs<OngoingInQueryCallWithArguments>,
			ExposesCall.ExposesYield<OngoingInQueryCallWithReturnFields> {

		/**
		 * Turn this call into a void call to continue with querying.
		 * @return the call, continue with a normal query from here.
		 * @since 2022.4.0
		 */
		VoidCall withoutResults();

	}

	/**
	 * The union of an in-query call exposing yields.
	 */
	interface OngoingInQueryCallWithArguments extends ExposesCall.ExposesYield<OngoingInQueryCallWithReturnFields> {

		/**
		 * Turn this call into a void call to continue with querying.
		 * @return the call, continue with a normal query from here.
		 * @since 2022.4.0
		 */
		VoidCall withoutResults();

	}

	/**
	 * The result of a call to a stored procedure not having any results. It is possible
	 * to continue with "normal" querying after the execution of such a procedure.
	 *
	 * @since 2022.4.0
	 */
	interface VoidCall extends OngoingReading {

	}

	/**
	 * An in-query call exposing where and return clauses.
	 */
	interface OngoingInQueryCallWithReturnFields
			extends ExposesMatch, ExposesWhere<StatementBuilder.OngoingReadingWithWhere>, ExposesReturning,
			ExposesFinish, ExposesWith, ExposesSubqueryCall, ExposesForeach {

	}

}
