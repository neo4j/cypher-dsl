/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.StatementBuilder.BuildableMatchAndUpdate;
import org.neo4j.cypherdsl.core.StatementBuilder.BuildableOngoingMergeAction;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingMatchAndUpdate;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingMerge;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingReadingWithWhere;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingReadingWithoutWhere;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingUpdate;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.internal.ProcedureName;
import org.neo4j.cypherdsl.core.internal.YieldItems;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @author Romain Rossi
 * @since 1.0
 */
@API(status = INTERNAL, since = "2021.2.1")
class DefaultStatementBuilder implements StatementBuilder,
	OngoingUpdate, OngoingMerge, OngoingReadingWithWhere, OngoingReadingWithoutWhere, OngoingMatchAndUpdate,
	BuildableMatchAndUpdate,
	BuildableOngoingMergeAction, ExposesSubqueryCall.BuildableSubquery, StatementBuilder.VoidCall {

	/**
	 * Current list of reading or update clauses to be generated.
	 */
	private final List<Visitable> currentSinglePartElements = new ArrayList<>();

	/**
	 * The latest ongoing match.
	 */
	private MatchBuilder currentOngoingMatch;

	/**
	 * The latest ongoing update to be build
	 */
	private DefaultStatementWithUpdateBuilder currentOngoingUpdate;

	/**
	 * A list of already build withs.
	 */
	private final List<MultiPartElement> multiPartElements = new ArrayList<>();

	/**
	 * Default constructor. Builder can be pre-loaded with visitables.
	 *
	 * @param visitables A set of visitables. {@literal NULL} values will be skipped.
	 */
	DefaultStatementBuilder(Visitable... visitables) {

		addVisitables(visitables);
	}

	/**
	 * A copy constructor
	 * @param source The source
	 * @param visitables A set of additional visitables. {@literal NULL} values will be skipped.
	 */
	DefaultStatementBuilder(DefaultStatementBuilder source, Visitable... visitables) {

		this.currentSinglePartElements.addAll(source.currentSinglePartElements);
		this.currentOngoingMatch = source.currentOngoingMatch;
		this.currentOngoingUpdate = source.currentOngoingUpdate;
		this.multiPartElements.addAll(source.multiPartElements);

		addVisitables(visitables);
	}

	private void addVisitables(Visitable[] visitables) {
		for (Visitable visitable : visitables) {
			if (visitable != null) {
				this.currentSinglePartElements.add(visitable);
			}
		}
	}

	@NotNull
	@Override
	public final OngoingReadingWithoutWhere match(boolean optional, PatternElement... pattern) {

		Assertions.notNull(pattern, "Patterns to match are required.");
		Assertions.notEmpty(pattern, "At least one pattern to match is required.");

		this.closeCurrentOngoingMatch();

		this.currentOngoingMatch = new MatchBuilder(optional);
		this.currentOngoingMatch.patternList.addAll(Arrays.asList(pattern));
		return this;
	}

	@NotNull
	@Override
	public final OngoingUpdate create(PatternElement... pattern) {

		return update(UpdateType.CREATE, pattern);
	}

	@NotNull
	@Override
	public final OngoingUpdate create(Collection<? extends PatternElement> pattern) {

		return create(pattern.toArray(new PatternElement[] {}));
	}

	@NotNull
	@Override
	public final OngoingMerge merge(PatternElement... pattern) {

		return update(UpdateType.MERGE, pattern);
	}

	@NotNull
	@Override
	public final OngoingMergeAction onCreate() {
		return ongoingOnAfterMerge(MergeAction.Type.ON_CREATE);
	}

	@NotNull
	@Override
	public final OngoingMergeAction onMatch() {
		return ongoingOnAfterMerge(MergeAction.Type.ON_MATCH);
	}

	private OngoingMergeAction ongoingOnAfterMerge(MergeAction.Type type) {

		Assertions.notNull(this.currentOngoingUpdate, "MERGE must have been invoked before defining an event.");
		Assertions.isTrue(this.currentOngoingUpdate.builder instanceof SupportsActionsOnTheUpdatingClause,
			"MERGE must have been invoked before defining an event.");
		return new OngoingMergeAction() {

			@NotNull
			@Override
			public BuildableOngoingMergeAction mutate(Expression target, Expression properties) {
				((SupportsActionsOnTheUpdatingClause) DefaultStatementBuilder.this.currentOngoingUpdate.builder)
					.on(type, UpdateType.MUTATE, target, properties);
				return DefaultStatementBuilder.this;
			}

			@NotNull
			@Override
			public BuildableOngoingMergeAction set(Expression... expressions) {
				((SupportsActionsOnTheUpdatingClause) DefaultStatementBuilder.this.currentOngoingUpdate.builder)
					.on(type, UpdateType.SET, expressions);
				return DefaultStatementBuilder.this;
			}

			@NotNull
			@Override
			public BuildableOngoingMergeAction set(Collection<? extends Expression> expressions) {
				return set(expressions.toArray(new Expression[] {}));
			}
		};
	}

	@Override
	public final OngoingUnwind unwind(Expression expression) {

		closeCurrentOngoingMatch();

		return new DefaultOngoingUnwind(expression);
	}

	private DefaultStatementBuilder update(UpdateType updateType, Object[] pattern) {

		Assertions.notNull(pattern, "Patterns to create are required.");
		Assertions.notEmpty(pattern, "At least one pattern to create is required.");

		this.closeCurrentOngoingMatch();
		this.closeCurrentOngoingUpdate();

		if (pattern.getClass().getComponentType() == PatternElement.class) {
			this.currentOngoingUpdate = new DefaultStatementWithUpdateBuilder(updateType, (PatternElement[]) pattern);
		} else if (Expression.class.isAssignableFrom(pattern.getClass().getComponentType())) {
			this.currentOngoingUpdate = new DefaultStatementWithUpdateBuilder(updateType, (Expression[]) pattern);
		}

		return this;
	}

	@NotNull
	@Override
	public final OngoingReadingAndReturn returning(Collection<? extends Expression> elements) {

		return returning(false, false, elements);
	}

	@NotNull
	@Override
	public final OngoingReadingAndReturn returningDistinct(Collection<? extends Expression> elements) {

		return returning(false, true, elements);
	}

	@NotNull
	@Override
	public StatementBuilder.OngoingReadingAndReturn returningRaw(Expression rawExpression) {

		return new DefaultStatementWithReturnBuilder(rawExpression);
	}

	private OngoingReadingAndReturn returning(boolean raw, boolean distinct, Collection<? extends Expression> elements) {

		DefaultStatementWithReturnBuilder ongoingMatchAndReturn = new DefaultStatementWithReturnBuilder(raw, distinct);
		ongoingMatchAndReturn.addExpressions(elements);
		return ongoingMatchAndReturn;
	}

	@NotNull
	@Override
	public final OrderableOngoingReadingAndWithWithoutWhere with(Collection<IdentifiableElement> elements) {
		return with(false, elements);
	}

	@NotNull
	@Override
	public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Collection<IdentifiableElement> elements) {
		return with(true, elements);
	}

	private OrderableOngoingReadingAndWithWithoutWhere with(boolean distinct, Collection<IdentifiableElement> elements) {

		DefaultStatementWithWithBuilder ongoingMatchAndWith = new DefaultStatementWithWithBuilder(distinct);
		ongoingMatchAndWith.addElements(elements);
		return ongoingMatchAndWith;
	}

	@NotNull
	@Override
	public final OngoingUpdate delete(Expression... expressions) {

		return update(UpdateType.DELETE, expressions);
	}

	@NotNull
	@Override
	public final OngoingUpdate delete(Collection<? extends Expression> expressions) {

		return delete(expressions.toArray(new Expression[] {}));
	}

	@NotNull
	@Override
	public final OngoingUpdate detachDelete(Expression... expressions) {

		return update(UpdateType.DETACH_DELETE, expressions);
	}

	@NotNull
	@Override
	public final OngoingUpdate detachDelete(Collection<? extends Expression> expressions) {

		return detachDelete(expressions.toArray(new Expression[] {}));
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate set(Expression... expressions) {

		DefaultStatementWithUpdateBuilder result = new DefaultStatementWithUpdateBuilder(UpdateType.SET, expressions);
		this.closeCurrentOngoingUpdate();
		return result;
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate set(Collection<? extends Expression> expressions) {

		return set(expressions.toArray(new Expression[] {}));
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate set(Node named, String... labels) {

		return new DefaultStatementWithUpdateBuilder(UpdateType.SET, Operations.set(named, labels));
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate set(Node named, Collection<String> labels) {

		return set(named, labels.toArray(new String[] {}));
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate mutate(Expression target, Expression properties) {

		DefaultStatementWithUpdateBuilder result = new DefaultStatementWithUpdateBuilder(UpdateType.MUTATE, Operations.mutate(target, properties));
		this.closeCurrentOngoingUpdate();
		return result;
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate remove(Property... properties) {

		return new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE, properties);
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate remove(Collection<Property> properties) {

		return remove(properties.toArray(new Property[] {}));
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate remove(Node named, String... labels) {

		return new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE, Operations.remove(named, labels));
	}

	@NotNull
	@Override
	public final BuildableMatchAndUpdate remove(Node named, Collection<String> labels) {

		return remove(named, labels.toArray(new String[] {}));
	}

	@NotNull
	@Override
	public final OngoingReadingWithWhere where(Condition newCondition) {

		this.currentOngoingMatch.conditionBuilder.where(newCondition);
		return this;
	}

	@NotNull
	@Override
	public final OngoingReadingWithWhere and(Condition additionalCondition) {

		this.currentOngoingMatch.conditionBuilder.and(additionalCondition);
		return this;
	}

	@NotNull
	@Override
	public final OngoingReadingWithWhere or(Condition additionalCondition) {

		this.currentOngoingMatch.conditionBuilder.or(additionalCondition);
		return this;
	}

	@NotNull
	@Override
	public Statement build() {

		return buildImpl(null);
	}

	protected final Statement buildImpl(Return returning) {

		SinglePartQuery singlePartQuery = SinglePartQuery.create(
			buildListOfVisitables(false), returning);

		if (multiPartElements.isEmpty()) {
			return singlePartQuery;
		} else {
			return MultiPartQuery.create(multiPartElements, singlePartQuery);
		}
	}

	protected final List<Visitable> buildListOfVisitables(boolean clearAfter) {

		List<Visitable> visitables = new ArrayList<>(this.currentSinglePartElements);

		if (this.currentOngoingMatch != null) {
			visitables.add(this.currentOngoingMatch.buildMatch());
		}

		if (this.currentOngoingUpdate != null) {
			visitables.add(this.currentOngoingUpdate.builder.build());
		}

		if (clearAfter) {
			this.currentOngoingMatch = null;
			this.currentOngoingUpdate = null;
			this.currentSinglePartElements.clear();
		}
		return visitables;
	}

	@SuppressWarnings("OptionalUsedAsFieldOrParameterType")
	protected final DefaultStatementBuilder addWith(Optional<With> optionalWith) {

		optionalWith.ifPresent(with -> multiPartElements.add(new MultiPartElement(buildListOfVisitables(true), with)));
		return this;
	}

	protected final void addUpdatingClause(UpdatingClause updatingClause) {

		// Close current match
		closeCurrentOngoingMatch();

		this.currentSinglePartElements.add(updatingClause);
	}

	@NotNull
	@Override
	public StatementBuilder.OngoingReadingWithoutWhere call(Statement statement, IdentifiableElement... imports) {

		this.closeCurrentOngoingMatch();
		this.closeCurrentOngoingUpdate();

		this.currentSinglePartElements.add(Subquery.call(statement, imports));

		return this;
	}

	@NotNull
	@Override
	public BuildableSubquery callInTransactions(Statement statement, Integer rows, IdentifiableElement... imports) {

		this.closeCurrentOngoingMatch();
		this.closeCurrentOngoingUpdate();

		this.currentSinglePartElements.add(Subquery.call(statement, true, imports).inTransactionsOf(rows));

		return this;
	}

	private void closeCurrentOngoingMatch() {
		if (this.currentOngoingMatch == null) {
			return;
		}

		this.currentSinglePartElements.add(this.currentOngoingMatch.buildMatch());
		this.currentOngoingMatch = null;
	}

	private void closeCurrentOngoingUpdate() {
		if (this.currentOngoingUpdate == null) {
			return;
		}

		this.currentSinglePartElements.add(this.currentOngoingUpdate.builder.build());
		this.currentOngoingUpdate = null;
	}

	@NotNull
	@Override
	public final Condition asCondition() {

		if (this.currentOngoingMatch == null || !this.currentSinglePartElements.isEmpty()) {
			throw new IllegalArgumentException("Only simple MATCH statements can be used as existential subqueries.");
		}

		return ExistentialSubquery.exists(this.currentOngoingMatch.buildMatch());
	}

	@NotNull
	@Override
	public final OngoingReadingWithoutWhere usingIndex(Property... properties) {

		this.currentOngoingMatch.hints.add(Hint.useIndexFor(false, properties));
		return this;
	}

	@NotNull
	@Override
	public final OngoingReadingWithoutWhere usingIndexSeek(Property... properties) {

		this.currentOngoingMatch.hints.add(Hint.useIndexFor(true, properties));
		return this;
	}

	@NotNull
	@Override
	public final OngoingReadingWithoutWhere usingScan(Node node) {

		this.currentOngoingMatch.hints.add(Hint.useScanFor(node));
		return this;
	}

	@NotNull
	@Override
	public final OngoingReadingWithoutWhere usingJoinOn(SymbolicName... names) {

		this.currentOngoingMatch.hints.add(Hint.useJoinOn(names));
		return this;
	}

	abstract static class ReturnListWrapper {
		protected final List<Expression> returnList = new ArrayList<>();

		protected final void addElements(Collection<IdentifiableElement> elements) {

			Assertions.notNull(elements, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSIONS_REQUIRED));
			var filteredElements = elements.stream().filter(Objects::nonNull).map(IdentifiableElement::asExpression).toList();
			Assertions.isTrue(!filteredElements.isEmpty(), Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_AT_LEAST_ONE_EXPRESSION_REQUIRED));

			this.returnList.addAll(filteredElements);
		}

		protected final void addExpressions(Collection<? extends Expression> expressions) {

			Assertions.notNull(expressions, Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_EXPRESSIONS_REQUIRED));
			Assertions.isTrue(!expressions.isEmpty() && expressions.stream().noneMatch(Objects::isNull), Cypher.MESSAGES.getString(MessageKeys.ASSERTIONS_AT_LEAST_ONE_EXPRESSION_REQUIRED));

			this.returnList.addAll(expressions);
		}
	}

	protected class DefaultStatementWithReturnBuilder extends ReturnListWrapper
		implements OngoingReadingAndReturn, TerminalOngoingOrderDefinition, OngoingMatchAndReturnWithOrder {

		protected final OrderBuilder orderBuilder = new OrderBuilder();
		protected boolean rawReturn;
		protected boolean distinct;

		protected DefaultStatementWithReturnBuilder(Expression rawReturnExpression) {
			this.distinct = false;
			this.rawReturn = true;
			this.returnList.add(rawReturnExpression);
		}

		protected DefaultStatementWithReturnBuilder(boolean rawReturn, boolean distinct) {
			this.distinct = distinct;
			this.rawReturn = rawReturn;
		}

		@Override
		public Collection<Expression> getIdentifiableExpressions() {
			return extractIdentifiablesFromReturnList(returnList);
		}

		@NotNull
		@Override
		public final OngoingMatchAndReturnWithOrder orderBy(SortItem... sortItem) {
			orderBuilder.orderBy(sortItem);
			return this;
		}

		@NotNull
		@Override
		public final OngoingMatchAndReturnWithOrder orderBy(Collection<SortItem> sortItem) {
			return orderBy(sortItem.toArray(new SortItem[] {}));
		}

		@NotNull
		@Override
		public final TerminalOngoingOrderDefinition orderBy(@NotNull Expression expression) {
			orderBuilder.orderBy(expression);
			return this;
		}

		@NotNull
		@Override
		public final TerminalOngoingOrderDefinition and(@NotNull Expression expression) {
			orderBuilder.and(expression);
			return this;
		}

		@NotNull
		@Override
		public final DefaultStatementWithReturnBuilder descending() {
			orderBuilder.descending();
			return this;
		}

		@NotNull
		@Override
		public final DefaultStatementWithReturnBuilder ascending() {
			orderBuilder.ascending();
			return this;
		}

		@NotNull
		@Override
		public final OngoingReadingAndReturn skip(Number number) {
			return skip(number == null ? null : new NumberLiteral(number));
		}

		@NotNull
		@Override
		public final OngoingReadingAndReturn skip(Expression expression) {
			orderBuilder.skip(expression);
			return this;
		}

		@NotNull
		@Override
		public final OngoingReadingAndReturn limit(Number number) {
			return limit(number == null ? null : new NumberLiteral(number));
		}

		@NotNull
		@Override
		public final OngoingReadingAndReturn limit(Expression expression) {
			orderBuilder.limit(expression);
			return this;
		}

		@NotNull
		@Override
		public ResultStatement build() {

			Return returning = Return.create(rawReturn, distinct, returnList, orderBuilder);
			return (ResultStatement) DefaultStatementBuilder.this.buildImpl(returning);
		}
	}

	/**
	 * Helper class aggregating a couple of interface, collecting conditions and returned objects.
	 */
	protected final class DefaultStatementWithWithBuilder extends ReturnListWrapper
		implements OngoingOrderDefinition, OrderableOngoingReadingAndWithWithoutWhere,
		OrderableOngoingReadingAndWithWithWhere, OngoingReadingAndWithWithWhereAndOrder, OngoingReadingAndWithWithSkip {

		protected final ConditionBuilder conditionBuilder = new ConditionBuilder();
		protected final OrderBuilder orderBuilder = new OrderBuilder();
		protected boolean distinct;

		protected DefaultStatementWithWithBuilder(boolean distinct) {
			this.distinct = distinct;
		}

		protected Optional<With> buildWith() {

			if (returnList.isEmpty()) {
				return Optional.empty();
			}

			ExpressionList returnItems = new ExpressionList(returnList);

			Where where = conditionBuilder.buildCondition().map(Where::new).orElse(null);

			Optional<With> returnedWith = Optional
				.of(new With(distinct, returnItems, orderBuilder.buildOrder().orElse(null), orderBuilder.getSkip(),
					orderBuilder.getLimit(), where));
			this.returnList.clear();
			this.orderBuilder.reset();
			return returnedWith;
		}

		@Override
		public Collection<Expression> getIdentifiableExpressions() {
			return extractIdentifiablesFromReturnList(returnList);
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returning(Collection<? extends Expression> expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.returning(expressions);
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returningDistinct(Collection<? extends Expression> expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.returningDistinct(expressions);
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returningRaw(Expression rawExpression) {
			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.returningRaw(rawExpression);
		}

		@NotNull
		@Override
		public OngoingUpdate delete(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.delete(expressions);
		}

		@NotNull
		@Override
		public OngoingUpdate delete(Collection<? extends Expression> expressions) {

			return delete(expressions.toArray(new Expression[] {}));
		}

		@NotNull
		@Override
		public OngoingUpdate detachDelete(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.detachDelete(expressions);
		}

		@NotNull
		@Override
		public OngoingUpdate detachDelete(Collection<? extends Expression> expressions) {

			return detachDelete(expressions.toArray(new Expression[] {}));
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.set(expressions);
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Collection<? extends Expression> expressions) {

			return set(expressions.toArray(new Expression[] {}));
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Node node, String... labels) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.set(node, labels);
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Node node, Collection<String> labels) {

			return set(node, labels.toArray(new String[] {}));
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate mutate(Expression target, Expression properties) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.mutate(target, properties);
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Node node, String... labels) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.remove(node, labels);
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Node node, Collection<String> labels) {

			return remove(node, labels.toArray(new String[] {}));
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Property... properties) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.remove(properties);
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Collection<Property> properties) {

			return remove(properties.toArray(new Property[] {}));
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithoutWhere with(Collection<IdentifiableElement> elements) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.with(elements);
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Collection<IdentifiableElement> elements) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.withDistinct(elements);
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithWhere where(@NotNull Condition newCondition) {

			conditionBuilder.where(newCondition);
			return this;
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithWhere and(Condition additionalCondition) {

			conditionBuilder.and(additionalCondition);
			return this;
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithWhere or(Condition additionalCondition) {

			conditionBuilder.or(additionalCondition);
			return this;
		}

		@NotNull
		@Override
		public OngoingReadingWithoutWhere match(boolean optional, PatternElement... pattern) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.match(optional, pattern);
		}

		@NotNull
		@Override
		public OngoingUpdate create(PatternElement... pattern) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.create(pattern);
		}

		@NotNull
		@Override
		public OngoingUpdate create(Collection<? extends PatternElement> pattern) {

			return create(pattern.toArray(new PatternElement[]{}));
		}

		@NotNull
		@Override
		public OngoingMerge merge(PatternElement... pattern) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.merge(pattern);
		}

		@Override
		public OngoingUnwind unwind(Expression expression) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.unwind(expression);
		}

		@NotNull
		@Override
		public OngoingReadingWithoutWhere call(Statement statement, IdentifiableElement... imports) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.call(statement, imports);
		}

		@NotNull
		@Override
		public BuildableSubquery callInTransactions(Statement statement, Integer rows, IdentifiableElement... imports) {
			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.callInTransactions(statement, rows, imports);
		}

		@NotNull
		@Override
		public InQueryCallBuilder call(String... namespaceAndProcedure) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.call(namespaceAndProcedure);
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithWhere orderBy(SortItem... sortItem) {
			orderBuilder.orderBy(sortItem);
			return this;
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithWhere orderBy(Collection<SortItem> sortItem) {
			return orderBy(sortItem.toArray(new SortItem[] {}));
		}

		@NotNull
		@Override
		public OngoingOrderDefinition orderBy(@NotNull Expression expression) {
			orderBuilder.orderBy(expression);
			return this;
		}

		@NotNull
		@Override
		public OngoingOrderDefinition and(@NotNull Expression expression) {
			orderBuilder.and(expression);
			return this;
		}

		@NotNull
		@Override
		public OngoingReadingAndWithWithWhereAndOrder descending() {
			orderBuilder.descending();
			return this;
		}

		@NotNull
		@Override
		public OngoingReadingAndWithWithWhereAndOrder ascending() {
			orderBuilder.ascending();
			return this;
		}

		@NotNull
		@Override
		public OngoingReadingAndWithWithSkip skip(Number number) {
			return skip(number == null ? null : new NumberLiteral(number));
		}

		@NotNull
		@Override
		public OngoingReadingAndWithWithSkip skip(Expression expression) {
			orderBuilder.skip(expression);
			return this;
		}

		@NotNull
		@Override
		public OngoingReadingAndWith limit(Number number) {
			return limit(number == null ? null : new NumberLiteral(number));
		}

		@NotNull
		@Override
		public OngoingReadingAndWith limit(Expression expression) {
			orderBuilder.limit(expression);
			return this;
		}

		@Override
		@NotNull
		public LoadCSVStatementBuilder.OngoingLoadCSV loadCSV(URI from, boolean withHeaders) {

			DefaultStatementBuilder this0 = DefaultStatementBuilder.this.addWith(buildWith());
			return new DefaultLoadCSVStatementBuilder.PrepareLoadCSVStatementImpl(from, withHeaders, this0);
		}
	}

	/**
	 * A private enum for distinguishing updating clauses.
	 */
	enum UpdateType {
		DELETE, DETACH_DELETE, SET, MUTATE, REMOVE,
		CREATE, MERGE
	}

	private static final EnumSet<UpdateType> MERGE_OR_CREATE = EnumSet.of(UpdateType.CREATE, UpdateType.MERGE);
	private static final EnumSet<UpdateType> SET = EnumSet.of(UpdateType.SET, UpdateType.MUTATE);

	private interface UpdatingClauseBuilder {

		UpdatingClause build();
	}

	interface SupportsActionsOnTheUpdatingClause {

		SupportsActionsOnTheUpdatingClause on(MergeAction.Type type, UpdateType updateType, Expression... expressions);
	}

	/**
	 * Creates a builder for an UPDATE clause. The vargs is list of pattern or expressions.
	 * In case {@code updateType} is of {@link UpdateType#MERGE} or {@link UpdateType#CREATE} they will
	 * be treated as pattern, otherwise as expression.
	 *
	 * @param updateType           The update type to create
	 * @param patternOrExpressions A list of pattern or expression
	 * @param <T>                  The type of {@code patternOrExpressions}
	 * @return Ongoing builder
	 */
	@SafeVarargs
	@SuppressWarnings("varargs") // WTH IDEA?
	private static <T extends Visitable> UpdatingClauseBuilder getUpdatingClauseBuilder(
		UpdateType updateType, T... patternOrExpressions
	) {

		boolean mergeOrCreate = MERGE_OR_CREATE.contains(updateType);
		String message = mergeOrCreate ?
			"At least one pattern is required." :
			"At least one modifying expressions is required.";
		Assertions.notNull(patternOrExpressions, message);
		Assertions.notEmpty(patternOrExpressions, message);

		if (mergeOrCreate) {
			final List<PatternElement> patternElements = Arrays.stream(patternOrExpressions)
				.map(PatternElement.class::cast).toList();
			if (updateType == UpdateType.CREATE) {
				return new AbstractUpdatingClauseBuilder.CreateBuilder(patternElements);
			} else {
				return new AbstractUpdatingClauseBuilder.MergeBuilder(patternElements);
			}
		} else {
			List<Expression> expressions = Arrays.stream(patternOrExpressions).map(Expression.class::cast)
				.toList();
			ExpressionList expressionList = new ExpressionList(
				SET.contains(updateType) ? prepareSetExpressions(updateType, expressions) : expressions);
			return switch (updateType) {
				case DETACH_DELETE -> () -> new Delete(expressionList, true);
				case DELETE -> () -> new Delete(expressionList, false);
				case SET, MUTATE -> () -> new Set(expressionList);
				case REMOVE -> () -> new Remove(expressionList);
				default -> throw new IllegalArgumentException("Unsupported update type " + updateType);
			};
		}
	}

	/**
	 * Utility method to prepare a list of expression to work with the set clause.
	 *
	 * @param possibleSetOperations A mixed list of expressions (property and list operations)
	 * @return A reified list of expressions that all target properties
	 */
	private static List<Expression> prepareSetExpressions(UpdateType updateType,
		List<Expression> possibleSetOperations) {

		List<Expression> propertyOperations = new ArrayList<>();
		List<Expression> listOfExpressions = new ArrayList<>();

		for (Expression possibleSetOperation : possibleSetOperations) {
			if (possibleSetOperation instanceof Operation) {
				propertyOperations.add(possibleSetOperation);
			} else {
				listOfExpressions.add(possibleSetOperation);
			}
		}

		if (listOfExpressions.size() % 2 != 0) {
			throw new IllegalArgumentException("The list of expression to set must be even.");
		}

		if (updateType == UpdateType.SET) {

			for (int i = 0; i < listOfExpressions.size(); i += 2) {
				propertyOperations.add(Operations.set(listOfExpressions.get(i), listOfExpressions.get(i + 1)));
			}
		} else if (updateType == UpdateType.MUTATE) {

			if (!(listOfExpressions.isEmpty() || propertyOperations.isEmpty())) {
				throw new IllegalArgumentException(
					"A mutating SET must be build through a single operation or through a pair of expression, not both.");
			}

			if (listOfExpressions.isEmpty()) {
				for (Expression operation : propertyOperations) {
					if (((Operation) operation).getOperator() != Operator.MUTATE) {
						throw new IllegalArgumentException(
							"Only property operations based on the " + Operator.MUTATE  + " are supported inside a mutating SET.");
					}
				}
			} else {
				for (int i = 0; i < listOfExpressions.size(); i += 2) {
					Expression rhs = listOfExpressions.get(i + 1);
					if (rhs instanceof Parameter) {
						propertyOperations.add(Operations.mutate(listOfExpressions.get(i), rhs));
					} else if (rhs instanceof MapExpression mapExpression) {
						propertyOperations.add(Operations.mutate(listOfExpressions.get(i), mapExpression));
					} else {
						throw new IllegalArgumentException(
							"A mutating SET operation can only be used with a named parameter or a map expression.");
					}
				}
			}
		}

		if (propertyOperations.stream().anyMatch(e -> e instanceof Operation op && op.getOperator() == Operator.REMOVE_LABEL)) {
			throw new IllegalArgumentException("REMOVE operations are not supported in a SET clause");
		}
		return propertyOperations;
	}

	@NotNull
	private static Collection<Expression> extractIdentifiablesFromReturnList(List<Expression> returnList) {
		return returnList.stream()
			.filter(IdentifiableElement.class::isInstance)
			.map(IdentifiableElement.class::cast)
			.map(IdentifiableElement::asExpression)
			.collect(Collectors.toSet());
	}

	/**
	 * Infrastructure for building {@link UpdatingClause updating clauses}
	 *
	 * @param <T> The type of the updating clause
	 */
	private abstract static class AbstractUpdatingClauseBuilder<T extends UpdatingClause>
		implements UpdatingClauseBuilder {

		protected final List<PatternElement> patternElements;

		AbstractUpdatingClauseBuilder(List<PatternElement> patternElements) {
			this.patternElements = patternElements;
		}

		abstract Function<Pattern, T> getUpdatingClauseProvider();

		@Override
		public T build() {
			return getUpdatingClauseProvider().apply(Pattern.of(patternElements));
		}

		static class CreateBuilder extends AbstractUpdatingClauseBuilder<Create> {

			CreateBuilder(List<PatternElement> patternElements) {
				super(patternElements);
			}

			@Override Function<Pattern, Create> getUpdatingClauseProvider() {
				return Create::new;
			}
		}

		static class MergeBuilder extends AbstractUpdatingClauseBuilder<Merge> implements
			SupportsActionsOnTheUpdatingClause {

			private final List<MergeAction> mergeActions = new ArrayList<>();

			MergeBuilder(List<PatternElement> patternElements) {
				super(patternElements);
			}

			@Override
			Function<Pattern, Merge> getUpdatingClauseProvider() {
				return pattern -> new Merge(pattern, mergeActions);
			}

			@Override
			public SupportsActionsOnTheUpdatingClause on(MergeAction.Type type, UpdateType updateType,
				Expression... expressions) {

				ExpressionList expressionList = new ExpressionList(
					prepareSetExpressions(updateType, Arrays.asList(expressions)));
				this.mergeActions.add(MergeAction.of(type, new Set(expressionList)));
				return this;
			}
		}
	}

	protected final class DefaultStatementWithUpdateBuilder implements BuildableMatchAndUpdate {

		final UpdatingClauseBuilder builder;

		protected DefaultStatementWithUpdateBuilder(UpdateType updateType, PatternElement... pattern) {

			this.builder = getUpdatingClauseBuilder(updateType, pattern);
		}

		protected DefaultStatementWithUpdateBuilder(UpdateType updateType, Expression... expressions) {

			this.builder = getUpdatingClauseBuilder(updateType, expressions);
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returning(Collection<? extends Expression> expressions) {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());

			DefaultStatementWithReturnBuilder delegate = new DefaultStatementWithReturnBuilder(false, false);
			delegate.addExpressions(expressions);
			return delegate;
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returningDistinct(Collection<? extends Expression> elements) {

			DefaultStatementWithReturnBuilder delegate = (DefaultStatementWithReturnBuilder) returning(elements);
			delegate.distinct = true;
			return delegate;
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returningRaw(Expression rawExpression) {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return new DefaultStatementWithReturnBuilder(rawExpression);
		}

		@NotNull
		@Override
		public OngoingUpdate delete(Expression... deletedExpressions) {
			return delete(false, deletedExpressions);
		}

		@NotNull
		@Override
		public OngoingUpdate delete(Collection<? extends Expression> deletedExpressions) {
			return delete(deletedExpressions.toArray(new Expression[] {}));
		}

		@NotNull
		@Override
		public OngoingUpdate detachDelete(Expression... deletedExpressions) {
			return delete(true, deletedExpressions);
		}

		@NotNull
		@Override
		public OngoingUpdate detachDelete(Collection<? extends Expression> deletedExpressions) {
			return detachDelete(deletedExpressions.toArray(new Expression[] {}));
		}

		@NotNull
		@Override
		public OngoingMerge merge(PatternElement... pattern) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.merge(pattern);
		}

		private OngoingUpdate delete(boolean nextDetach, Expression... deletedExpressions) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this
				.update(nextDetach ? UpdateType.DETACH_DELETE : UpdateType.DELETE, deletedExpressions);
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Expression... keyValuePairs) {

			DefaultStatementWithUpdateBuilder result = DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(UpdateType.SET, keyValuePairs);
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return result;
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Collection<? extends Expression> keyValuePairs) {

			return set(keyValuePairs.toArray(new Expression[] {}));
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Node node, String... labels) {

			DefaultStatementWithUpdateBuilder result = DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(
				UpdateType.SET, Operations.set(node, labels));
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return result;
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate set(Node node, Collection<String> labels) {

			return set(node, labels.toArray(new String[] {}));
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate mutate(Expression target, Expression properties) {

			DefaultStatementWithUpdateBuilder result = DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(
				UpdateType.MUTATE, Operations.mutate(target, properties));
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return result;
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Node node, String... labels) {

			DefaultStatementWithUpdateBuilder result = DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE,
				Operations.set(node, labels));
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return result;
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Node node, Collection<String> labels) {

			return remove(node, labels.toArray(new String[] {}));
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Property... properties) {

			DefaultStatementWithUpdateBuilder result = DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE, properties);
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return result;
		}

		@NotNull
		@Override
		public BuildableMatchAndUpdate remove(Collection<Property> properties) {

			return remove(properties.toArray(new Property[] {}));
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithoutWhere with(Collection<IdentifiableElement> returnedExpressions) {
			return this.with(false, returnedExpressions);
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Collection<IdentifiableElement> elements) {
			return this.with(true, elements);
		}

		@NotNull
		@Override
		public OngoingUpdate create(PatternElement... pattern) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.create(pattern);
		}

		@NotNull
		@Override
		public OngoingUpdate create(Collection<? extends PatternElement> pattern) {
			return create(pattern.toArray(new PatternElement[] {}));
		}

		private OrderableOngoingReadingAndWithWithoutWhere with(boolean distinct, Collection<IdentifiableElement> elements) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.with(distinct, elements);
		}

		@NotNull
		@Override
		public Statement build() {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.buildImpl(null);
		}
	}

	// Static builder and support classes

	static final class MatchBuilder {

		private final List<PatternElement> patternList = new ArrayList<>();

		private final List<Hint> hints = new ArrayList<>();

		private final ConditionBuilder conditionBuilder = new ConditionBuilder();

		private final boolean optional;

		MatchBuilder(boolean optional) {
			this.optional = optional;
		}

		Match buildMatch() {
			return (Match) Clauses.match(optional, this.patternList, Where.from(conditionBuilder.buildCondition().orElse(null)), hints);
		}
	}

	final class DefaultOngoingUnwind implements OngoingUnwind {

		private final Expression expressionToUnwind;

		DefaultOngoingUnwind(Expression expressionToUnwind) {
			this.expressionToUnwind = expressionToUnwind;
		}

		@NotNull
		@Override
		public OngoingReading as(@NotNull String variable) {
			DefaultStatementBuilder.this.currentSinglePartElements.add(new Unwind(expressionToUnwind, variable));
			return DefaultStatementBuilder.this;
		}
	}

	@NotNull
	@Override
	public InQueryCallBuilder call(String... namespaceAndProcedure) {

		Assertions.notEmpty(namespaceAndProcedure, "The procedure namespace and name must not be null or empty.");

		closeCurrentOngoingMatch();

		return new InQueryCallBuilder(ProcedureName.from(namespaceAndProcedure));
	}

	abstract static class AbstractCallBuilder {

		protected final ProcedureName procedureName;

		protected Expression[] arguments;

		protected final DefaultStatementBuilder.ConditionBuilder conditionBuilder = new DefaultStatementBuilder.ConditionBuilder();

		AbstractCallBuilder(ProcedureName procedureName) {
			this(procedureName, null);
		}

		AbstractCallBuilder(ProcedureName procedureName, Expression[] arguments) {
			this.procedureName = procedureName;
			this.arguments = arguments;
		}

		Arguments createArgumentList() {
			Arguments argumentsList = null;
			if (arguments != null && arguments.length > 0) {
				argumentsList = new Arguments(arguments);
			}
			return argumentsList;
		}
	}

	static final class StandaloneCallBuilder extends AbstractCallBuilder implements

		OngoingStandaloneCallWithoutArguments,
		OngoingStandaloneCallWithArguments {

		StandaloneCallBuilder(ProcedureName procedureName) {
			super(procedureName);
		}

		@NotNull
		@Override
		public StandaloneCallBuilder withArgs(Expression... arguments) {

			super.arguments = arguments;
			return this;
		}

		@NotNull
		public OngoingStandaloneCallWithReturnFields yield(Asterisk asterisk) {

			return new YieldingStandaloneCallBuilder(procedureName, arguments, asterisk);
		}

		@NotNull
		@Override
		public DefaultStatementBuilder.YieldingStandaloneCallBuilder yield(SymbolicName... resultFields) {

			return new YieldingStandaloneCallBuilder(procedureName, arguments, resultFields);
		}

		@NotNull
		@Override
		public DefaultStatementBuilder.YieldingStandaloneCallBuilder yield(AliasedExpression... aliasedResultFields) {

			return new YieldingStandaloneCallBuilder(procedureName,  arguments, aliasedResultFields);
		}

		@NotNull
		@Override
		public Expression asFunction(boolean distinct) {

			if (super.arguments == null || super.arguments.length == 0) {
				return FunctionInvocation.create(procedureName::getQualifiedName);
			}
			if (distinct) {
				return FunctionInvocation.createDistinct(procedureName::getQualifiedName, super.arguments);
			} else {
				return FunctionInvocation.create(procedureName::getQualifiedName, super.arguments);
			}
		}

		@Override
		public VoidCall withoutResults() {
			return new DefaultStatementBuilder(this.build());
		}

		@NotNull
		@Override
		public ProcedureCall build() {

			return ProcedureCallImpl.create(procedureName, createArgumentList(), null,
				conditionBuilder.buildCondition().map(Where::new).orElse(null));
		}
	}

	static final class YieldingStandaloneCallBuilder extends AbstractCallBuilder
		implements ExposesWhere<StatementBuilder.OngoingReadingWithWhere>, ExposesReturning, OngoingStandaloneCallWithReturnFields {

		private final YieldItems yieldItems;

		YieldingStandaloneCallBuilder(ProcedureName procedureName, Expression[] arguments, SymbolicName... resultFields) {
			super(procedureName, arguments);
			this.yieldItems = YieldItems.yieldAllOf(resultFields);
		}

		YieldingStandaloneCallBuilder(ProcedureName procedureName, Expression[] arguments, Asterisk asterisk) {
			super(procedureName, arguments);
			this.yieldItems = YieldItems.yieldAllOf(asterisk);
		}

		YieldingStandaloneCallBuilder(ProcedureName procedureName, Expression[] arguments, AliasedExpression... aliasedResultFields) {
			super(procedureName, arguments);
			this.yieldItems = YieldItems.yieldAllOf(aliasedResultFields);
		}

		@NotNull
		@Override
		public StatementBuilder.OngoingReadingAndReturn returning(Collection<? extends Expression> expressions) {

			return new DefaultStatementBuilder(this.buildCall()).returning(expressions);
		}

		@NotNull
		@Override
		public StatementBuilder.OngoingReadingAndReturn returningDistinct(Collection<? extends Expression> expressions) {

			return new DefaultStatementBuilder(this.buildCall()).returningDistinct(expressions);
		}

		@NotNull
		@Override
		public StatementBuilder.OngoingReadingAndReturn returningRaw(Expression rawExpression) {
			return new DefaultStatementBuilder(this.buildCall()).returningRaw(rawExpression);
		}

		@NotNull
		@Override
		public StatementBuilder.OngoingReadingWithWhere where(Condition newCondition) {

			conditionBuilder.where(newCondition);
			return new DefaultStatementBuilder(this.buildCall());
		}

		@NotNull
		@Override
		public StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere with(Collection<IdentifiableElement> elements) {
			return new DefaultStatementBuilder(this.buildCall()).with(elements);
		}

		@NotNull
		@Override
		public StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere withDistinct(Collection<IdentifiableElement> elements) {
			return new DefaultStatementBuilder(this.buildCall()).withDistinct(elements);
		}

		@NotNull
		@Override
		public StatementBuilder.OngoingReadingWithoutWhere call(Statement statement, IdentifiableElement... imports) {
			return new DefaultStatementBuilder(this.buildCall()).call(statement, imports);
		}

		@NotNull
		@Override
		public BuildableSubquery callInTransactions(Statement statement, Integer rows, IdentifiableElement... imports) {
			return new DefaultStatementBuilder(this.buildCall()).callInTransactions(statement, rows, imports);
		}

		@NotNull
		@Override
		public ResultStatement build() {

			return (ResultStatement) ProcedureCallImpl.create(procedureName, createArgumentList(), yieldItems,
				conditionBuilder.buildCondition().map(Where::new).orElse(null));
		}

		Statement buildCall() {
			return build();
		}

		@NotNull
		@Override
		public  StatementBuilder.OngoingReadingWithoutWhere match(boolean optional, PatternElement... pattern) {
			return new DefaultStatementBuilder(this.buildCall()).match(optional, pattern);
		}
	}

	final class InQueryCallBuilder extends AbstractCallBuilder implements

		OngoingInQueryCallWithoutArguments,
		OngoingInQueryCallWithArguments,
		OngoingInQueryCallWithReturnFields {

		private YieldItems yieldItems;

		InQueryCallBuilder(ProcedureName procedureName) {
			super(procedureName);
		}

		Statement buildCall() {

			return ProcedureCallImpl.create(procedureName, createArgumentList(), yieldItems,
				conditionBuilder.buildCondition().map(Where::new).orElse(null));
		}

		@NotNull
		@Override
		public InQueryCallBuilder withArgs(Expression... arguments) {

			super.arguments = arguments;
			return this;
		}

		@NotNull
		@Override
		public InQueryCallBuilder yield(SymbolicName... resultFields) {

			this.yieldItems = YieldItems.yieldAllOf(resultFields);
			return this;
		}

		@NotNull
		@Override
		public InQueryCallBuilder yield(AliasedExpression... aliasedResultFields) {

			this.yieldItems = YieldItems.yieldAllOf(aliasedResultFields);
			return this;
		}

		@NotNull
		@Override
		public OngoingReadingWithWhere where(Condition newCondition) {

			conditionBuilder.where(newCondition);
			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this;
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returning(Collection<? extends Expression> expressions) {

			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.returning(expressions);
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returningDistinct(Collection<? extends Expression> expressions) {

			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.returningDistinct(expressions);
		}

		@NotNull
		@Override
		public OngoingReadingAndReturn returningRaw(Expression rawExpression) {
			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.returningRaw(rawExpression);
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithoutWhere with(Collection<IdentifiableElement> elements) {

			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.with(elements);
		}

		@NotNull
		@Override
		public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Collection<IdentifiableElement> elements) {

			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.withDistinct(elements);
		}

		@NotNull
		@Override
		public StatementBuilder.OngoingReadingWithoutWhere call(Statement statement, IdentifiableElement... imports) {

			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.call(statement, imports);
		}

		@NotNull
		@Override
		public BuildableSubquery callInTransactions(Statement statement, Integer rows, IdentifiableElement... imports) {

			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.callInTransactions(statement, rows, imports);
		}

		@NotNull
		@Override
		public
		StatementBuilder.OngoingReadingWithoutWhere match(boolean optional, PatternElement... pattern) {

			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this.match(optional, pattern);
		}

		@Override
		public VoidCall withoutResults() {
			DefaultStatementBuilder.this.currentSinglePartElements.add(this.buildCall());
			return DefaultStatementBuilder.this;
		}
	}

	static final class ConditionBuilder {
		protected Condition condition;

		void where(Condition newCondition) {

			Assertions.notNull(newCondition, "The new condition must not be null.");
			this.condition = newCondition;
		}

		void and(Condition additionalCondition) {

			this.condition = this.condition.and(additionalCondition);
		}

		void or(Condition additionalCondition) {

			this.condition = this.condition.or(additionalCondition);
		}

		private boolean hasCondition() {
			return this.condition != null && (!(this.condition instanceof CompoundCondition compoundCondition)
					|| compoundCondition.hasConditions());
		}

		Optional<Condition> buildCondition() {
			return hasCondition() ? Optional.of(this.condition) : Optional.empty();
		}
	}

	static final class OrderBuilder {
		final List<SortItem> sortItemList = new ArrayList<>();
		SortItem lastSortItem;
		Skip skip;
		Limit limit;

		void reset() {
			this.sortItemList.clear();
			this.lastSortItem = null;
			this.skip = null;
			this.limit = null;
		}

		void orderBy(SortItem... sortItem) {
			this.sortItemList.addAll(Arrays.asList(sortItem));
		}

		void orderBy(Collection<SortItem> sortItems) {
			if (sortItems != null) {
				this.sortItemList.addAll(sortItems);
			}
		}

		void orderBy(Expression expression) {
			this.lastSortItem = Cypher.sort(expression);
		}

		void and(Expression expression) {
			orderBy(expression);
		}

		void descending() {
			this.sortItemList.add(this.lastSortItem.descending());
			this.lastSortItem = null;
		}

		void ascending() {
			this.sortItemList.add(this.lastSortItem.ascending());
			this.lastSortItem = null;
		}

		void skip(Expression expression) {
			if (expression != null) {
				skip = Skip.create(expression);
			}
		}

		void limit(Expression expression) {
			if (expression != null) {
				limit = Limit.create(expression);
			}
		}

		Optional<Order> buildOrder() {
			if (lastSortItem != null) {
				sortItemList.add(lastSortItem);
			}
			Optional<Order> result = sortItemList.isEmpty() ? Optional.empty() : Optional.of(new Order(sortItemList));
			sortItemList.clear();
			lastSortItem = null;
			return result;
		}

		Skip getSkip() {
			return skip;
		}

		Limit getLimit() {
			return limit;
		}
	}
}
