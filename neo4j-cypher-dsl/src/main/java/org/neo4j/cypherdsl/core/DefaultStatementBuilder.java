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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.neo4j.cypherdsl.core.ProcedureCall.OngoingInQueryCallWithArguments;
import org.neo4j.cypherdsl.core.ProcedureCall.OngoingInQueryCallWithReturnFields;
import org.neo4j.cypherdsl.core.ProcedureCall.OngoingInQueryCallWithoutArguments;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingMatchAndUpdate;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingReadingWithWhere;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingReadingWithoutWhere;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingUpdate;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @author Romain Rossi
 * @since 1.0
 */
class DefaultStatementBuilder implements StatementBuilder,
	OngoingUpdate, OngoingReadingWithWhere, OngoingReadingWithoutWhere, OngoingMatchAndUpdate {

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
	 * Current ongoing call.
	 */
	private ProcedureCall.Builder currentOngoingCall;

	DefaultStatementBuilder() {
	}

	DefaultStatementBuilder(ProcedureCall.Builder currentOngoingCall) {
		this.currentOngoingCall = currentOngoingCall;
	}

	@Override
	public OngoingReadingWithoutWhere optionalMatch(PatternElement... pattern) {

		return this.match(true, pattern);
	}

	@Override
	public OngoingReadingWithoutWhere match(PatternElement... pattern) {

		return this.match(false, pattern);
	}

	private OngoingReadingWithoutWhere match(boolean optional, PatternElement... pattern) {

		Assertions.notNull(pattern, "Patterns to match are required.");
		Assertions.notEmpty(pattern, "At least one pattern to match is required.");

		this.closeCurrentOngoingMatch();
		this.closeCurrentOngoingCall();

		this.currentOngoingMatch = new MatchBuilder(optional);
		this.currentOngoingMatch.patternList.addAll(Arrays.asList(pattern));
		return this;
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns `this`, implementing `OngoingUpdate`
	public OngoingUpdate create(PatternElement... pattern) {

		return update(UpdateType.CREATE, pattern);
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns `this`, implementing `OngoingUpdate`
	public OngoingUpdate merge(PatternElement... pattern) {

		return update(UpdateType.MERGE, pattern);
	}

	@Override
	public OngoingMergeAction onCreate() {
		return ongoingOnAfterMerge(MergeAction.Type.ON_CREATE);
	}

	@Override
	public OngoingMergeAction onMatch() {
		return ongoingOnAfterMerge(MergeAction.Type.ON_MATCH);
	}

	private OngoingMergeAction ongoingOnAfterMerge(MergeAction.Type type) {

		Assertions.notNull(this.currentOngoingUpdate, "MERGE must have been invoked before defining an event.");
		Assertions.isTrue(this.currentOngoingUpdate.builder instanceof SupportsActionsOnTheUpdatingClause, "MERGE must have been invoked before defining an event.");

		return new OngoingMergeAction() {

			@Override
			public OngoingMatchAndUpdate set(Expression... expressions) {
				((SupportsActionsOnTheUpdatingClause) DefaultStatementBuilder.this.currentOngoingUpdate.builder).on(type, expressions);
				return DefaultStatementBuilder.this;
			}
		};
	}


	@Override
	public OngoingUnwind unwind(Expression expression) {

		closeCurrentOngoingMatch();

		return new DefaultOngoingUnwind(expression);
	}

	private DefaultStatementBuilder update(UpdateType updateType, Object[] pattern) {

		Assertions.notNull(pattern, "Patterns to create are required.");
		Assertions.notEmpty(pattern, "At least one pattern to create is required.");

		this.closeCurrentOngoingMatch();
		this.closeCurrentOngoingCall();
		this.closeCurrentOngoingUpdate();

		if (pattern.getClass().getComponentType() == PatternElement.class) {
			this.currentOngoingUpdate = new DefaultStatementWithUpdateBuilder(updateType, (PatternElement[]) pattern);
		} else if (pattern.getClass().getComponentType() == Expression.class) {
			this.currentOngoingUpdate = new DefaultStatementWithUpdateBuilder(updateType, (Expression[]) pattern);
		}

		return this;
	}

	@Override
	public OngoingReadingAndReturn returning(Expression... expressions) {

		return returning(false, expressions);
	}

	@Override
	public OngoingReadingAndReturn returningDistinct(Expression... expressions) {
		return returning(true, expressions);
	}

	private OngoingReadingAndReturn returning(boolean distinct, Expression... expressions) {

		DefaultStatementWithReturnBuilder ongoingMatchAndReturn = new DefaultStatementWithReturnBuilder(distinct);
		ongoingMatchAndReturn.addExpressions(expressions);
		return ongoingMatchAndReturn;
	}

	@Override
	public OrderableOngoingReadingAndWithWithoutWhere with(String... variables) {
		return with(false, Expressions.createSymbolicNames(variables));
	}

	@Override
	public OrderableOngoingReadingAndWithWithoutWhere with(Named... variables) {
		return with(false, Expressions.createSymbolicNames(variables));
	}

	@Override
	public OrderableOngoingReadingAndWithWithoutWhere with(Expression... expressions) {

		return with(false, expressions);
	}

	@Override
	public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Expression... expressions) {

		return with(true, expressions);
	}

	private OrderableOngoingReadingAndWithWithoutWhere with(boolean distinct, Expression... expressions) {

		DefaultStatementWithWithBuilder ongoingMatchAndWith = new DefaultStatementWithWithBuilder(distinct);
		ongoingMatchAndWith.addExpressions(expressions);
		return ongoingMatchAndWith;
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns `this`, implementing `OngoingUpdate`
	public OngoingUpdate delete(Expression... expressions) {

		return update(UpdateType.DELETE, expressions);
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns `this`, implementing `OngoingUpdate`
	public OngoingUpdate detachDelete(Expression... expressions) {

		return update(UpdateType.DETACH_DELETE, expressions);
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns a `DefaultStatementWithUpdateBuilder`, implementing the necessary interfaces
	public OngoingMatchAndUpdate set(Expression... expressions) {

		this.closeCurrentOngoingUpdate();

		return new DefaultStatementWithUpdateBuilder(UpdateType.SET, expressions);
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns a `DefaultStatementWithUpdateBuilder`, implementing the necessary interfaces
	public OngoingMatchAndUpdate set(Node named, String... labels) {

		return new DefaultStatementWithUpdateBuilder(UpdateType.SET, Operations.set(named, labels));
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns a `DefaultStatementWithUpdateBuilder`, implementing the necessary interfaces
	public OngoingMatchAndUpdate remove(Property... properties) {

		return new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE, properties);
	}

	@Override
	@SuppressWarnings("unchecked") // This method returns a `DefaultStatementWithUpdateBuilder`, implementing the necessary interfaces
	public OngoingMatchAndUpdate remove(Node named, String... labels) {

		return new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE, Operations.remove(named, labels));
	}

	@Override
	public OngoingReadingWithWhere where(Condition newCondition) {

		this.currentOngoingMatch.conditionBuilder.where(newCondition);
		return this;
	}

	@Override
	public OngoingReadingWithWhere and(Condition additionalCondition) {

		this.currentOngoingMatch.conditionBuilder.and(additionalCondition);
		return this;
	}

	@Override
	public OngoingReadingWithWhere or(Condition additionalCondition) {

		this.currentOngoingMatch.conditionBuilder.or(additionalCondition);
		return this;
	}

	@Override
	public Statement build() {

		return buildImpl(false, null);
	}

	protected Statement buildImpl(boolean clearCurrentBuildSteps, Return returning) {

		SinglePartQuery singlePartQuery = SinglePartQuery.create(
			buildListOfVisitables(clearCurrentBuildSteps), returning);

		if (multiPartElements.isEmpty()) {
			return singlePartQuery;
		} else {
			return new MultiPartQuery(multiPartElements, singlePartQuery);
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

		if (this.currentOngoingCall != null) {
			visitables.add(this.currentOngoingCall.build());
		}

		if (clearAfter) {
			this.currentOngoingMatch = null;
			this.currentOngoingUpdate = null;
			this.currentOngoingCall = null;
			this.currentSinglePartElements.clear();
		}
		return visitables;
	}

	protected final DefaultStatementBuilder addWith(Optional<With> optionalWith) {

		optionalWith.ifPresent(with -> multiPartElements.add(new MultiPartElement(buildListOfVisitables(true), with)));
		return this;
	}

	protected final DefaultStatementBuilder addUpdatingClause(UpdatingClause updatingClause) {

		// Close current match
		closeCurrentOngoingMatch();

		this.currentSinglePartElements.add(updatingClause);
		return this;
	}

	@Override
	public OngoingReadingWithoutWhere call(Statement statement) {

		this.closeCurrentOngoingMatch();
		this.closeCurrentOngoingCall();
		this.closeCurrentOngoingUpdate();

		this.currentSinglePartElements.add(Subquery.call(statement));

		return this;
	}

	private void closeCurrentOngoingMatch() {
		if (this.currentOngoingMatch == null) {
			return;
		}

		this.currentSinglePartElements.add(this.currentOngoingMatch.buildMatch());
		this.currentOngoingMatch = null;
	}

	private void closeCurrentOngoingCall() {
		if (this.currentOngoingCall == null) {
			return;
		}

		this.currentSinglePartElements.add(this.currentOngoingCall.build());
		this.currentOngoingCall = null;
	}

	private void closeCurrentOngoingUpdate() {
		if (this.currentOngoingUpdate == null) {
			return;
		}

		this.currentSinglePartElements.add(this.currentOngoingUpdate.builder.build());
		this.currentOngoingUpdate = null;
	}

	@Override
	public Condition asCondition() {

		if (this.currentOngoingMatch == null || !this.currentSinglePartElements.isEmpty()) {
			throw new IllegalArgumentException("Only simple MATCH statements can be used as existential subqueries.");
		}

		return ExistentialSubquery.exists(this.currentOngoingMatch.buildMatch());
	}

	protected class DefaultStatementWithReturnBuilder
		implements OngoingReadingAndReturn, TerminalOngoingOrderDefinition, OngoingMatchAndReturnWithOrder {

		protected final List<Expression> returnList = new ArrayList<>();
		protected final OrderBuilder orderBuilder = new OrderBuilder();
		protected boolean distinct;

		protected DefaultStatementWithReturnBuilder(boolean distinct) {
			this.distinct = distinct;
		}

		@Override
		public final OngoingMatchAndReturnWithOrder orderBy(SortItem... sortItem) {
			orderBuilder.orderBy(sortItem);
			return this;
		}

		@Override
		public final TerminalOngoingOrderDefinition orderBy(Expression expression) {
			orderBuilder.orderBy(expression);
			return this;
		}

		@Override
		public final TerminalOngoingOrderDefinition and(Expression expression) {
			orderBuilder.and(expression);
			return this;
		}

		@Override
		@SuppressWarnings("unchecked")
		public final DefaultStatementWithReturnBuilder descending() {
			orderBuilder.descending();
			return this;
		}

		@Override
		@SuppressWarnings("unchecked")
		public final DefaultStatementWithReturnBuilder ascending() {
			orderBuilder.ascending();
			return this;
		}

		@Override
		@SuppressWarnings("unchecked")
		public final OngoingReadingAndReturn skip(Number number) {
			orderBuilder.skip(number);
			return this;
		}

		@Override
		public final OngoingReadingAndReturn limit(Number number) {
			orderBuilder.limit(number);
			return this;
		}

		@Override
		public Statement build() {

			Return returning = null;
			if (!returnList.isEmpty()) {

				ExpressionList returnItems = new ExpressionList(this.returnList);
				returning = new Return(distinct, returnItems, orderBuilder.buildOrder().orElse(null),
					orderBuilder.getSkip(),
					orderBuilder.getLimit());
			}

			return DefaultStatementBuilder.this.buildImpl(false, returning);
		}

		protected final void addExpressions(Expression... expressions) {

			Assertions.notNull(expressions, "Expressions to return are required.");
			Assertions.notEmpty(expressions, "At least one expressions to return is required.");

			this.returnList.addAll(Arrays.asList(expressions));
		}
	}

	/**
	 * Adds support for With to a return builder.
	 */
	protected abstract class WithBuilderSupport {

	}

	/**
	 * Ongoing with extends from {@link WithBuilderSupport} and therefore from {@link DefaultStatementWithReturnBuilder}.
	 */
	protected final class DefaultStatementWithWithBuilder extends WithBuilderSupport
		implements OngoingOrderDefinition, OrderableOngoingReadingAndWithWithoutWhere,
		OrderableOngoingReadingAndWithWithWhere, OngoingReadingAndWithWithWhereAndOrder {

		protected final ConditionBuilder conditionBuilder = new ConditionBuilder();
		protected final List<Expression> returnList = new ArrayList<>();
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

		protected void addExpressions(Expression... expressions) {

			Assertions.notNull(expressions, "Expressions to return are required.");
			Assertions.notEmpty(expressions, "At least one expressions to return is required.");

			this.returnList.addAll(Arrays.asList(expressions));
		}

		@Override
		public OngoingReadingAndReturn returning(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.returning(expressions);
		}

		@Override
		public OngoingReadingAndReturn returningDistinct(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.returningDistinct(expressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate delete(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.delete(expressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate detachDelete(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.detachDelete(expressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate set(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.set(expressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate set(Node node, String... labels) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.set(node, labels);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate remove(Node node, String... labels) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.remove(node, labels);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate remove(Property... properties) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.remove(properties);
		}

		@Override
		public OrderableOngoingReadingAndWithWithoutWhere with(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.with(expressions);
		}

		@Override
		public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Expression... expressions) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.withDistinct(expressions);
		}

		@Override
		public OrderableOngoingReadingAndWithWithWhere where(Condition newCondition) {

			conditionBuilder.where(newCondition);
			return this;
		}

		@Override
		public OrderableOngoingReadingAndWithWithWhere and(Condition additionalCondition) {

			conditionBuilder.and(additionalCondition);
			return this;
		}

		@Override
		public OrderableOngoingReadingAndWithWithWhere or(Condition additionalCondition) {

			conditionBuilder.or(additionalCondition);
			return this;
		}

		@Override
		public OngoingReadingWithoutWhere match(PatternElement... pattern) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.match(pattern);
		}

		@Override
		public OngoingReadingWithoutWhere optionalMatch(PatternElement... pattern) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.optionalMatch(pattern);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate create(PatternElement... pattern) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.create(pattern);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate merge(PatternElement... pattern) {

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

		@Override
		public OngoingReadingWithoutWhere call(Statement statement) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.call(statement);
		}

		@Override
		public InQueryCallBuilder call(String... namespaceAndProcedure) {

			return DefaultStatementBuilder.this
				.addWith(buildWith())
				.call(namespaceAndProcedure);
		}

		@Override
		public OrderableOngoingReadingAndWithWithWhere orderBy(SortItem... sortItem) {
			orderBuilder.orderBy(sortItem);
			return this;
		}

		@Override
		public OngoingOrderDefinition orderBy(Expression expression) {
			orderBuilder.orderBy(expression);
			return this;
		}

		@Override
		public OngoingOrderDefinition and(Expression expression) {
			orderBuilder.and(expression);
			return this;
		}

		@Override
		@SuppressWarnings("unchecked")
		public OrderableOngoingReadingAndWithWithWhere descending() {
			orderBuilder.descending();
			return this;
		}

		@Override
		@SuppressWarnings("unchecked")
		public OrderableOngoingReadingAndWithWithWhere ascending() {
			orderBuilder.ascending();
			return this;
		}

		@Override
		@SuppressWarnings("unchecked")
		public OrderableOngoingReadingAndWithWithWhere skip(Number number) {
			orderBuilder.skip(number);
			return this;
		}

		@Override
		public OngoingReadingAndWith limit(Number number) {
			orderBuilder.limit(number);
			return this;
		}
	}

	/**
	 * A private enum for distinguishing updating clauses.
	 */
	enum UpdateType {
		DELETE, DETACH_DELETE, SET, REMOVE,
		CREATE, MERGE;
	}

	private static final EnumSet<UpdateType> MERGE_OR_CREATE = EnumSet.of(UpdateType.CREATE, UpdateType.MERGE);

	private interface UpdatingClauseBuilder {

		UpdatingClause build();
	}

	interface SupportsActionsOnTheUpdatingClause {

		SupportsActionsOnTheUpdatingClause on(MergeAction.Type type, Expression... expressions);
	}

	private static <T extends Visitable> UpdatingClauseBuilder getUpdatingClauseBuilder(UpdateType updateType, T... patternOrExpressions) {

		boolean mergeOrCreate = MERGE_OR_CREATE.contains(updateType);
		String message = mergeOrCreate ? "At least one pattern is required." : "At least one modifying expressions is required.";
		Assertions.notNull(patternOrExpressions, message);
		Assertions.notEmpty(patternOrExpressions, message);

		if (mergeOrCreate) {
			final List<PatternElement> patternElements = Arrays.stream(patternOrExpressions).map(PatternElement.class::cast).collect(Collectors.toList());
			if (updateType == UpdateType.CREATE) {
				return new AbstractUpdatingClauseBuilder.CreateBuilder(patternElements);
			} else {
				return new AbstractUpdatingClauseBuilder.MergeBuilder(patternElements);
			}
		} else {
			List<Expression> expressions = Arrays.stream(patternOrExpressions).map(Expression.class::cast).collect(Collectors.toList());
			ExpressionList expressionList = new ExpressionList(updateType == UpdateType.SET ? prepareSetExpressions(expressions) : expressions);
			switch (updateType) {
				case DETACH_DELETE:
					return () -> new Delete(expressionList, true);
				case DELETE:
					return () -> new Delete(expressionList, false);
				case SET:
					return () -> new Set(expressionList);
				case REMOVE:
					return () -> new Remove(expressionList);
				default:
					throw new IllegalArgumentException("Unsupported update type " + updateType);
			}
		}
	}

	/**
	 * Utility method to prepare a list of expression to work with the set clause.
	 * @param possibleSetOperations A mixed list of expressions (property and list operations)
	 * @return A reified list of expressions that all target properties
	 */
	private static List<Expression> prepareSetExpressions(List<Expression> possibleSetOperations) {
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
		for (int i = 0; i < listOfExpressions.size(); i += 2) {
			propertyOperations.add(Operations.set(listOfExpressions.get(i), listOfExpressions.get(i + 1)));
		}

		return propertyOperations;
	}

	/**
	 * Infrastructure for building {@link UpdatingClause updating clauses}
	 * @param <T> The type of the updating clause
	 */
	private abstract static class AbstractUpdatingClauseBuilder<T extends UpdatingClause> implements UpdatingClauseBuilder {

		protected final List<PatternElement> patternElements;

		AbstractUpdatingClauseBuilder(List<PatternElement> patternElements) {
			this.patternElements = patternElements;
		}

		abstract Function<Pattern, T> getUpdatingClauseProvider();

		@Override
		public T build() {
			return getUpdatingClauseProvider().apply(new Pattern(patternElements));
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

			private List<MergeAction> mergeActions = new ArrayList<>();

			MergeBuilder(List<PatternElement> patternElements) {
				super(patternElements);
			}

			@Override
			Function<Pattern, Merge> getUpdatingClauseProvider() {
				return pattern -> new Merge(pattern, mergeActions);
			}

			@Override
			public SupportsActionsOnTheUpdatingClause on(MergeAction.Type type, Expression... expressions) {

				ExpressionList expressionList = new ExpressionList(prepareSetExpressions(Arrays.asList(expressions)));
				this.mergeActions.add(new MergeAction(type, new Set(expressionList)));
				return this;
			}
		}
	}

	protected final class DefaultStatementWithUpdateBuilder extends DefaultStatementWithReturnBuilder implements OngoingMatchAndUpdate {

		final UpdatingClauseBuilder builder;

		protected DefaultStatementWithUpdateBuilder(UpdateType updateType, PatternElement... pattern) {
			super(false);

			this.builder = getUpdatingClauseBuilder(updateType, pattern);
		}

		protected DefaultStatementWithUpdateBuilder(UpdateType updateType, Expression... expressions) {
			super(false);

			this.builder = getUpdatingClauseBuilder(updateType, expressions);
		}

		@Override
		public OngoingReadingAndReturn returning(Expression... returnedExpressions) {

			Assertions.notNull(returnedExpressions, "Expressions to return are required.");
			Assertions.notEmpty(returnedExpressions, "At least one expressions to return is required.");

			super.returnList.addAll(Arrays.asList(returnedExpressions));
			return this;
		}

		@Override
		public OngoingReadingAndReturn returningDistinct(Expression... returnedExpressions) {

			returning(returnedExpressions);
			super.distinct = true;
			return this;
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate delete(Expression... deletedExpressions) {
			return delete(false, deletedExpressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate detachDelete(Expression... deletedExpressions) {
			return delete(true, deletedExpressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate merge(PatternElement... pattern) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.merge(pattern);
		}

		private OngoingUpdate delete(boolean nextDetach, Expression... deletedExpressions) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.update(nextDetach ? UpdateType.DETACH_DELETE : UpdateType.DELETE, deletedExpressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate set(Expression... keyValuePairs) {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(UpdateType.SET, keyValuePairs);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate set(Node node, String... labels) {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(
				UpdateType.SET, Operations.set(node, labels));
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate remove(Node node, String... labels) {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE,
				Operations.set(node, labels));
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingMatchAndUpdate remove(Property... properties) {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.new DefaultStatementWithUpdateBuilder(UpdateType.REMOVE, properties);
		}

		@Override
		public OrderableOngoingReadingAndWithWithoutWhere with(Expression... returnedExpressions) {
			return this.with(false, returnedExpressions);
		}

		@Override
		public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Expression... returnedExpressions) {
			return this.with(true, returnedExpressions);
		}

		@Override
		@SuppressWarnings("unchecked")
		public OngoingUpdate create(PatternElement... pattern) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.create(pattern);
		}

		private OrderableOngoingReadingAndWithWithoutWhere with(boolean distinct, Expression... returnedExpressions) {
			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return DefaultStatementBuilder.this.with(distinct, returnedExpressions);
		}

		@Override
		public Statement build() {

			DefaultStatementBuilder.this.addUpdatingClause(builder.build());
			return super.build();
		}
	}

	// Static builder and support classes

	static final class MatchBuilder {

		private final List<PatternElement> patternList = new ArrayList<>();

		private final ConditionBuilder conditionBuilder = new ConditionBuilder();

		private final boolean optional;

		MatchBuilder(boolean optional) {
			this.optional = optional;
		}

		Match buildMatch() {
			Pattern pattern = new Pattern(this.patternList);
			return new Match(optional, pattern, conditionBuilder.buildCondition().map(Where::new).orElse(null));
		}
	}

	final class DefaultOngoingUnwind implements OngoingUnwind {

		private final Expression expressionToUnwind;

		DefaultOngoingUnwind(Expression expressionToUnwind) {
			this.expressionToUnwind = expressionToUnwind;
		}

		@Override
		public OngoingReading as(String variable) {
			DefaultStatementBuilder.this.currentSinglePartElements.add(new Unwind(expressionToUnwind, variable));
			return DefaultStatementBuilder.this;
		}
	}

	@Override
	public InQueryCallBuilder call(String... namespaceAndProcedure) {

		Assertions.notEmpty(namespaceAndProcedure, "The procedure namespace and name must not be null or empty.");

		closeCurrentOngoingMatch();

		closeCurrentOngoingCall();

		InQueryCallBuilder inQueryCallBuilder = new InQueryCallBuilder(ProcedureName.from(namespaceAndProcedure));
		this.currentOngoingCall = inQueryCallBuilder;
		return inQueryCallBuilder;
	}

	private final class InQueryCallBuilder extends ProcedureCall.Builder implements
		OngoingInQueryCallWithoutArguments, OngoingInQueryCallWithArguments,
		OngoingInQueryCallWithReturnFields {

		InQueryCallBuilder(ProcedureName procedureName) {
			super(procedureName);
		}

		@Override
		public InQueryCallBuilder withArgs(Expression... arguments) {

			super.arguments = arguments;
			return this;
		}

		@Override
		public InQueryCallBuilder yield(SymbolicName... resultFields) {

			super.yieldItems = YieldItems.yieldAllOf(resultFields);
			return this;
		}

		@Override
		public InQueryCallBuilder yield(AliasedExpression... aliasedResultFields) {

			super.yieldItems = YieldItems.yieldAllOf(aliasedResultFields);
			return this;
		}

		@Override
		public OngoingReadingWithWhere where(Condition newCondition) {

			conditionBuilder.where(newCondition);
			DefaultStatementBuilder.this.currentOngoingCall = this;
			return DefaultStatementBuilder.this;
		}

		@Override
		public OngoingReadingAndReturn returning(Expression... expressions) {

			DefaultStatementBuilder.this.currentOngoingCall = this;
			return DefaultStatementBuilder.this.returning(expressions);
		}

		@Override
		public OngoingReadingAndReturn returningDistinct(Expression... expressions) {

			DefaultStatementBuilder.this.currentOngoingCall = this;
			return DefaultStatementBuilder.this.returningDistinct(expressions);
		}

		@Override
		public OrderableOngoingReadingAndWithWithoutWhere with(Expression... expressions) {

			DefaultStatementBuilder.this.currentOngoingCall = this;
			return DefaultStatementBuilder.this.with(expressions);
		}

		@Override
		public OrderableOngoingReadingAndWithWithoutWhere withDistinct(Expression... expressions) {

			DefaultStatementBuilder.this.currentOngoingCall = this;
			return DefaultStatementBuilder.this.withDistinct(expressions);
		}

		@Override
		public OngoingReadingWithoutWhere call(Statement statement) {

			DefaultStatementBuilder.this.currentOngoingCall = this;
			return DefaultStatementBuilder.this.call(statement);
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
			return !(this.condition == null || this.condition == CompoundCondition.EMPTY_CONDITION);
		}

		Optional<Condition> buildCondition() {
			return hasCondition() ? Optional.of(this.condition) : Optional.empty();
		}
	}

	static final class OrderBuilder {
		protected final List<SortItem> sortItemList = new ArrayList<>();
		protected SortItem lastSortItem;
		protected Skip skip;
		protected Limit limit;

		protected void reset() {
			this.sortItemList.clear();
			this.lastSortItem = null;
			this.skip = null;
			this.limit = null;
		}

		protected void orderBy(SortItem... sortItem) {
			Arrays.stream(sortItem).forEach(this.sortItemList::add);
		}

		protected void orderBy(Expression expression) {
			this.lastSortItem = Cypher.sort(expression);
		}

		protected void and(Expression expression) {
			orderBy(expression);
		}

		protected void descending() {
			this.sortItemList.add(this.lastSortItem.descending());
			this.lastSortItem = null;
		}

		protected void ascending() {
			this.sortItemList.add(this.lastSortItem.ascending());
			this.lastSortItem = null;
		}

		protected void skip(Number number) {

			if (number != null) {
				skip = Skip.create(number);
			}
		}

		protected void limit(Number number) {

			if (number != null) {
				limit = Limit.create(number);
			}
		}

		protected Optional<Order> buildOrder() {
			if (lastSortItem != null) {
				sortItemList.add(lastSortItem);
			}
			Optional<Order> result = sortItemList.size() > 0 ? Optional.of(new Order(sortItemList)) : Optional.empty();
			sortItemList.clear();
			lastSortItem = null;
			return result;
		}

		protected Skip getSkip() {
			return skip;
		}

		protected Limit getLimit() {
			return limit;
		}
	}
}
