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

import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/StandaloneCall.html">StandaloneCall</a>.
 *
 * @author Michael J. Simons
 * @soundtrack Apocalyptica - Cell-0
 * @since 2020.0.1
 */
public final class ProcedureCall implements Statement {

	private final ProcedureName name;

	private final Arguments arguments;

	private final YieldItems yieldItems;

	private final Where optionalWhere;

	ProcedureCall(ProcedureName name, Arguments arguments, YieldItems yieldItems, Where optionalWhere) {

		this.name = name;
		this.arguments = arguments == null ? new Arguments() : arguments;
		this.yieldItems = yieldItems;
		this.optionalWhere = optionalWhere;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.name.accept(visitor);
		Visitable.visitIfNotNull(arguments, visitor);
		Visitable.visitIfNotNull(yieldItems, visitor);
		Visitable.visitIfNotNull(optionalWhere, visitor);
		visitor.leave(this);
	}

	/**
	 * The union of a buildable statement and call exposing new arguments and yields.
	 */
	public interface OngoingStandaloneCallWithoutArguments extends
		StatementBuilder.BuildableStatement, ExposesCall.ExposesWithArgs<OngoingStandaloneCallWithArguments>,
		ExposesCall.ExposesYield<OngoingStandaloneCallWithReturnFields>, ExposesCall.AsFunction {
	}

	/**
	 * The union of a buildable statement and call exposing yields.
	 */
	public interface OngoingStandaloneCallWithArguments extends
		StatementBuilder.BuildableStatement, ExposesCall.ExposesYield<OngoingStandaloneCallWithReturnFields>, ExposesCall.AsFunction {
	}

	/**
	 * A buildable statement exposing where and return clauses.
	 */
	public interface OngoingStandaloneCallWithReturnFields extends
		StatementBuilder.BuildableStatement,
		ExposesWhere, ExposesReturning, StatementBuilder.ExposesWith {
	}

	/**
	 * The union of an in-query call exposing new arguments and yields.
	 */
	public interface OngoingInQueryCallWithoutArguments extends
		ExposesCall.ExposesWithArgs<OngoingInQueryCallWithArguments>,
		ExposesCall.ExposesYield<OngoingInQueryCallWithReturnFields> {
	}

	/**
	 * The union of an in-query call exposing yields.
	 */
	public interface OngoingInQueryCallWithArguments extends
		ExposesCall.ExposesYield<OngoingInQueryCallWithReturnFields> {
	}


	/**
	 * An in-query call exposing where and return clauses.
	 */
	public interface OngoingInQueryCallWithReturnFields extends
		ExposesWhere, ExposesReturning, StatementBuilder.ExposesWith {
	}

	protected abstract static class Builder implements ExposesWhere, ExposesReturning,
		StatementBuilder.BuildableStatement {

		protected final ProcedureName procedureName;

		protected Expression[] arguments;

		protected YieldItems yieldItems;

		protected final DefaultStatementBuilder.ConditionBuilder conditionBuilder = new DefaultStatementBuilder.ConditionBuilder();

		Builder(ProcedureName procedureName) {
			this.procedureName = procedureName;
		}

		@Override
		public Statement build() {

			Arguments argumentsList = null;
			if (arguments != null && arguments.length > 0) {
				argumentsList = new Arguments(arguments);
			}

			return new ProcedureCall(procedureName, argumentsList, yieldItems,
				conditionBuilder.buildCondition().map(Where::new).orElse(null));
		}
	}

	protected static final class StandaloneCallBuilder extends Builder implements
		OngoingStandaloneCallWithoutArguments,
		OngoingStandaloneCallWithArguments,
		OngoingStandaloneCallWithReturnFields {

		StandaloneCallBuilder(ProcedureName procedureName) {
			super(procedureName);
		}

		public StandaloneCallBuilder withArgs(Expression... arguments) {

			super.arguments = arguments;
			return this;
		}

		public StandaloneCallBuilder yield(SymbolicName... resultFields) {

			super.yieldItems = YieldItems.yieldAllOf(resultFields);
			return this;
		}

		public StandaloneCallBuilder yield(AliasedExpression... aliasedResultFields) {

			super.yieldItems = YieldItems.yieldAllOf(aliasedResultFields);
			return this;
		}

		@Override
		public StatementBuilder.OngoingReadingWithWhere where(Condition newCondition) {

			conditionBuilder.where(newCondition);
			return new DefaultStatementBuilder(this);
		}

		@Override
		public StatementBuilder.OngoingReadingAndReturn returning(Expression... expressions) {

			return new DefaultStatementBuilder(this).returning(expressions);
		}

		@Override
		public StatementBuilder.OngoingReadingAndReturn returningDistinct(Expression... expressions) {

			return new DefaultStatementBuilder(this).returningDistinct(expressions);
		}

		@Override
		public Expression asFunction() {

			if (super.arguments == null || super.arguments.length == 0) {
				return FunctionInvocation.create(() -> procedureName.getQualifiedName());
			}
			return FunctionInvocation.create(() -> procedureName.getQualifiedName(), super.arguments);
		}

		@Override
		public StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere with(Expression... expressions) {
			return new DefaultStatementBuilder(this).with(expressions);
		}

		@Override
		public StatementBuilder.OrderableOngoingReadingAndWithWithoutWhere withDistinct(Expression... expressions) {
			return new DefaultStatementBuilder(this).withDistinct(expressions);
		}
	}
}
