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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A typed subtree representing the arguments of a call to the {@code reduce()} function.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 2020.1.5
 * @see Cypher#reduce(SymbolicName)
 */
@API(status = STABLE, since = "2020.1.5")
public final class Reduction extends TypedSubtree<Visitable> {

	private Reduction(Visitable... children) {
		super(children);
	}

	/**
	 * Step 1: Define the variable of the reduction.
	 * @param variable the closure will have a variable introduced in its context. We
	 * decide here which variable to use.
	 * @return an ongoing definition
	 */
	static OngoingDefinitionWithVariable of(SymbolicName variable) {

		Assertions.notNull(variable, "A variable is required");
		return new Builder(variable);
	}

	/**
	 * Step 2: Define the list.
	 */
	public interface OngoingDefinitionWithVariable {

		/**
		 * Returns an ongoing definition.
		 * @param list the list that is the subject of the reduction
		 * @return an ongoing definition
		 */
		@CheckReturnValue
		OngoingDefinitionWithList in(Expression list);

	}

	/**
	 * Step 3: Define the map expression.
	 */
	public interface OngoingDefinitionWithList {

		/**
		 * Return an ongoing definition.
		 * @param mapper this expression will run once per value in the list, and produce
		 * the result value.
		 * @return an ongoing definition
		 */
		@CheckReturnValue
		OngoingDefinitionWithReducer map(Expression mapper);

	}

	/**
	 * Step 4a: Define the accumulator.
	 */
	public interface OngoingDefinitionWithReducer {

		/**
		 * Return an ongoing definition.
		 * @param accumulator a variable that will hold the result and the partial results
		 * as the list is iterated.
		 * @return an ongoing definition
		 */
		@CheckReturnValue
		OngoingDefinitionWithInitial accumulateOn(Expression accumulator);

	}

	/**
	 * Step 4b: Define the initial value.
	 */
	public interface OngoingDefinitionWithInitial {

		/**
		 * Return an ongoing definition.
		 * @param initialValue an expression that runs once to give a starting value to
		 * the accumulator.
		 * @return an ongoing definition
		 */
		FunctionInvocation withInitialValueOf(Expression initialValue);

	}

	private static final class Builder implements OngoingDefinitionWithVariable, OngoingDefinitionWithList,
			OngoingDefinitionWithInitial, OngoingDefinitionWithReducer {

		private final SymbolicName variable;

		private Expression accumulatorExpression;

		private Expression listExpression;

		private Expression mapExpression;

		private Builder(SymbolicName variable) {
			this.variable = variable;
		}

		@Override
		public OngoingDefinitionWithList in(Expression list) {

			this.listExpression = list;
			return this;
		}

		@Override
		public OngoingDefinitionWithReducer map(Expression mapper) {

			this.mapExpression = mapper;
			return this;
		}

		@Override
		public OngoingDefinitionWithInitial accumulateOn(Expression accumulator) {

			this.accumulatorExpression = accumulator;
			return this;
		}

		@Override
		public FunctionInvocation withInitialValueOf(Expression initialValue) {

			Expression accumulatorAssignment = this.accumulatorExpression.isEqualTo(initialValue);
			ReductionPipeline reductionPipeline = new ReductionPipeline(this.variable, this.listExpression,
					this.mapExpression);

			Reduction reduction = new Reduction(accumulatorAssignment, reductionPipeline);
			return FunctionInvocation.create(BuiltInFunctions.Lists.REDUCE, reduction);
		}

	}

	private static final class ReductionPipeline implements Visitable {

		private final SymbolicName variable;

		private final Expression list;

		private final Expression expression;

		ReductionPipeline(SymbolicName variable, Expression list, Expression expression) {

			this.variable = variable;
			this.list = list;
			this.expression = expression;
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.enter(this);
			this.variable.accept(visitor);
			Operator.IN.accept(visitor);
			this.list.accept(visitor);
			Operator.PIPE.accept(visitor);
			this.expression.accept(visitor);
			visitor.leave(this);
		}

		@Override
		public String toString() {
			return RendererBridge.render(this);
		}

	}

}
