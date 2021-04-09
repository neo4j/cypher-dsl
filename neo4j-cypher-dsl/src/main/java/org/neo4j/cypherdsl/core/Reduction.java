/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.support.TypedSubtree;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;
import org.neo4j.cypherdsl.core.utils.CheckReturnValue;

/**
 * A typed subtree representing the arguments of a call to the {@code reduce()} function.
 *
 * @see Functions#reduce(SymbolicName)
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @soundtrack Metallica - Metallica
 * @since 2020.1.5
 */
@API(status = EXPERIMENTAL, since = "2020.1.5")
public final class Reduction extends TypedSubtree<Visitable> {

	/**
	 * Step 1: Define the variable of the reduction
	 *
	 * @param variable The closure will have a variable introduced in its context. We decide here which variable to use.
	 * @return An ongoing definition
	 */
	@NotNull @Contract(pure = true)
	static OngoingDefinitionWithVariable of(SymbolicName variable) {

		Assertions.notNull(variable, "A variable is required");
		return new Builder(variable);
	}

	private Reduction(Visitable... children) {
		super(children);
	}

	/**
	 * Step 2: Define the list
	 */
	public interface OngoingDefinitionWithVariable {

		/**
		 * @param list The list that is the subject of the reduction
		 * @return An ongoing definition
		 */
		@NotNull @CheckReturnValue
		OngoingDefinitionWithList in(Expression list);
	}

	/**
	 * Step 3: Define the map expression
	 */
	public interface OngoingDefinitionWithList {

		/**
		 * @param mapper This expression will run once per value in the list, and produce the result value.
		 * @return An ongoing definition
		 */
		@NotNull @CheckReturnValue
		OngoingDefinitionWithReducer map(Expression mapper);
	}

	/**
	 * Step 4a: Define the accumulator
	 */
	public interface OngoingDefinitionWithReducer {

		/**
		 * @param accumulator A variable that will hold the result and the partial results as the list is iterated.
		 * @return An ongoing definition
		 */
		@NotNull @CheckReturnValue
		OngoingDefinitionWithInitial accumulateOn(Expression accumulator);
	}

	/**
	 * Step 4b: Define the initial value
	 */
	public interface OngoingDefinitionWithInitial {

		/**
		 * @param initialValue An expression that runs once to give a starting value to the accumulator.
		 * @return An ongoing definition
		 */
		@NotNull @Contract(pure = true)
		FunctionInvocation withInitialValueOf(Expression initialValue);
	}

	private static class Builder
		implements OngoingDefinitionWithVariable, OngoingDefinitionWithList, OngoingDefinitionWithInitial,
		OngoingDefinitionWithReducer {

		private Expression accumulatorExpression;
		private final SymbolicName variable;
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

			Expression accumulatorAssignment = accumulatorExpression.isEqualTo(initialValue);
			ReductionPipeline reductionPipeline = new ReductionPipeline(variable, listExpression, mapExpression);

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
	}
}
