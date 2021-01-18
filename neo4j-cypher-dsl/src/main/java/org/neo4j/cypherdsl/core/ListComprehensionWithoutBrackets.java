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

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A reduced clone of the {@link ListComprehension} that will not get brackets around it, if it gets rendered.
 * Its usage is currently limited to
 * {@link Functions#reduce(Expression, Expression, SymbolicName, Expression, Expression)}.
 * For this reason, the class is kept internal for now.
 *
 * @since 2020.1.5
 */
@API(status = INTERNAL, since = "2020.1.5")
final class ListComprehensionWithoutBrackets implements Expression {

	/**
	 * The variable for the where part.
	 */
	private final SymbolicName variable;

	/**
	 * The list expression. No further assertions are taken to check beforehand if it is actually a Cypher List atm.
	 */
	private final Expression listExpression;

	/**
	 * The new list to be returned.
	 */
	private final Expression listDefinition;

	static OngoingDefinitionWithVariable with(SymbolicName variable) {

		Assertions.notNull(variable, "A variable is required");
		return new Builder(variable);
	}

	/**
	 * {@link #in(Expression)} must be used to define the source list.
	 */
	public interface OngoingDefinitionWithVariable {

		/**
		 * Create a list comprehension past on a literal list.
		 *
		 * @param list The source list.
		 * @return An ongoing definition
		 */
		OngoingDefinitionWithList in(Expression list);
	}

	/**
	 * Last state
	 */
	public interface OngoingDefinitionWithList extends OngoingDefinitionWithoutReturn { }

	/**
	 * Provides the final step of defining a list comprehension.
	 */
	public interface OngoingDefinitionWithoutReturn {

		/**
		 * @param variables the elements to be returned from the list
		 * @return The final definition of the list comprehension
		 * @see #returning(Expression...)
		 */
		default ListComprehensionWithoutBrackets returning(Named... variables) {
			return returning(Expressions.createSymbolicNames(variables));
		}

		/**
		 * @param listDefinition Defines the elements to be returned from the pattern
		 * @return The final definition of the list comprehension
		 */
		ListComprehensionWithoutBrackets returning(Expression... listDefinition);

	}

	private static class Builder
		implements OngoingDefinitionWithVariable, OngoingDefinitionWithList {

		private final SymbolicName variable;
		private Expression listExpression;

		private Builder(SymbolicName variable) {
			this.variable = variable;
		}

		@Override
		public OngoingDefinitionWithList in(Expression list) {
			this.listExpression = list;
			return this;
		}

		@Override
		public ListComprehensionWithoutBrackets returning(Expression... expressions) {

			return new ListComprehensionWithoutBrackets(variable, listExpression,
				ListExpression.listOrSingleExpression(expressions));
		}
	}

	private ListComprehensionWithoutBrackets(SymbolicName variable, Expression listExpression,
		Expression listDefinition) {
		this.variable = variable;
		this.listExpression = listExpression;
		this.listDefinition = listDefinition;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.variable.accept(visitor);
		Operator.IN.accept(visitor);
		this.listExpression.accept(visitor);
		if (this.listDefinition != null) {
			Operator.PIPE.accept(visitor);
			this.listDefinition.accept(visitor);
		}
		visitor.leave(this);
	}
}
