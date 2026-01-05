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
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Atom.html#ListComprehension">ListComprehension</a>
 * and <a href=
 * "https://neo4j.com/docs/cypher-manual/current/syntax/lists/#cypher-list-comprehension">the
 * corresponding cypher manual entry</a>.
 *
 * @author Michael J. Simons
 * @since 1.0.1
 */
@API(status = STABLE, since = "1.0.1")
public final class ListComprehension implements Expression {

	// Modelling from the FilterExpression:
	// https://s3.amazonaws.com/artifacts.opencypher.org/M14/railroad/FilterExpression.html
	// */

	/**
	 * The variable for the where part.
	 */
	private final SymbolicName variable;

	/**
	 * The list expression. No further assertions are taken to check beforehand if it is
	 * actually a Cypher List atm.
	 */
	private final Expression listExpression;

	/**
	 * Filtering on the list.
	 */
	private final Where where;

	/**
	 * The new list to be returned.
	 */
	private final Expression listDefinition;

	private ListComprehension(SymbolicName variable, Expression listExpression, Where where,
			Expression listDefinition) {
		this.variable = variable;
		this.listExpression = listExpression;
		this.where = where;
		this.listDefinition = listDefinition;
	}

	static OngoingDefinitionWithVariable with(SymbolicName variable) {

		Assertions.notNull(variable, "A variable is required");
		return new Builder(variable);
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.variable.accept(visitor);
		Operator.IN.accept(visitor);
		this.listExpression.accept(visitor);
		Visitable.visitIfNotNull(this.where, visitor);
		if (this.listDefinition != null) {
			Operator.PIPE.accept(visitor);
			this.listDefinition.accept(visitor);
		}
		visitor.leave(this);
	}

	/**
	 * {@link #in(Expression)} must be used to define the source list.
	 */
	public interface OngoingDefinitionWithVariable {

		/**
		 * Create a list comprehension past on a literal list.
		 * @param list the source list.
		 * @return an ongoing definition
		 */
		@CheckReturnValue
		OngoingDefinitionWithList in(Expression list);

	}

	/**
	 * Allows to add a where clause into the definition of the list.
	 */
	public interface OngoingDefinitionWithList extends OngoingDefinitionWithoutReturn {

		/**
		 * Adds a {@code WHERE} clause to this comprehension.
		 * @param condition the condition to start the {@code WHERE} clause with.
		 * @return an ongoing definition
		 */
		@CheckReturnValue
		OngoingDefinitionWithoutReturn where(Condition condition);

	}

	/**
	 * Provides the final step of defining a list comprehension.
	 */
	public interface OngoingDefinitionWithoutReturn {

		/**
		 * Defines the {@code RETURN} clause.
		 * @param variables the elements to be returned from the list
		 * @return the final definition of the list comprehension
		 * @see #returning(Expression...)
		 */
		default ListComprehension returning(Named... variables) {
			return returning(Expressions.createSymbolicNames(variables));
		}

		/**
		 * Defines the {@code RETURN} clause.
		 * @param listDefinition defines the elements to be returned from the pattern
		 * @return the final definition of the list comprehension
		 */
		ListComprehension returning(Expression... listDefinition);

		/**
		 * Defines an empty {@code RETURN} clause.
		 * @return returns the list comprehension as is, without a {@literal WHERE} and
		 * returning each element of the original list
		 */
		ListComprehension returning();

	}

	private static final class Builder implements OngoingDefinitionWithVariable, OngoingDefinitionWithList {

		private final SymbolicName variable;

		private Expression listExpression;

		private Where where;

		private Builder(SymbolicName variable) {
			this.variable = variable;
		}

		@Override
		public OngoingDefinitionWithList in(Expression list) {
			this.listExpression = list;
			return this;
		}

		@Override
		public OngoingDefinitionWithoutReturn where(Condition condition) {
			this.where = new Where(condition);
			return this;
		}

		@Override
		public ListComprehension returning() {

			return new ListComprehension(this.variable, this.listExpression, this.where, null);
		}

		@Override
		public ListComprehension returning(Expression... expressions) {

			return new ListComprehension(this.variable, this.listExpression, this.where,
					ListExpression.listOrSingleExpression(expressions));
		}

	}

}
