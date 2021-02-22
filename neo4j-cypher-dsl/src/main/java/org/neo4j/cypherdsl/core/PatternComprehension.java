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
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/PatternComprehension.html">PatternComprehension</a>
 * and <a href="https://neo4j.com/docs/cypher-manual/current/syntax/lists/#cypher-pattern-comprehension">the corresponding cypher manual entry</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class PatternComprehension implements Expression {

	static OngoingDefinitionWithPattern basedOn(RelationshipPattern pattern) {

		Assertions.notNull(pattern, "A pattern is required");
		return new Builder(pattern);
	}

	static OngoingDefinitionWithPattern basedOn(NamedPath pattern) {

		Assertions.notNull(pattern, "A pattern is required");
		return new Builder(pattern);
	}

	/**
	 * Provides the final step of defining a pattern comprehension.
	 */
	public interface OngoingDefinitionWithoutReturn {

		/**
		 * @param variables the elements to be returned from the pattern
		 * @return The final definition of the pattern comprehension
		 * @see #returning(Expression...)
		 */
		default PatternComprehension returning(Named... variables) {
			return returning(Expressions.createSymbolicNames(variables));
		}

		/**
		 * @param listDefinition Defines the elements to be returned from the pattern
		 * @return The final definition of the pattern comprehension
		 */
		PatternComprehension returning(Expression... listDefinition);
	}

	/**
	 * Allows to add a where clause into the definition of the pattern.
	 */
	public interface OngoingDefinitionWithPattern extends OngoingDefinitionWithoutReturn {

		/**
		 * Adds a {@code WHERE} clause to the inner statement of the pattern comprehension
		 *
		 * @param condition An initial condition to be used with {@code WHERE}
		 * @return An ongoing definition of a pattern comprehension for furhter modification
		 */
		OngoingDefinitionWithPatternAndWhere where(Condition condition);

		/**
		 * Adds a where clause based on a path pattern to the ongoing definition
		 *
		 * @param pathPattern The path pattern to add to the where clause.
		 *                    This path pattern must not be {@literal null} and must
		 *                    not introduce new variables not available in the match.
		 * @return A match or a call restricted by a where clause with no return items yet.
		 * @since 2020.1.4
		 */
		default OngoingDefinitionWithPatternAndWhere where(RelationshipPattern pathPattern) {

			Assertions.notNull(pathPattern, "The path pattern must not be null.");
			return this.where(new RelationshipPatternCondition(pathPattern));
		}
	}

	/**
	 * Intermediate step that allows expressing additional, logical operators.
	 */
	public interface OngoingDefinitionWithPatternAndWhere extends OngoingDefinitionWithoutReturn, ExposesLogicalOperators<OngoingDefinitionWithPatternAndWhere>  {
	}

	/**
	 * Ongoing definition of a pattern comprehension. Can be defined without a where-clause now.
	 */
	private static class Builder implements OngoingDefinitionWithPattern, OngoingDefinitionWithPatternAndWhere  {
		private final PatternElement pattern;
		private final DefaultStatementBuilder.ConditionBuilder conditionBuilder = new DefaultStatementBuilder.ConditionBuilder();

		private Builder(PatternElement pattern) {
			this.pattern = pattern;
		}

		@Override
		public OngoingDefinitionWithPatternAndWhere where(Condition condition) {
			conditionBuilder.where(condition);
			return this;
		}

		@Override
		public OngoingDefinitionWithPatternAndWhere and(Condition condition) {
			conditionBuilder.and(condition);
			return this;
		}

		@Override
		public OngoingDefinitionWithPatternAndWhere or(Condition condition) {
			conditionBuilder.or(condition);
			return this;
		}

		@Override
		public PatternComprehension returning(Expression... expressions) {
			Where where = conditionBuilder.buildCondition().map(Where::new).orElse(null);
			return new PatternComprehension(pattern, where, ListExpression.listOrSingleExpression(expressions));
		}
	}

	private final PatternElement pattern;
	private final Where where;
	private final Expression listDefinition;

	private PatternComprehension(PatternElement pattern, Where where, Expression listDefinition) {
		this.pattern = pattern;
		this.where = where;
		this.listDefinition = listDefinition;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.pattern.accept(visitor);
		Visitable.visitIfNotNull(this.where, visitor);
		Operator.PIPE.accept(visitor);
		this.listDefinition.accept(visitor);
		visitor.leave(this);
	}
}
