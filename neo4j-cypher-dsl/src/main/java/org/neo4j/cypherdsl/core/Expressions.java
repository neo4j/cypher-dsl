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

import static org.apiguardian.api.API.Status.STABLE;

import java.util.Arrays;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.Statement.UnionQuery;

/**
 * Utility methods for dealing with expressions.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Expressions {

	/**
	 * Something that can build counting sub-queries. Might be used in the future for existential sub-queries, too.
	 *
	 * @since 2023.0.0
	 */
	public interface SubqueryExpressionBuilder {

		/**
		 * Creates a {@literal COUNT} sub-query expressions from at least one pattern.
		 *
		 * @param requiredPattern One pattern is required
		 * @param patternElement Optional pattern
		 * @return The immutable {@link CountExpression}
		 */
		@NotNull
		CountExpression count(PatternElement requiredPattern, PatternElement... patternElement);

		/**
		 * Creates a {@literal COUNT} with an inner {@literal UNION} sub-query.
		 *
		 * @param union The union that will be the source of the {@literal COUNT} sub-query
		 * @return The immutable {@link CountExpression}
		 * @since 2023.0.0
		 */
		@NotNull
		CountExpression count(UnionQuery union);
	}

	/**
	 * Creates a {@literal COUNT} sub-query expressions from at least one pattern.
	 *
	 * @param requiredPattern One pattern is required
	 * @param patternElement Optional pattern
	 * @return The immutable {@link CountExpression}
	 * @since 2023.0.0
	 */
	@NotNull
	public static CountExpression count(PatternElement requiredPattern, PatternElement... patternElement) {
		return CountExpression.count(Pattern.of(requiredPattern, patternElement));
	}

	/**
	 * Creates a {@literal COUNT} with an inner {@literal UNION} sub-query.
	 *
	 * @param union The union that will be the source of the {@literal COUNT} sub-query
	 * @return The immutable {@link CountExpression}
	 * @since 2023.0.0
	 */
	@NotNull
	public static CountExpression count(UnionQuery union) {
		return CountExpression.count(union);
	}

	/**
	 * Creates a {@literal COUNT} from a full statement, including  its filters and conditions. The statement may or may
	 * not have a {@literal RETURN} clause. It must however not contain any updates. While it would render syntactically
	 * correct Cypher, Neo4j does not support updates inside counting sub-queries.
	 *
	 * @param statement The statement to be passed to {@code count{}}
	 * @param imports   Optional imports to be used in the statement (will be imported with {@literal WITH})
	 * @return A counting sub-query.
	 * @since 2023.1.0
	 */
	@NotNull
	public static CountExpression count(Statement statement, IdentifiableElement... imports) {
		return CountExpression.count(statement, imports);
	}

	/**
	 * Start building a new sub-query expression by importing variables into the scope with a {@literal WITH} clause.
	 *
	 * @param identifiableElements The identifiable elements to import
	 * @return A builder for creating the concrete sub-query
	 * @since 2023.0.0
	 */
	public static SubqueryExpressionBuilder with(String... identifiableElements) {

		return with(Arrays.stream(identifiableElements).map(SymbolicName::of).toArray(SymbolicName[]::new));
	}

	/**
	 * Start building a new sub-query expression by importing variables into the scope with a {@literal WITH} clause.
	 *
	 * @param identifiableElements The identifiable elements to import
	 * @return A builder for creating the concrete sub-query
	 * @since 2023.0.0
	 */
	public static SubqueryExpressionBuilder with(IdentifiableElement... identifiableElements) {

		var returnItems = new ExpressionList(Arrays.stream(identifiableElements).map(IdentifiableElement::asExpression).toList());
		var with = new With(false, returnItems, null, null, null, null);
		return new SubqueryExpressionBuilder() {
			@Override @NotNull
			public CountExpression count(PatternElement requiredPattern, PatternElement... patternElement) {
				return CountExpression.count(with, Pattern.of(requiredPattern, patternElement));
			}

			@Override @NotNull
			public CountExpression count(UnionQuery union) {
				return CountExpression.count(with, union);
			}
		};
	}

	/**
	 * @param expression Possibly named with a non-empty symbolic name.
	 * @param <T>        The type being returned
	 * @return The name of the expression if the expression is named or the expression itself.
	 */
	static <T extends Expression> Expression nameOrExpression(T expression) {

		if (expression instanceof Named) {
			return ((Named) expression).getSymbolicName().map(Expression.class::cast).orElse(expression);
		} else {
			return expression;
		}
	}

	static SymbolicName[] createSymbolicNames(String[] variables) {
		return Arrays.stream(variables).map(SymbolicName::of).toArray(SymbolicName[]::new);
	}

	static SymbolicName[] createSymbolicNames(Named[] variables) {
		return Arrays.stream(variables).map(Named::getRequiredSymbolicName)
			.toArray(SymbolicName[]::new);
	}

	static String format(Expression expression) {

		if (expression instanceof Named) {
			return ((Named) expression).getRequiredSymbolicName().getValue();
		} else if (expression instanceof AliasedExpression) {
			return ((AliasedExpression) expression).getAlias();
		} else if (expression instanceof SymbolicName) {
			return ((SymbolicName) expression).getValue();
		} else if (expression instanceof Property) {
			StringBuilder ref = new StringBuilder();
			expression.accept(segment -> {
				if (segment instanceof SymbolicName) {
					if (ref.length() > 0) {
						ref.append(".");
					}
					ref.append(((SymbolicName) segment).getValue());

				}
			});
			return ref.toString();
		}

		throw new IllegalArgumentException("Cannot format expression " + expression.toString());
	}

	private Expressions() {
	}
}
