/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import static org.apiguardian.api.API.Status.DEPRECATED;

import java.util.Arrays;
import java.util.List;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.Statement.UnionQuery;

/**
 * Utility methods for dealing with expressions.
 *
 * @author Michael J. Simons
 * @since 1.0
 * @deprecated use {@link Cypher} instead. This class will become package private in the next major release and no longer
 * be accessible.
 */
@API(status = DEPRECATED, since = "2023.9.0")
@Deprecated(since = "2023.9.0")
@SuppressWarnings({ "squid:S1133" }) // Suppresses warnings about removing deprecations
public final class Expressions {

	/**
	 * @deprecated use {@link org.neo4j.cypherdsl.core.SubqueryExpressionBuilder} instead.
	 */
	@API(status = DEPRECATED, since = "2023.9.0")
	@Deprecated(since = "2023.9.0")
	public interface SubqueryExpressionBuilder extends org.neo4j.cypherdsl.core.SubqueryExpressionBuilder {
	}

	/**
	 * Creates a {@literal COUNT} sub-query expressions from at least one pattern.
	 *
	 * @param requiredPattern One pattern is required
	 * @param patternElement  Optional pattern
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
	 * Creates a {@literal COUNT} expression based on a list of pattern
	 *
	 * @param pattern the list of patterns that shall be counted
	 * @param where   an optional where-clause
	 * @return a count expression.
	 * @since 2023.9.0
	 */
	public static CountExpression count(List<PatternElement> pattern, @Nullable Where where) {

		return CountExpression.count(pattern, where);
	}

	/**
	 * Start building a new sub-query expression by importing variables into the scope with a {@literal WITH} clause.
	 *
	 * @param identifiableElements The identifiable elements to import
	 * @return A builder for creating the concrete sub-query
	 * @since 2023.0.0
	 * @deprecated use {@link Cypher#subqueryWith(String...)} instead.
	 */
	@Deprecated
	public static SubqueryExpressionBuilder with(String... identifiableElements) {

		return with(Arrays.stream(identifiableElements).map(SymbolicName::of).toArray(SymbolicName[]::new));
	}

	/**
	 * Start building a new sub-query expression by importing variables into the scope with a {@literal WITH} clause.
	 *
	 * @param identifiableElements The identifiable elements to import
	 * @return A builder for creating the concrete sub-query
	 * @since 2023.0.0
	 * @deprecated use {@link Cypher#subqueryWith(IdentifiableElement...)} instead.
	 */
	@Deprecated
	public static SubqueryExpressionBuilder with(IdentifiableElement... identifiableElements) {

		var returnItems = new ExpressionList(
			Arrays.stream(identifiableElements).map(IdentifiableElement::asExpression).toList());
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

			@Override
			public CollectExpression collect(Statement statement) {
				return CollectExpression.collect(with, statement);
			}
		};
	}

	/**
	 * Creates a {@literal COLLECT} subquery from a statement, including  its filters and conditions. The statement must
	 * return exactly one column. It must however not contain any updates. While it would render syntactically
	 * correct Cypher, Neo4j does not support updates inside counting sub-queries.
	 *
	 * @param statement the statement to be passed to {@code COLLECT{}}
	 * @return a collecting sub-query.
	 * @since 2023.8.0
	 */
	@NotNull
	public static Expression collect(Statement statement) {

		if (!statement.doesReturnOrYield()) {
			throw new IllegalArgumentException(
				"The final RETURN clause in a subquery used with COLLECT is mandatory and the RETURN clause must return exactly one column.");
		}

		return CollectExpression.collect(statement);
	}

	/**
	 * @param expression Possibly named with a non-empty symbolic name.
	 * @param <T>        The type being returned
	 * @return The name of the expression if the expression is named or the expression itself.
	 */
	static <T extends Expression> Expression nameOrExpression(T expression) {

		if (expression instanceof Named named) {
			return named.getSymbolicName().map(Expression.class::cast).orElse(expression);
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

		if (expression instanceof Named named) {
			return named.getRequiredSymbolicName().getValue();
		} else if (expression instanceof AliasedExpression aliasedExpression) {
			return aliasedExpression.getAlias();
		} else if (expression instanceof SymbolicName symbolicName) {
			return symbolicName.getValue();
		} else if (expression instanceof Property) {
			StringBuilder ref = new StringBuilder();
			expression.accept(segment -> {
				if (segment instanceof SymbolicName symbolicName) {
					if (!ref.isEmpty()) {
						ref.append(".");
					}
					ref.append(symbolicName.getValue());

				}
			});
			return ref.toString();
		}

		throw new IllegalArgumentException("Cannot format expression " + expression.toString());
	}

	private Expressions() {
	}
}
