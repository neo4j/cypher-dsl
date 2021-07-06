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

import java.util.Arrays;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Represents a "callable" subquery. A subquery can contain statement that returns something (standard statements, union
 * statements and calls to stored procedures, yielding their elements). Subqueries can be nested.
 *
 * @author Michael J. Simons
 * @soundtrack Die Ärzte - Seitenhirsch
 * @neo4j.version 4.0.0
 * @since 2020.1.2
 */
@API(status = EXPERIMENTAL, since = "2020.1.2")
@Neo4jVersion(minimum = "4.0.0")
public final class Subquery implements Clause {

	private final With imports;
	private final With renames;
	private final Statement statement;

	/**
	 * The {@code statement} must return elements, either through a {@literal RETURN} or {@literal YIELD} clause.
	 *
	 * @param statement The statement to wrap into a subquery.
	 * @param imports   additional imports
	 * @return A subquery.
	 */
	static Subquery call(Statement statement, IdentifiableElement... imports) {
		return call(statement, false, imports);
	}

	/**
	 * The {@code statement} must return elements, either through a {@literal RETURN} or {@literal YIELD} clause.
	 *
	 * @param statement      The statement to wrap into a subquery.
	 * @param skipAssertions Set to true to skip assertions about the statement that should make up the subquery. This comes in handy
	 *                       when building a statement based on a list of flat clauses and not through the fluent api.
	 * @return A subquery.
	 */
	static Subquery call(Statement statement, boolean skipAssertions, IdentifiableElement... imports) {

		if (!skipAssertions) {
			boolean validReturn = statement.doesReturnOrYield();
			if (!validReturn) {
				throw new IllegalArgumentException("Only a statement that returns elements, either via RETURN or YIELD, can be used in a subquery.");
			}
		}

		With optionalImports = null;
		With optionalRenames = null;
		if (imports.length > 0) {
			ExpressionList returnItems = new ExpressionList(Arrays.stream(imports)
				.map(i -> {
					if (i instanceof AliasedExpression) {
						return ((AliasedExpression) i).getDelegate();
					} else {
						return i.asExpression();
					}
				})
				.collect(Collectors.toList()));

			optionalImports = new With(false, returnItems, null, null, null, null);

			returnItems = new ExpressionList(Arrays.stream(imports)
				.filter(i -> i instanceof AliasedExpression)
				.map(AliasedExpression.class::cast)
				.collect(Collectors.toList()));

			optionalRenames = new With(false, returnItems, null, null, null, null);
		}

		return new Subquery(optionalImports, optionalRenames, statement);
	}

	private Subquery(@Nullable With imports, @Nullable With renames, Statement statement) {
		this.imports = imports;
		this.renames = renames;
		this.statement = statement;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		Visitable.visitIfNotNull(this.imports, visitor);
		Visitable.visitIfNotNull(this.renames, visitor);
		statement.accept(visitor);
		visitor.leave(this);
	}
}
