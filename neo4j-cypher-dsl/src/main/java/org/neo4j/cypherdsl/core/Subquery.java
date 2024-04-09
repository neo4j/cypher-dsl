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

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

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
@API(status = STABLE, since = "2020.1.2")
@Neo4jVersion(minimum = "4.0.0")
public final class Subquery implements Clause {

	private final With imports;
	private final With renames;
	private final Statement statement;
	private final RawLiteral rawStatement;

	static Subquery raw(String format, Object... mixedArgs) {
		return new Subquery(RawLiteral.create(format, mixedArgs));
	}

	/**
	 * The {@code statement} can either be a unit sub-query, used to modify the graph. Those won't impact the amount of
	 * rows returned by the enclosing query.
	 * In case it's a statement that returns or yields values it must ensure that it does not return variables with the
	 * same names as variables in the enclosing query.
	 *
	 * @param statement      The statement to wrap into a sub-query.
	 * @param imports   additional imports
	 * @return A sub-query.
	 */
	static Subquery call(Statement statement, IdentifiableElement... imports) {

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

			optionalImports = returnItems.isEmpty() ? null : new With(false, returnItems, null, null, null, null);

			returnItems = new ExpressionList(Arrays.stream(imports)
				.filter(AliasedExpression.class::isInstance)
				.map(AliasedExpression.class::cast)
				.collect(Collectors.toList()));

			optionalRenames = returnItems.isEmpty() ? null : new With(false, returnItems, null, null, null, null);
		}

		return new Subquery(optionalImports, optionalRenames, statement);
	}

	private Subquery(@Nullable With imports, @Nullable With renames, Statement statement) {
		this.imports = imports;
		this.renames = renames;
		this.statement = statement;
		this.rawStatement = null;
	}

	private Subquery(RawLiteral rawStatement) {
		this.rawStatement = rawStatement;
		this.imports = null;
		this.renames = null;
		this.statement = null;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		if (this.rawStatement != null) {
			this.rawStatement.accept(visitor);
		} else {
			Visitable.visitIfNotNull(this.imports, visitor);
			Visitable.visitIfNotNull(this.renames, visitor);
			this.statement.accept(visitor);
		}
		visitor.leave(this);
	}

	@API(status = INTERNAL)
	InTransactions inTransactionsOf(Integer rows) {
		return new InTransactions(this, rows);
	}

	@API(status = INTERNAL)
	public boolean doesReturnOrYield() {
		return statement != null && statement.doesReturnOrYield();
	}
}
