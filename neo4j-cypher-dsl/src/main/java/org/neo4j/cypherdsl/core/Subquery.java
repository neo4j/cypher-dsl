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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Represents a  "callable" sub-query. A  sub-query can contain statement  that returns something  (standard statements,
 * union statements and calls to stored procedures, yielding their elements). Sub-queries can be nested.
 *
 * @author Michael J. Simons
 * @soundtrack Die Ärzte - Seitenhirsch
 * @neo4j.version 4.0.0
 * @since 2020.1.2
 */
@API(status = STABLE, since = "2020.1.2")
@Neo4jVersion(minimum = "4.0.0")
public final class Subquery extends AbstractClause implements Clause {

	private ImportingWith importingWith;
	private final Statement statement;

	/**
	 * The {@code statement} must return elements, either through a {@literal RETURN} or {@literal YIELD} clause.
	 *
	 * @param statement The statement to wrap into a sub-query.
	 * @param imports   additional imports
	 * @return A sub-query.
	 */
	static Subquery call(Statement statement, IdentifiableElement... imports) {
		return call(statement, false, imports);
	}

	/**
	 * The {@code statement} must return elements, either through a {@literal RETURN} or {@literal YIELD} clause.
	 *
	 * @param statement      The statement to wrap into a sub-query.
	 * @param skipAssertions Set to true to skip assertions about the  statement that should make up the sub-query. This
	 *                       comes in handy  when building a statement based  on a list of flat clauses  and not through
	 *                       the fluent api.
	 * @return A sub-query.
	 */
	static Subquery call(Statement statement, boolean skipAssertions, IdentifiableElement... imports) {

		if (!skipAssertions) {
			boolean validReturn = statement.doesReturnOrYield();
			if (!validReturn) {
				throw new IllegalArgumentException("Only a statement that returns elements, either via RETURN or YIELD, can be used in a subquery.");
			}
		}

		return new Subquery(ImportingWith.of(imports), statement);
	}

	private Subquery(ImportingWith importingWith, Statement statement) {
		this.importingWith = importingWith;
		this.statement = statement;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.importingWith.accept(visitor);
		statement.accept(visitor);
		visitor.leave(this);
	}

	@API(status = INTERNAL)
	InTransactions inTransactionsOf(Integer rows) {
		return new InTransactions(this, rows);
	}

	/**
	 * @return {@literal true} if this sub-query yields any items
	 */
	@API(status = INTERNAL)
	public boolean doesReturnOrYield() {
		return statement.doesReturnOrYield();
	}
}
