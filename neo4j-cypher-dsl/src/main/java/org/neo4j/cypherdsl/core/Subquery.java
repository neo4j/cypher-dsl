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
import org.neo4j.cypherdsl.core.Statement.ResultQuery;
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
public final class Subquery implements Visitable {

	private final Statement statement;

	/**
	 * The {@code statement} must return elements, either through a {@literal RETURN} or {@literal YIELD} clause.
	 *
	 * @param statement The statement to wrap into a subquery.
	 * @return A subquery.
	 */
	static Subquery call(Statement statement) {

		boolean validReturn = statement instanceof ResultQuery || statement instanceof UnionQuery;
		if (!validReturn) {
			throw new IllegalArgumentException("Only a statement that returns elements, either via RETURN or YIELD, can be used in a subquery.");
		}

		return new Subquery(statement);
	}

	Subquery(Statement statement) {
		this.statement = statement;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		statement.accept(visitor);
		visitor.leave(this);
	}
}
