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

import org.apiguardian.api.API;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Implementing <a href="https://neo4j.com/docs/cypher-manual/current/subqueries/collect/">COLLECT subqueries</a>
 *
 * @author Michael J. Simons
 * @soundtrack The Cross - Blue Rock
 * @since 2023.8.0
 */
@API(status = STABLE, since = "2023.0.0")
@Neo4jVersion(minimum = "5.6")
public final class CollectExpression implements SubqueryExpression {

	private final ImportingWith optionalWith;

	private final Statement resultStatement;


	static CollectExpression collect(Statement statement, IdentifiableElement... imports) {

		return new CollectExpression(ImportingWith.of(imports), statement);
	}

	static CollectExpression collect(Statement resultStatement) {
		return new CollectExpression(new ImportingWith(), resultStatement);
	}

	static CollectExpression collect(@Nullable With optionalWith, Statement resultStatement) {
		return new CollectExpression(new ImportingWith(optionalWith, null), resultStatement);
	}

	private CollectExpression(ImportingWith optionalWith, Statement resultStatement) {

		this.optionalWith = optionalWith;
		this.resultStatement = resultStatement;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.optionalWith.accept(visitor);
		this.resultStatement.accept(visitor);
		visitor.leave(this);
	}
}
