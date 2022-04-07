/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Used for subqueries {@literal IN TRANSACTIONS}
 *
 * @author Michael J. Simons
 * @soundtrack Korn - Greatest Hits, Vol. 1
 * @since 2022.3.0
 */
@API(status = STABLE, since = "2022.3.0")
public final class InTransactions implements Visitable {

	private final Subquery subquery;

	@Nullable
	private final Integer rows;

	@API(status = INTERNAL)
	InTransactions(Subquery subquery, @Nullable Integer rows) {
		this.subquery = subquery;
		this.rows = rows;
	}

	@API(status = INTERNAL)
	@Nullable
	public Integer getRows() {
		return rows;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		subquery.accept(visitor);
		visitor.leave(this);
	}
}
