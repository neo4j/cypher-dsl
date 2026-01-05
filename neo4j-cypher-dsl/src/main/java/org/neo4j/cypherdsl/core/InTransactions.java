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
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * Used for subqueries {@code IN TRANSACTIONS}.
 *
 * @author Michael J. Simons
 * @since 2022.3.0
 */
@API(status = STABLE, since = "2022.3.0")
public final class InTransactions implements Visitable {

	private final Subquery subquery;

	private final Integer rows;

	@API(status = INTERNAL)
	InTransactions(Subquery subquery, Integer rows) {
		this.subquery = subquery;
		this.rows = rows;
	}

	/**
	 * {@return number of rows in this transaction}
	 */
	@API(status = INTERNAL)
	public Integer getRows() {
		return this.rows;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.subquery.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
