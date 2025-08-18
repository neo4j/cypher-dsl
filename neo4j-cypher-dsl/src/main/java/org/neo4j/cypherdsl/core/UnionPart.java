/*
 * Copyright (c) 2019-2025 "Neo4j,"
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
 * Represents a part of a union.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class UnionPart implements Visitable {

	private final boolean all;

	private final Statement query;

	UnionPart(boolean all, Statement query) {
		this.all = all;
		this.query = query;
	}

	/**
	 * {@return <code>true</code>, if an <code>ALL</code> keyword should be rendered}
	 */
	@API(status = INTERNAL)
	public boolean isAll() {
		return this.all;
	}

	Statement getQuery() {
		return this.query;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.query.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
