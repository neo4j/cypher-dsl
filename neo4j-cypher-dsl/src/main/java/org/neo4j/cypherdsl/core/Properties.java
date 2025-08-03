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

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Represents the properties of a {@link Node node} or a {@link Relationship relationship} when used as part of the
 * whole pattern (inside a {@code MATCH}, {@code CREATE} or {@code MERGE} clause as {@code {p1: v1, p2: v2, pn: vn}}.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Properties implements Visitable {

	private final MapExpression value;

	/**
	 * Wraps an expression into a {@link Properties} node.
	 *
	 * @param expression Nullable expression
	 * @return A properties expression
	 */
	public static Properties create(MapExpression expression) {

		return expression == null ? null : new Properties(expression);
	}

	private Properties(MapExpression value) {
		this.value = value;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.value.accept(visitor);
		visitor.leave(this);
	}

	public String toString() {
		return RendererBridge.render(this);
	}
}
