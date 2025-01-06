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
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Roughly corresponding to <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Match.html#Where">Where</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Where implements Visitable {

	private final Condition condition;

	/**
	 * Creates a new {@literal WHERE}
	 *
	 * @param optionalWhere An optional expression that must be usable {@link Expression#asCondition() "as condition"}.
	 * @return A {@literal WHERE} expression or null when {@code optionalWhere} has been {@literal NULL}
	 * @since 2022.0.0
	 */
	@Nullable
	public static Where from(@Nullable Expression optionalWhere) {
		return optionalWhere == null ? null : new Where(optionalWhere.asCondition());
	}

	Where(Condition condition) {
		this.condition = condition;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);

		this.condition.accept(visitor);

		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}
}
