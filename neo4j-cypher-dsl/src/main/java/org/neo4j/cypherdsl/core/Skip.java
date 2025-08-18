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
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Representation of the {@code SKIP} clause.
 *
 * @author Gerrit Meier
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Skip implements Visitable {

	private final Expression expression;

	private Skip(Expression expression) {
		this.expression = expression;
	}

	static Skip create(Expression expression) {

		Assertions.notNull(expression, "Cannot skip a value of null.");

		return new Skip(expression);
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.expression.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
