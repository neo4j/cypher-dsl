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
import org.neo4j.cypherdsl.core.ast.Visitor;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Unwind.html">Unwind</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class Unwind extends AbstractClause implements ReadingClause {

	private final Expression expressionToUnwind;

	Unwind(Expression expressionToUnwind, String variable) {

		this.expressionToUnwind = ((expressionToUnwind instanceof Aliased aliased) ? aliased.asName()
				: expressionToUnwind)
			.as(variable);
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.expressionToUnwind.accept(visitor);
		visitor.leave(this);
	}

}
