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

import static org.apiguardian.api.API.Status.STABLE;

import java.util.Collection;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Represents the {@literal FOREACH} clause and is currently only producible via the Cypher-Parser.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class Foreach extends AbstractClause implements UpdatingClause {

	private final SymbolicName variable;

	private final Expression list;

	private final Collection<UpdatingClause> updatingClauses;

	Foreach(SymbolicName v, Expression list, Collection<UpdatingClause> updatingClauses) {
		this.variable = v;
		this.list = list;
		this.updatingClauses = updatingClauses;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.variable.accept(visitor);
		Operator.IN.accept(visitor);
		this.list.accept(visitor);
		Operator.PIPE.accept(visitor);
		this.updatingClauses.forEach(clause -> clause.accept(visitor));
		visitor.leave(this);
	}
}
