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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.List;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.UsingPeriodicCommit;

/**
 * This variant of a {@link Statement} takes in a plain list of clauses without any further checks. During rendering they
 * are visited in the same order as originally provided.
 *
 * @author Michael J. Simons
 * @soundtrack Black Sabbath - Master Of Reality
 * @since 2021.3.0
 */
@API(status = INTERNAL, since = "2021.3.0")
class ClausesBasedStatement extends AbstractStatement {

	private final UsingPeriodicCommit optionalPeriodicCommit;

	private final List<Clause> clauses;

	ClausesBasedStatement(@NotNull List<Clause> clauses, @Nullable UsingPeriodicCommit optionalPeriodicCommit) {
		this.optionalPeriodicCommit = optionalPeriodicCommit;
		this.clauses = List.copyOf(clauses);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		Visitable.visitIfNotNull(optionalPeriodicCommit, visitor);
		clauses.forEach(c -> c.accept(visitor));
		visitor.leave(this);
	}

	@Override
	public boolean doesReturnOrYield() {
		if (this.clauses.isEmpty()) {
			return false;
		}
		Clause lastClause = this.clauses.get(this.clauses.size() - 1);
		return lastClause instanceof Return || lastClause instanceof Finish;
	}

	@API(status = INTERNAL)
	List<Clause> getClauses() {
		return clauses;
	}
}
