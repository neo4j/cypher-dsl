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

import java.util.ArrayList;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Statement.SingleQuery;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/SinglePartQuery.html">SinglePartQuery</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
class SinglePartQuery extends AbstractStatement implements SingleQuery {

	static SinglePartQuery create(List<Visitable> precedingClauses, Clause returnOrFinish) {

		if (precedingClauses.isEmpty() || precedingClauses.get(precedingClauses.size() - 1) instanceof Match) {
			Assertions.notNull(returnOrFinish, "A returning or finishing clause is required.");
		}

		if (returnOrFinish == null) {
			if (precedingClauses.get(precedingClauses.size() - 1) instanceof ResultStatement) {
				return new SinglePartQueryAsResultStatementWrapper(precedingClauses);
			}
			return new SinglePartQuery(precedingClauses);
		} else {
			return new SinglePartQueryWithFinishingClause(precedingClauses, returnOrFinish);
		}
	}

	private final List<Visitable> precedingClauses;

	private SinglePartQuery(List<Visitable> precedingClauses) {

		this.precedingClauses = new ArrayList<>(precedingClauses);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		precedingClauses.forEach(c -> c.accept(visitor));
		visitor.leave(this);
	}

	static final class SinglePartQueryAsResultStatementWrapper extends SinglePartQuery implements ResultStatement {

		private SinglePartQueryAsResultStatementWrapper(List<Visitable> precedingClauses) {
			super(precedingClauses);
		}
	}

	static final class SinglePartQueryWithFinishingClause extends SinglePartQuery implements ResultStatement {

		private final Clause finishingClause;

		private SinglePartQueryWithFinishingClause(List<Visitable> precedingClauses, Clause finishingClause) {
			super(precedingClauses);

			this.finishingClause = finishingClause;
		}

		@Override
		public void accept(Visitor visitor) {
			visitor.enter(this);
			super.precedingClauses.forEach(c -> c.accept(visitor));
			finishingClause.accept(visitor);
			visitor.leave(this);
		}

		@Override
		public String toString() {
			return RendererBridge.render(this);
		}
	}
}
