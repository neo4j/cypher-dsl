/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/SinglePartQuery.html">SinglePartQuery</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
final class SinglePartQuery extends AbstractStatement implements SingleQuery {

	private final List<Visitable> precedingClauses;

	private final Return aReturn;

	static SinglePartQuery create(List<Visitable> precedingClauses, Return aReturn) {

		if (precedingClauses.isEmpty() || precedingClauses.get(precedingClauses.size() - 1) instanceof Match) {
			Assertions.notNull(aReturn, "A return clause is required.");
		}

		return new SinglePartQuery(precedingClauses, aReturn);
	}

	private SinglePartQuery(List<Visitable> precedingClauses, Return aReturn) {

		this.precedingClauses = new ArrayList<>(precedingClauses);
		this.aReturn = aReturn;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		precedingClauses.forEach(c -> c.accept(visitor));
		Visitable.visitIfNotNull(aReturn, visitor);
		visitor.leave(this);
	}

	boolean doesReturnElements() {
		return this.aReturn != null;
	}
}
