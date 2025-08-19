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

import java.util.ArrayList;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Implementation of a {@code UNION} query.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
final class UnionQueryImpl extends AbstractStatement implements Statement.UnionQuery {

	private final boolean all;

	private final Statement firstQuery;

	private final List<UnionPart> additionalQueries;

	private UnionQueryImpl(boolean all, Statement firstQuery, List<UnionPart> additionalQueries) {
		this.all = all;
		this.firstQuery = firstQuery;
		this.additionalQueries = additionalQueries;
	}

	@SuppressWarnings("squid:S6416") // This is about the assertion, Sonar suddenly things
										// this is an issue: Idk. We want the exception.
	static UnionQueryImpl create(boolean unionAll, List<Statement> queries) {

		Assertions.isTrue(queries != null && queries.size() >= 2, "At least two queries are needed.");

		@SuppressWarnings("squid:S2259") // Really, we asserted it
		List<UnionPart> unionParts = queries.stream().skip(1).map(q -> new UnionPart(unionAll, q)).toList();
		return new UnionQueryImpl(unionAll, queries.get(0), unionParts);
	}

	/**
	 * Creates a new union query by appending more parts.
	 * @param newAdditionalQueries more additional queries
	 * @return a new union query
	 */
	UnionQueryImpl addAdditionalQueries(List<Statement> newAdditionalQueries) {

		List<Statement> queries = new ArrayList<>();
		queries.add(this.firstQuery);
		queries.addAll(this.additionalQueries.stream().map(UnionPart::getQuery).toList());
		queries.addAll(newAdditionalQueries);

		return create(this.isAll(), queries);
	}

	boolean isAll() {
		return this.all;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.firstQuery.accept(visitor);
		this.additionalQueries.forEach(q -> q.accept(visitor));
		visitor.leave(this);
	}

}
