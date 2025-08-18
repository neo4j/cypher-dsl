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
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;
import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A step exposing a several {@code where} methods that are provide entry points of adding
 * conditions.
 *
 * @param <T> the type of the owner exposing the {@literal WHERE} clause
 * @author Michael J. Simons
 * @since 2020.0.1
 */
@API(status = STABLE, since = "2020.0.1")
public interface ExposesWhere<T> {

	/**
	 * Adds a where clause to this fragement.
	 * @param condition the new condition, must not be {@literal null}
	 * @return a match or call restricted by a where clause with no return items yet.
	 */
	@CheckReturnValue
	T where(Condition condition);

	/**
	 * Adds a where clause based on a path pattern to this match. See <a href=
	 * "https://neo4j.com/docs/cypher-manual/4.0/clauses/where/#query-where-patterns">Using
	 * path patterns in WHERE</a>.
	 * @param pathPattern the path pattern to add to the where clause. This path pattern
	 * must not be {@literal null} and must not introduce new variables not available in
	 * the match.
	 * @return a match or a call restricted by a where clause with no return items yet.
	 * @since 1.0.1
	 */
	@CheckReturnValue
	default T where(RelationshipPattern pathPattern) {

		Assertions.notNull(pathPattern, "The path pattern must not be null.");
		return this.where(RelationshipPatternCondition.of(pathPattern));
	}

}
