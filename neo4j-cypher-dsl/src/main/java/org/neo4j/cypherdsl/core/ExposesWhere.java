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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A step exposing a several {@code where} methods that are provide entry points of adding conditions.
 *
 * @author Michael J. Simons
 * @soundtrack Smoke Blow - Dark Angel
 * @since 2020.0.1
 */
@API(status = EXPERIMENTAL, since = "2020.0.1")
public interface ExposesWhere {

	/**
	 * Adds a where clause to this fragement.
	 *
	 * @param condition The new condition, must not be {@literal null}
	 * @return A match or call restricted by a where clause with no return items yet.
	 */
	StatementBuilder.OngoingReadingWithWhere where(Condition condition);

	/**
	 * Adds a where clause based on a path pattern to this match.
	 * See <a href="https://neo4j.com/docs/cypher-manual/4.0/clauses/where/#query-where-patterns">Using path patterns in WHERE</a>.
	 *
	 * @param pathPattern The path pattern to add to the where clause.
	 *                    This path pattern must not be {@literal null} and must
	 *                    not introduce new variables not available in the match.
	 * @return A match or a call restricted by a where clause with no return items yet.
	 * @since 1.0.1
	 */
	default StatementBuilder.OngoingReadingWithWhere where(RelationshipPattern pathPattern) {

		Assertions.notNull(pathPattern, "The path pattern must not be null.");
		return this.where(new RelationshipPatternCondition(pathPattern));
	}
}
