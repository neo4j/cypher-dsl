/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A step that allows to specify a {@code SEARCH} clause after a {@code MATCH} clause. See
 * {@link Cypher#search(SymbolicName)} for defining such a clause.
 *
 * @author Michael J. Simons
 * @since 2025.3.0
 */
@API(status = STABLE, since = "2025.3.0")
public interface ExposesSearch {

	/**
	 * Adds a search clause to a match.
	 * @param search the search clause
	 * @return the ongoing reading
	 */
	StatementBuilder.OngoingReadingWithoutWhereWithSearch search(Search search);

}
