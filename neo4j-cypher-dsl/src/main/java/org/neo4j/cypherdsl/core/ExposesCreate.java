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

import java.util.Collection;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A step exposing a {@link #create(PatternElement...)} method.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface ExposesCreate {

	/**
	 * Starts building a {@code CREATE} clause.
	 * @param pattern patterns to create
	 * @return an ongoing merge
	 * @see Cypher#create(PatternElement...)
	 */
	@CheckReturnValue
	StatementBuilder.OngoingUpdate create(PatternElement... pattern);

	/**
	 * Starts building a {@code CREATE} clause.
	 * @param pattern patterns to create
	 * @return an ongoing merge
	 * @since 2021.2.2
	 * @see Cypher#create(Collection)
	 */
	@CheckReturnValue
	StatementBuilder.OngoingUpdate create(Collection<? extends PatternElement> pattern);

}
