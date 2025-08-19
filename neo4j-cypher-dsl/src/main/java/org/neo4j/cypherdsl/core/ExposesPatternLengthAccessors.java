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

import static org.apiguardian.api.API.Status.STABLE;

/**
 * This interface is used to derive new relationship patterns from existing
 * {@link Relationship relationships} or {@link RelationshipChain chains of relationships}
 * with new lengths (min, max or unbounded) configured.
 *
 * @param <T> the type of the patterns whose lengths can be adjusted.
 * @author Michael J. Simons
 * @since 2021.2.3
 */
@API(status = STABLE, since = "2021.2.3")
public interface ExposesPatternLengthAccessors<T extends RelationshipPattern> {

	/**
	 * Creates a new relationship pattern with an unbound length minimum length.
	 * @return the new relationship
	 * @since 1.1.1
	 */
	T unbounded();

	/**
	 * Creates a new relationship pattern with a new minimum length.
	 * @param minimum the new minimum
	 * @return the new relationship
	 */
	T min(Integer minimum);

	/**
	 * Creates a new relationship pattern with a new maximum length.
	 * @param maximum the new maximum
	 * @return the new relationship
	 */
	T max(Integer maximum);

	/**
	 * Creates a new relationship pattern with a new length.
	 * @param minimum the new minimum
	 * @param maximum the new maximum
	 * @return the new relationship
	 */
	T length(Integer minimum, Integer maximum);

}
