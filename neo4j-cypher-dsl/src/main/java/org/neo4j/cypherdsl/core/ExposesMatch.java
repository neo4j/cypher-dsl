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

import java.util.Collection;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A step exposing a {@link #match(PatternElement...)} method. This is one of the main
 * entry points of most Cypher queries.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface ExposesMatch {

	/**
	 * Adds (another) {@code MATCH} clause.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 */
	@CheckReturnValue
	default StatementBuilder.OngoingReadingWithoutWhere match(PatternElement... pattern) {
		return this.match(false, pattern);
	}

	/**
	 * Adds (another) {@code MATCH} clause.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 * @since 2021.2.2
	 */
	@CheckReturnValue
	default StatementBuilder.OngoingReadingWithoutWhere match(Collection<? extends PatternElement> pattern) {
		return this.match(pattern.toArray(new PatternElement[] {}));
	}

	/**
	 * Adds (another) optional {@code MATCH} clause.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 */
	@CheckReturnValue
	default StatementBuilder.OngoingReadingWithoutWhere optionalMatch(PatternElement... pattern) {
		return this.match(true, pattern);
	}

	/**
	 * Adds (another) optional {@code MATCH} clause.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 * @since 2021.2.2
	 */
	@CheckReturnValue
	default StatementBuilder.OngoingReadingWithoutWhere optionalMatch(Collection<? extends PatternElement> pattern) {
		return this.optionalMatch(pattern.toArray(pattern.toArray(new PatternElement[] {})));
	}

	/**
	 * Adds (another) {@code MATCH} clause.
	 * @param optional a flag whether the {@code MATCH} clause includes the
	 * {@code OPTIONAL} keyword.
	 * @param pattern the patterns to match
	 * @return an ongoing match that is used to specify an optional where and a required
	 * return clause
	 * @since 2020.1.3
	 */
	@CheckReturnValue
	StatementBuilder.OngoingReadingWithoutWhere match(boolean optional, PatternElement... pattern);

}
