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

import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;
import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;

/**
 * A step exposing logical operators {@code and} and {@code or} after a {@code where}
 * clause.
 *
 * @param <T> the type being returned after the new condition has been chained
 * @author Michael J. Simons
 * @since 1.1
 */
public interface ExposesLogicalOperators<T> {

	/**
	 * Adds a condition to the existing conditions, connected by an {@code AND}. Existing
	 * conditions will be logically grouped by using {@code ()} in the statement if
	 * previous conditions used another logical operator.
	 * @param condition an additional condition
	 * @return the ongoing definition of a match
	 */
	@CheckReturnValue
	T and(Condition condition);

	/**
	 * Adds a condition based on a path pattern to the existing conditions, connected by
	 * an {@code AND}. Existing conditions will be logically grouped by using {@code ()}
	 * in the statement if previous conditions used another logical operator.
	 * @param pathPattern an additional pattern to include in the conditions
	 * @return the ongoing definition of a match
	 */
	@CheckReturnValue
	default T and(RelationshipPattern pathPattern) {
		return this.and(RelationshipPatternCondition.of(pathPattern));
	}

	/**
	 * Adds a condition to the existing conditions, connected by an {@code OR}. Existing
	 * conditions will be logically grouped by using {@code ()} in the statement if
	 * previous conditions used another logical operator.
	 * @param condition an additional condition
	 * @return the ongoing definition of a match
	 */
	@CheckReturnValue
	T or(Condition condition);

	/**
	 * Adds a condition based on a path pattern to the existing conditions, connected by
	 * an {@code OR}. Existing conditions will be logically grouped by using {@code ()} in
	 * the statement if previous conditions used another logical operator.
	 * @param pathPattern an additional pattern to include in the conditions
	 * @return the ongoing definition of a match
	 */
	@CheckReturnValue
	default T or(RelationshipPattern pathPattern) {
		return this.or(RelationshipPatternCondition.of(pathPattern));
	}

}
