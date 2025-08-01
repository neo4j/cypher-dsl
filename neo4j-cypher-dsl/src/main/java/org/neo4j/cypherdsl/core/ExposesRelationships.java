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

import static org.apiguardian.api.API.Status.STABLE;

import org.apiguardian.api.API;

/**
 * A marker interface for things that expose methods to create new relationships to other elements.
 *
 * @author Michael J. Simons
 * @param <T> The type of the resulting {@link RelationshipPattern}.
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface ExposesRelationships<T extends RelationshipPattern & ExposesPatternLengthAccessors<?>> {

	/**
	 * Starts building an outgoing relationship to the {@code other} {@link Node node}.
	 *
	 * @param other The other end of the outgoing relationship
	 * @param types The types to match
	 * @return An ongoing relationship definition, that can be used to specify the type
	 */
	T relationshipTo(Node other, String... types);

	/**
	 * Starts building an incoming relationship starting at the {@code other} {@link Node node}.
	 *
	 * @param other The source of the incoming relationship
	 * @param types The types to match
	 * @return An ongoing relationship definition, that can be used to specify the type
	 */
	T relationshipFrom(Node other, String... types);

	/**
	 * Starts building an undirected relationship between this {@link Node node} and the {@code other}.
	 *
	 * @param other The other end of the relationship
	 * @param types The types to match
	 * @return An ongoing relationship definition, that can be used to specify the type
	 */
	T relationshipBetween(Node other, String... types);

	/**
	 * A convenience method for creating relationships between nodes avoiding going through the fluent API by allowing
	 * to pass in the type directly.
	 *
	 * @param other The other end of the relationship
	 * @param direction The direction of the relationship, seen from {@code this} node
	 * @param types The type of the relationship to create or the types to match
	 * @return An ongoing relationship definition, that can be used to specify details of the relationship
	 * @since 2023.5.0
	 */
	default T relationshipWith(Node other, Relationship.Direction direction, String... types) {
		return switch (direction) {
			case LTR -> this.relationshipTo(other, types);
			case RTL -> this.relationshipFrom(other, types);
			case UNI -> this.relationshipBetween(other, types);
		};
	}
}
