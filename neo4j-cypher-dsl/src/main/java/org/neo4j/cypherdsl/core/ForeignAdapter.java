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
 * Represents an adapter that allows to turn foreign expressions into Cypher-DSL {@link Expression expressions}.
 *
 * @author Michael J. Simons
 * @param <FE> The type of the foreign expression.
 * @soundtrack Paul Kalkbrenner - Berlin Calling
 * @since 2021.1.0
 */
@API(status = STABLE, since = "2021.1.0")
public interface ForeignAdapter<FE> {

	/**
	 * Adapts a foreign expression into a Cypher-DSL {@link Condition}. The memoized expression should be something  that
	 * can be evaluated into something boolean.
	 *
	 * @return A condition
	 * @throws IllegalArgumentException if the expression doesn't resolve into something boolean
	 */
	Condition asCondition();

	/**
	 * Adapts a foreign expression into a Cypher-DSL {@link Expression}.
	 *
	 * @return A native expression
	 */
	Expression asExpression();

	/**
	 * Adapts a foreign expression into a Cypher-DSL {@link Node}, that allows to address it further down in queries.
	 *
	 * @return A node
	 * @throws IllegalArgumentException if the expression doesn't describe something that can be used to describe a node
	 */
	Node asNode();

	/**
	 * Adapts a foreign expression into a Cypher-DSL {@link Relationship}, that allows to address it further down in queries.
	 *
	 * @return A node
	 * @throws IllegalArgumentException if the expression doesn't describe something that can be used to describe a node
	 */
	Relationship asRelationship();

	/**
	 * Adapts a foreign expression into a Cypher-DSL {@link SymbolicName}. The memoized expression should ideally be something
	 * that is named or resolves to an alias.
	 *
	 * @return A symbolic name
	 * @throws IllegalArgumentException if a name cannot be derived from the expression.
	 */
	SymbolicName asName();
}
