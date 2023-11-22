/*
 * Copyright (c) 2019-2023 "Neo4j,"
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
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.annotations.CheckReturnValue;

/**
 * A shared, public interface for  {@link Relationship relationships} and {@link RelationshipChain chains of relationships}.
 * This interface reassembles the <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/RelationshipPattern.html">RelationshipPattern</a>.
 * <p>
 * This interface can be used synonymous with the concept of a <a href="https://neo4j.com/docs/cypher-manual/4.0/clauses/where/#query-where-patterns">Path Pattern</a>.
 *
 * @author Michael J. Simons
 * @soundtrack Mine &amp; Fatoni - Alle Liebe nachträglich
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface RelationshipPattern extends PatternElement, ExposesRelationships<RelationshipChain> {

	/**
	 * Turns the pattern into a named chain of relationships.
	 *
	 * @param name The name to be used.
	 * @return A named relationship that can be chained with more relationship definitions.
	 */
	@NotNull @CheckReturnValue
	ExposesRelationships<RelationshipChain> named(String name);

	/**
	 * Turns the pattern into a named chain of relationships.
	 *
	 * @param name The name to be used.
	 * @return A named relationship that can be chained with more relationship definitions.
	 */
	@NotNull @CheckReturnValue
	ExposesRelationships<RelationshipChain> named(SymbolicName name);

	/**
	 * Transform this pattern into a condition. All names of the patterns must be known upfront in the final statement,
	 * as PatternExpressions are not allowed to introduce new variables.
	 *
	 * @return A condition based on this pattern.
	 * @since 2021.0.0
	 */
	@NotNull @Contract(pure = true)
	Condition asCondition();

	/**
	 * Quantifies the relationship.
	 *
	 * @param quantifier the quantifier to use
	 * @return a quantified relationship
	 * @since 2023.9.0
	 */
	@NotNull @Contract(pure = true)
	default PatternElement quantifyRelationship(@Nullable QuantifiedPathPattern.Quantifier quantifier) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Quantifies the pattern.
	 *
	 * @param quantifier the quantifier to use
	 * @return a quantified path pattern
	 * @since 2023.9.0
	 */
	@NotNull @Contract(pure = true)
	default PatternElement quantify(@Nullable QuantifiedPathPattern.Quantifier quantifier) {
		throw new UnsupportedOperationException();
	}
}
