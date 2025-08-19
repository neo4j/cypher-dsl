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
package org.neo4j.cypherdsl.core.ast;

/**
 * Visitors are used to traverse the complete Cypher-DSL AST.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@FunctionalInterface
public interface Visitor {

	/**
	 * Enter a {@link Visitable}. Not all visitables will obey to the result
	 * @param segment the segment to visit.
	 */
	void enter(Visitable segment);

	/**
	 * A method that is used to pass control to some extent from the visitor to the
	 * {@link Visitable}. Not all visitables react to this, and we don't give any
	 * guarantees about which will. This method has been mainly introduced in parallel to
	 * {@link #enter(Visitable)} so that existing external implementations of
	 * {@link Visitor visitors} won't break.
	 * @param segment the segment to visit.
	 * @return a result indicating whether visitation of child elements should continue or
	 * not.
	 * @since 2022.3.0
	 */
	default EnterResult enterWithResult(Visitable segment) {
		enter(segment);
		return EnterResult.CONTINUE;
	}

	/**
	 * Leave a {@link Visitable}.
	 * @param segment the visited segment.
	 */
	default void leave(Visitable segment) {
	}

}
