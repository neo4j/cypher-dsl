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
package org.neo4j.cypherdsl.core.fump;

import org.neo4j.cypherdsl.core.Operator;

/**
 * This type encapsulates a comparison in which a property of a node or relationship was used. The property may appear
 * on the left hand side or right hand side or even on both side of the comparison (think being used in a function on
 * both sides with different arguments). The {@code context} attribute will specify
 *
 * @author Michael J. Simons
 * @param clause   The clause in which the comparison was used
 * @param property The property used in this condition
 * @param left     The left hand side of the comparison represented as Cypher
 * @param operator The operator used
 * @param right    The right hand side of the comparison represented as Cypher
 * @soundtrack Die Ärzte - Le Frisur
 * @since TBA
 */
public record SomeGoodNameForANNonSTCComparison(
	Clause clause, Property property, String left, Operator operator, String right
) {

	/**
	 * Enum for the clause in which a comparison was made.
	 */
	public enum Clause {
		/**
		 * The comparison was used in a {@code MATCH} clause.
		 */
		MATCH,
		/**
		 * The comparison was used in a {@code CREATE} clause.
		 */
		CREATE,
		/**
		 * The comparison was used in a {@code MERGE} clause.
		 */
		MERGE,
		/**
		 * The comparison was used in a {@code DELETE} clause.
		 */
		DELETE,
		/**
		 * The comparison was used in a {@code WITH} clause. The {@code WITH} clause is different to the {@code WHERE}
		 * clause as here "WHERE simply filters the results."
		 * (quoting from <a href="https://neo4j.com/docs/cypher-manual/current/clauses/where/">where</a>).
		 */
		WITH,
		/**
		 * Used in case the analysis could not determine a clause.
		 */
		UNKNOWN
	}
}
