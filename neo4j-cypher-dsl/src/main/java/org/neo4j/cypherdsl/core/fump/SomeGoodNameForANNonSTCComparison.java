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

import java.util.Map;
import java.util.Set;

import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Operator;
import org.neo4j.cypherdsl.core.Statement;

/**
 * This type encapsulates a comparison in which a property of a node or relationship was used. The property may appear
 * on the left hand side or right hand side or even on both side of the comparison (think being used in a function on
 * both sides with different arguments). The {@code clause} attribute will specify the context in which the comparison
 * was made.
 * <p>
 * The expressions used in the comparison are provided as Cypher-DSL AST expressions. They can be freely visited or rendered
 * into Cypher via the {@link org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer} like this:
 * <pre>{@code
 *     var cypher = Renderer.getRenderer(Configuration.prettyPrinting(), GeneralizedRenderer.class)
 *          .render(comparison.left());
 * }</pre>
 *
 * @author Michael J. Simons
 * @param clause         The clause in which the comparison was used
 * @param property       The property used in this condition
 * @param left           The left hand side of the comparison
 * @param operator       The operator used
 * @param right          The right hand side of the comparison
 * @param parameterNames The parameter names used in this comparison (analoge to {@link Statement#getParameterNames()}
 * @param parameters     Parameters with defined values used in this comparison (analoge to {@link Statement#getParameters()}
 * @soundtrack Die Ärzte - Le Frisur
 * @since TBA
 */
public record SomeGoodNameForANNonSTCComparison(
	Clause clause, Property property, Expression left, Operator operator, Expression right, Set<String> parameterNames,
	Map<String, Object> parameters
) {

	public SomeGoodNameForANNonSTCComparison {
		parameterNames = Set.copyOf(parameterNames);
		parameters = Map.copyOf(parameters);
	}

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
