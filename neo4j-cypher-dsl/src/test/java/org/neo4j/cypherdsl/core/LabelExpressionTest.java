/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class LabelExpressionTest {

	static final LabelExpression SIMPLE_OR = new LabelExpression("Person").or(new LabelExpression("Organization"));
	static final LabelExpression OR_AND_NOT = SIMPLE_OR.and(new LabelExpression("Sanctioned").negate());

	@Test // GH-1077
	void labelExpressionsShouldWork1() {

		var node = Cypher.node(SIMPLE_OR).named("n");
		var cypher = Cypher.match(node).returning(node).build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (n:`Person`|`Organization`) RETURN n");
	}

	@Test // GH-1077
	void labelExpressionsShouldWork2() {

		var node = Cypher.node(OR_AND_NOT).named("n");
		var cypher = Cypher.match(node).returning(node).build().getCypher();

		assertThat(cypher).isEqualTo("MATCH (n:(`Person`|`Organization`)&!`Sanctioned`) RETURN n");
	}

	@Nested
	class AsConditions {

		@Test // GH-1077
		void labelExpressionsShouldWork1() {

			var node = Cypher.anyNode("n");
			var cypher = Cypher.match(node).where(node.hasLabels(SIMPLE_OR)).returning(node).build().getCypher();

			assertThat(cypher).isEqualTo("MATCH (n) WHERE n:`Person`|`Organization` RETURN n");
		}

		@Test // GH-1077
		void labelExpressionsShouldWork2() {

			var node = Cypher.anyNode("n");
			var cypher = Cypher.match(node).where(node.hasLabels(OR_AND_NOT)).returning(node).build().getCypher();

			assertThat(cypher).isEqualTo("MATCH (n) WHERE n:(`Person`|`Organization`)&!`Sanctioned` RETURN n");
		}

		@Test // GH-1141
		void labelExpressionsInPredicates() {

			var movieOrFilm = new LabelExpression("Movie").or(new LabelExpression("Film"));

			String statement;
			Node a = Cypher.node("Person").withProperties("name", Cypher.literalOf("Keanu Reeves")).named("a");
			Node b = Cypher.anyNode("b");

			statement = Cypher.match(a)
				.returning(
					Cypher.listBasedOn(a.relationshipBetween(b))
						.where(b.hasLabels(movieOrFilm).and(b.property("released").isNotNull()))
						.returning(b.property("released"))
						.as("years"))
				.build().getCypher();
			assertThat(statement)
				.isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE (b:`Movie`|`Film` AND b.released IS NOT NULL) | b.released] AS years");

			statement = Cypher.match(a)
				.returning(
					Cypher.listBasedOn(a.relationshipBetween(b))
						.where(
							b.hasLabels(movieOrFilm)
								.and(b.property("released").isNotNull())
								.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix")))
								.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix 2"))))
						.returning(b.property("released"))
						.as("years"))
				.build().getCypher();
			assertThat(statement)
				.isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE ((b:`Movie`|`Film` AND b.released IS NOT NULL) OR b.title = 'The Matrix' OR b.title = 'The Matrix 2') | b.released] AS years");

			statement = Cypher.match(a)
				.returning(
					Cypher.listBasedOn(a.relationshipBetween(b))
						.where(b.hasLabels(movieOrFilm))
						.and(b.property("released").isNotNull())
						.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix")))
						.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix 2")))
						.returning(b.property("released"))
						.as("years"))
				.build().getCypher();

			assertThat(statement)
				.isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE ((b:`Movie`|`Film` AND b.released IS NOT NULL) OR b.title = 'The Matrix' OR b.title = 'The Matrix 2') | b.released] AS years");

			statement = Cypher.match(a)
				.returning(
					Cypher.listBasedOn(a.relationshipBetween(b))
						.where(b.hasLabels(movieOrFilm))
						.returning(b.property("released"))
						.as("years"))
				.build().getCypher();
			assertThat(statement)
				.isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE b:`Movie`|`Film` | b.released] AS years");
		}
	}
}
