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

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

class LabelsTests {

	static final Labels SIMPLE_OR = Labels.exactly("Person").or(Labels.exactly("Organization"));
	static final Labels OR_AND_NOT = SIMPLE_OR.and(Labels.exactly("Sanctioned").negate());

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

	@Test
	void shouldMatchLabelsDynamically() {
		var labels = Cypher.listOf(Cypher.literalOf("Person"), Cypher.literalOf("Director")).as("labels");
		var directors = Cypher.node(Cypher.allLabels(labels)).named("directors");
		var stmt = Cypher.with(labels).match(directors).returning(directors).build();
		assertThat(stmt.getCypher())
			.isEqualTo("WITH ['Person', 'Director'] AS labels MATCH (directors:$(labels)) RETURN directors");
	}

	@Test
	void shouldMatchNodesDynamicallyUsingAny() {
		var labels = Cypher.listOf(Cypher.literalOf("Movie"), Cypher.literalOf("Actor"));
		var n = Cypher.node(Cypher.anyLabel(labels)).named("n");
		var stmt = Cypher.match(n).returning(n.as("nodes")).build();
		assertThat(stmt.getCypher()).isEqualTo("MATCH (n:$any(['Movie', 'Actor'])) RETURN n AS nodes");
	}

	@Test
	void combinationWithLabelExpressionsShouldWork() {

		var labels = Cypher.anyLabel(Cypher.listOf(Cypher.literalOf("Movie"), Cypher.literalOf("Actor")))
			.and(Cypher.exactlyLabel("Foo"))
			.or(Cypher.exactlyLabel("Bar").and(Cypher.allLabels(Cypher.parameter("IWantToDie"))));
		var n = Cypher.node(labels).named("n");
		var stmt = Cypher.match(n).returning(n.as("nodes")).build();
		assertThat(stmt.getCypher())
			.isEqualTo("MATCH (n:$any(['Movie', 'Actor'])&`Foo`|`Bar`&$($IWantToDie)) RETURN n AS nodes");
	}

	@Test
	void dynamicConjunctionsShouldWork() {

		var labels = Cypher.allLabels(Cypher.parameter("a")).or(Cypher.anyLabel(Cypher.parameter("b")));
		var n = Cypher.node(labels).named("n");
		var stmt = Cypher.match(n).returning(n.as("nodes")).build();
		assertThat(stmt.getCypher()).isEqualTo("MATCH (n:$($a)|$any($b)) RETURN n AS nodes");
	}

	@Test
	void dynamicSetWrongType1() {
		var labels = Cypher.allLabels(Cypher.parameter("a")).or(Cypher.anyLabel(Cypher.parameter("b")));
		var n = Cypher.node("Whatever").named("n");
		var match = Cypher.match(n);
		assertThatIllegalArgumentException().isThrownBy(() -> match.set(n, labels))
			.withMessage(
					"Only a single dynamic label expression or a set of static labels might be used in an updating clause");
	}

	@Test
	void dynamicSetWrongSelector() {
		var labels = Cypher.anyLabel(Cypher.parameter("a"));
		var n = Cypher.node("Whatever").named("n");
		var match = Cypher.match(n);
		assertThatIllegalArgumentException().isThrownBy(() -> match.set(n, labels))
			.withMessage(
					"Only a single dynamic label expression or a set of static labels might be used in an updating clause");
	}

	@Test
	void dynamicSetAll() {
		var labels = Cypher.allLabels(Cypher.parameter("a"));
		var n = Cypher.node("Whatever").named("n");
		var stmt = Cypher.match(n).set(n, labels).build();
		assertThat(stmt.getCypher()).isEqualTo("MATCH (n:`Whatever`) SET n:$($a)");
	}

	@Test
	void dynamicSetAllColon() {
		var labels = Cypher.allLabels(Cypher.parameter("a")).conjunctionWith(Cypher.allLabels(Cypher.parameter("b")));
		var n = Cypher.node("Whatever").named("n");
		var stmt = Cypher.match(n).set(n, labels).build();
		assertThat(stmt.getCypher()).isEqualTo("MATCH (n:`Whatever`) SET n:$($a):$($b)");
	}

	@Test
	void dynamicSetAllColonMixed() {
		var labels = Cypher.allLabels(Cypher.parameter("a")).conjunctionWith(Cypher.allLabels(Cypher.parameter("b"))).conjunctionWith(Labels.exactly("OhBuggerOff"));
		var n = Cypher.node("Whatever").named("n");
		var stmt = Cypher.match(n).set(n, labels).build();
		assertThat(stmt.getCypher()).isEqualTo("MATCH (n:`Whatever`) SET n:$($a):$($b):`OhBuggerOff`");
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

			var movieOrFilm = Labels.exactly("Movie").or(Labels.exactly("Film"));

			String statement;
			Node a = Cypher.node("Person").withProperties("name", Cypher.literalOf("Keanu Reeves")).named("a");
			Node b = Cypher.anyNode("b");

			statement = Cypher.match(a)
				.returning(Cypher.listBasedOn(a.relationshipBetween(b))
					.where(b.hasLabels(movieOrFilm).and(b.property("released").isNotNull()))
					.returning(b.property("released"))
					.as("years"))
				.build()
				.getCypher();
			assertThat(statement).isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE (b:`Movie`|`Film` AND b.released IS NOT NULL) | b.released] AS years");

			statement = Cypher.match(a)
				.returning(Cypher.listBasedOn(a.relationshipBetween(b))
					.where(b.hasLabels(movieOrFilm)
						.and(b.property("released").isNotNull())
						.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix")))
						.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix 2"))))
					.returning(b.property("released"))
					.as("years"))
				.build()
				.getCypher();
			assertThat(statement).isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE ((b:`Movie`|`Film` AND b.released IS NOT NULL) OR b.title = 'The Matrix' OR b.title = 'The Matrix 2') | b.released] AS years");

			statement = Cypher.match(a)
				.returning(Cypher.listBasedOn(a.relationshipBetween(b))
					.where(b.hasLabels(movieOrFilm))
					.and(b.property("released").isNotNull())
					.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix")))
					.or(b.property("title").isEqualTo(Cypher.literalOf("The Matrix 2")))
					.returning(b.property("released"))
					.as("years"))
				.build()
				.getCypher();

			assertThat(statement).isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE ((b:`Movie`|`Film` AND b.released IS NOT NULL) OR b.title = 'The Matrix' OR b.title = 'The Matrix 2') | b.released] AS years");

			statement = Cypher.match(a)
				.returning(Cypher.listBasedOn(a.relationshipBetween(b))
					.where(b.hasLabels(movieOrFilm))
					.returning(b.property("released"))
					.as("years"))
				.build()
				.getCypher();
			assertThat(statement).isEqualTo(
					"MATCH (a:`Person` {name: 'Keanu Reeves'}) RETURN [(a)--(b) WHERE b:`Movie`|`Film` | b.released] AS years");
		}

	}

}
