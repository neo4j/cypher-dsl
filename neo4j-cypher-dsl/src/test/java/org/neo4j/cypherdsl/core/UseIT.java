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

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.StatementBuilder.OngoingReadingAndReturn;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

/**
 * @author Michael J. Simons
 */
class UseIT {

	private final Renderer renderer = Renderer
		.getRenderer(Configuration.newConfig().withDialect(Dialect.NEO4J_5).build());

	private final OngoingReadingAndReturn ongoingInnerStatementDefinition = Cypher
		.call(Cypher.match(Cypher.node("Movie").named("movie")).returning("movie").build())
		.returning("movie");

	static Stream<Arguments> graphByNameShouldWork() {
		return Stream.of(
				Arguments.of(Cypher.literalOf("myComposite.myConstituent"),
						"USE graph.byName('myComposite.myConstituent') MATCH (n) RETURN n"),
				Arguments.of(Cypher.parameter("graphName"), "USE graph.byName($graphName) MATCH (n) RETURN n"));
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
			       myDatabase, USE myDatabase MATCH (n) RETURN n
			       myComposite.myConstituent, USE myComposite.myConstituent MATCH (n) RETURN n
			""")
	void simpleUseShouldWork(String target, String expected) {

		var statement = Cypher.match(Cypher.anyNode("n")).returning("n").build();
		var cypher = Cypher.use(target, statement).getCypher();
		assertThat(cypher).isEqualTo(expected);
	}

	@ParameterizedTest
	@MethodSource
	void graphByNameShouldWork(Expression target, String expected) {

		var statement = Cypher.match(Cypher.anyNode("n")).returning("n").build();
		var cypher = "";
		if (target instanceof StringLiteral literal) {
			cypher = Cypher.use(literal, statement).getCypher();
		}
		else if (target instanceof Parameter<?> parameter) {
			cypher = Cypher.use(parameter, statement).getCypher();
		}
		else {
			Assertions.fail("Unexpected expression type in test.");
		}
		assertThat(cypher).isEqualTo(expected);
	}

	@Test
	void usageInSubqueryShouldWork() {

		var expected = """
				UNWIND ['cineasts.latest', 'cineasts.upcoming'] AS graphName
				CALL {
				USE graph.byName(graphName)
				  MATCH (movie:`Movie`)
				  RETURN movie
				}
				RETURN movie.title AS title
				""".lines()
			.map(String::trim)
			.collect(Collectors.joining(" "))
			.replace("CALL { ", "CALL {")
			.replace(" }", "}");

		var innerStatement = Cypher.match(Cypher.node("Movie").named("movie")).returning("movie").build();
		var cypher = Cypher.unwind(Cypher.literalOf(List.of("cineasts.latest", "cineasts.upcoming")))
			.as("graphName")
			.call(Cypher.use((Expression) Cypher.name("graphName"), innerStatement))
			.returning(Cypher.name("movie").property("title").as("title"))
			.build()
			.getCypher();
		assertThat(cypher).isEqualTo(expected);
	}

	@Test
	void useBeforeCall() {

		var innerStatement = this.ongoingInnerStatementDefinition.build();
		var cypher = Cypher.use("cineasts.latest", innerStatement).getCypher();
		assertThat(cypher).isEqualTo("USE cineasts.latest CALL {MATCH (movie:`Movie`) RETURN movie} RETURN movie");
	}

	@Test
	void nestedUseShouldThrow() {

		var innerStatement = Cypher.use("x", this.ongoingInnerStatementDefinition.build());
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.use("y", innerStatement))
			.withMessage("Nested USE clauses are not supported");
	}

	@Test
	void useBeforeCallWithCallInTx() {

		var title = Cypher.name("title");
		var movie = Cypher.node("Movie", Cypher.mapOf("title", title)).named("m");
		var innerStatement = Cypher.unwind(Cypher.parameter("newMovies"))
			.as(title)
			.call(Cypher.with(title).merge(movie).returning(movie.elementId().as("id")).build())
			.returning("id")
			.build();
		var cypher = this.renderer.render(Cypher.use("cineasts.latest", innerStatement));
		assertThat(cypher).isEqualTo(
				"USE cineasts.latest UNWIND $newMovies AS title CALL {WITH title MERGE (m:`Movie` {title: title}) RETURN elementId(m) AS id} RETURN id");
	}

	@Test
	void addExplain() {

		var innerStatement = this.ongoingInnerStatementDefinition.build();
		var cypher = Cypher.use("cineasts.latest", innerStatement).explain().getCypher();
		assertThat(cypher)
			.isEqualTo("EXPLAIN USE cineasts.latest CALL {MATCH (movie:`Movie`) RETURN movie} RETURN movie");
	}

	@Test
	void innerProfileShouldThrow() {

		var innerStatement = this.ongoingInnerStatementDefinition.profile();
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.use("x", innerStatement))
			.withMessage("PROFILE'd statements are not supported inside USE clauses");
	}

	@Test
	void innerExplainShouldThrow() {

		var innerStatement = this.ongoingInnerStatementDefinition.explain();
		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.use("x", innerStatement))
			.withMessage("EXPLAIN'ed statements are not supported inside USE clauses");
	}

}
