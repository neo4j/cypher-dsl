/*
 * Copyright (c) 2019-2026 "Neo4j,"
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
import java.util.stream.Stream;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.Parameter;
import org.junit.jupiter.params.ParameterizedClass;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import static org.assertj.core.api.Assertions.assertThat;

class SearchTests {

	static Stream<org.junit.jupiter.params.provider.Arguments> basicVectorSearchClause() {
		return Stream.of(
				org.junit.jupiter.params.provider.Arguments.of(Cypher.vector(new long[] { 1, 2, 3 }),
						"vector([1, 2, 3], 3, INTEGER NOT NULL)"),
				org.junit.jupiter.params.provider.Arguments.of(Cypher.literalOf(List.of(1, 2, 3)), "[1, 2, 3]"),
				org.junit.jupiter.params.provider.Arguments
					.of(Cypher.listOf(Cypher.literalOf(1), Cypher.literalOf(2), Cypher.literalOf(3)), "[1, 2, 3]"),
				org.junit.jupiter.params.provider.Arguments.of(Cypher.parameter("snowWhiteEmbedding"),
						"$snowWhiteEmbedding"),
				org.junit.jupiter.params.provider.Arguments.of(Cypher.name("snowWhite").property("embedding"),
						"snowWhite.embedding"),
				Arguments.of(Cypher.node("Movie", Cypher.mapOf("title", Cypher.literalOf("Snow White")))
					.named("snowWhite")
					.property("embedding"), "snowWhite.embedding"));
	}

	@ParameterizedTest
	@MethodSource
	void basicVectorSearchClause(Expression vector, String vectorAsString) {

		var movie = Cypher.name("movie");
		var topK = 4;
		var search = Cypher.search(movie).in("moviePlots").forVector(vector).limit(topK).build();
		assertThat(search).hasToString("Search{cypher=SEARCH movie IN (VECTOR INDEX moviePlots FOR %s LIMIT 4)}",
				vectorAsString);
	}

	@Test
	void basicVectorSearchClauseWithScore() {

		var movie = Cypher.name("movie");
		var vector = Cypher.vector(new long[] { 1, 2, 3 });
		var topK = 4;
		var search = Cypher.search(movie)
			.in("moviePlots")
			.forVector(vector)
			.limit(topK)
			.score(Cypher.name("similarityScore"))
			.build();
		assertThat(search).hasToString(
				"Search{cypher=SEARCH movie IN (VECTOR INDEX moviePlots FOR vector([1, 2, 3], 3, INTEGER NOT NULL) LIMIT 4) SCORE AS similarityScore}");
	}

	@Test
	void vectorSearchWithFilters1() {
		var movie = Cypher.name("movie");
		var vector = Cypher.vector(new long[] { 1, 2, 3 });
		var topK = 4;
		var search = Cypher.search(movie)
			.in("moviePlots")
			.forVector(vector)
			.where(movie.property("releaseDate").gt(Cypher.date("1990")))
			.limit(topK)
			.score(Cypher.name("similarityScore"))
			.build();
		assertThat(search).hasToString(
				"Search{cypher=SEARCH movie IN (VECTOR INDEX moviePlots FOR vector([1, 2, 3], 3, INTEGER NOT NULL) WHERE movie.releaseDate > date('1990') LIMIT 4) SCORE AS similarityScore}");
	}

	@Test
	void vectorSearchWithFilters2() {
		var movie = Cypher.name("movie");
		var vector = Cypher.vector(new long[] { 1, 2, 3 });
		var topK = 4;
		var search = Cypher.search(movie)
			.in("moviePlots")
			.forVector(vector)
			.where(movie.property("releaseDate").gt(Cypher.date("1990")),
					movie.property("title").startsWith(Cypher.literalOf("A")))
			.limit(topK)
			.score(Cypher.name("similarityScore"))
			.build();
		assertThat(search).hasToString(
				"Search{cypher=SEARCH movie IN (VECTOR INDEX moviePlots FOR vector([1, 2, 3], 3, INTEGER NOT NULL) WHERE (movie.releaseDate > date('1990') AND movie.title STARTS WITH 'A') LIMIT 4) SCORE AS similarityScore}");
	}

	@Nested
	@ParameterizedClass
	@ValueSource(booleans = { true, false })
	class FullExamples {

		@Parameter
		boolean prettyPrint;

		@Test
		void findTheMostSimilarNodesWithScore() {
			var movie = Cypher.name("movie");
			var similarityScore = Cypher.name("similarityScore");

			var search = Cypher.search(movie)
				.in("moviePlots")
				.forVector(Cypher.vector(new long[] { 1, 2, 3 }))
				.limit(4)
				.score(similarityScore)
				.build();

			var stmt = Cypher.match(Cypher.node("Movie").named(movie))
				.search(search)
				.returning(movie.property("title").as("title"), similarityScore)
				.build();

			if (!this.prettyPrint) {
				assertThat(stmt.getCypher()).isEqualTo(
						"MATCH (movie:`Movie`) SEARCH movie IN (VECTOR INDEX moviePlots FOR vector([1, 2, 3], 3, INTEGER NOT NULL) LIMIT 4) SCORE AS similarityScore RETURN movie.title AS title, similarityScore");
			}
			else {
				var expected = """
						MATCH (movie:Movie)
						  SEARCH movie IN (
						    VECTOR INDEX moviePlots
						    FOR vector([1, 2, 3], 3, INTEGER NOT NULL)
						    LIMIT 4
						  ) SCORE AS similarityScore
						RETURN movie.title AS title, similarityScore""";
				assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(stmt)).isEqualTo(expected);
			}
		}

		@Test
		void vectorSearchWithFilters() {
			var movie = Cypher.name("movie");

			var search = Cypher.search(movie)
				.in("moviePlots")
				.forVector(Cypher.vector(new long[] { 1, 2, 3 }))
				.where(movie.property("releaseDate").gt(Cypher.date("1990")))
				.limit(4)
				.build();

			var stmt = Cypher.match(Cypher.node("Movie").named(movie))
				.search(search)

				.returning(movie.property("title").as("title"),
						movie.property("releaseDate").property("year").as("year"))
				.build();

			if (this.prettyPrint) {
				assertThat(stmt.getCypher()).isEqualTo(
						"MATCH (movie:`Movie`) SEARCH movie IN (VECTOR INDEX moviePlots FOR vector([1, 2, 3], 3, INTEGER NOT NULL) WHERE movie.releaseDate > date('1990') LIMIT 4) RETURN movie.title AS title, movie.releaseDate.year AS year");
			}
			else {
				var expected = """
						MATCH (movie:Movie)
						  SEARCH movie IN (
						    VECTOR INDEX moviePlots
						    FOR vector([1, 2, 3], 3, INTEGER NOT NULL)
						    WHERE movie.releaseDate > date('1990')
						    LIMIT 4
						  )
						RETURN movie.title AS title, movie.releaseDate.year AS year""";
				assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(stmt)).isEqualTo(expected);
			}

		}

		@Test
		void vectorSearchWithOuterFilters() {
			var movie = Cypher.name("movie");

			var search = Cypher.search(movie)
				.in("moviePlots")
				.forVector(Cypher.vector(new long[] { 1, 2, 3 }))
				.limit(4)
				.build();

			var stmt = Cypher.match(Cypher.node("Movie").named(movie))
				.search(search)
				.where(movie.property("releaseDate").gt(Cypher.date("1990")))
				.returning(movie.property("title").as("title"),
						movie.property("releaseDate").property("year").as("year"))
				.build();

			if (this.prettyPrint) {
				assertThat(stmt.getCypher()).isEqualTo(
						"MATCH (movie:`Movie`) SEARCH movie IN (VECTOR INDEX moviePlots FOR vector([1, 2, 3], 3, INTEGER NOT NULL) LIMIT 4) WHERE movie.releaseDate > date('1990') RETURN movie.title AS title, movie.releaseDate.year AS year");
			}
			else {
				var expected = """
						MATCH (movie:Movie)
						  SEARCH movie IN (
						    VECTOR INDEX moviePlots
						    FOR vector([1, 2, 3], 3, INTEGER NOT NULL)
						    LIMIT 4
						  )
						WHERE movie.releaseDate > date('1990')
						RETURN movie.title AS title, movie.releaseDate.year AS year""";
				assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(stmt)).isEqualTo(expected);
			}

		}

		@Test
		void vectorSearchWithFiltersAndWhereClause() {
			var movie = Cypher.name("movie");

			var search = Cypher.search(movie)
				.in("moviePlots")
				.forVector(Cypher.vector(new long[] { 1, 2, 3 }))
				.where(movie.property("releaseDate").gt(Cypher.date("1990")))
				.limit(4)
				.build();

			var stmt = Cypher.match(Cypher.node("Movie").named(movie))
				.search(search)
				.where(movie.property("rating").gt(Cypher.literalOf(7.5)))
				.returning(movie.property("title").as("title"),
						movie.property("releaseDate").property("year").as("year"))
				.build();

			if (this.prettyPrint) {
				assertThat(stmt.getCypher()).isEqualTo(
						"MATCH (movie:`Movie`) SEARCH movie IN (VECTOR INDEX moviePlots FOR vector([1, 2, 3], 3, INTEGER NOT NULL) WHERE movie.releaseDate > date('1990') LIMIT 4) WHERE movie.rating > 7.5 RETURN movie.title AS title, movie.releaseDate.year AS year");
			}
			else {
				var expected = """
						MATCH (movie:Movie)
						  SEARCH movie IN (
						    VECTOR INDEX moviePlots
						    FOR vector([1, 2, 3], 3, INTEGER NOT NULL)
						    WHERE movie.releaseDate > date('1990')
						    LIMIT 4
						  )
						WHERE movie.rating > 7.5
						RETURN movie.title AS title, movie.releaseDate.year AS year""";
				assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(stmt)).isEqualTo(expected);
			}
		}

		@Test
		void vectorSearchWithFilterWithAnANDPredicateOnseparateProperties() {

			var snowWhite = Cypher.node("Movie", Cypher.mapOf("title", Cypher.literalOf("Snow White")))
				.named("snowWhite");
			var movie = Cypher.name("movie");

			var search = Cypher.search(movie)
				.in("moviePlots")
				.forVector(snowWhite.property("embedding"))
				.where(movie.property("releaseDate").lt(Cypher.date("2000")),
						movie.property("rating").gte(snowWhite.property("rating")))
				.limit(4)
				.build();

			var stmt = Cypher.match(snowWhite)
				.match(Cypher.node("Movie").named(movie))
				.search(search)
				.returning(movie.property("title").as("title"),
						movie.property("releaseDate").property("year").as("year"))
				.build();

			if (this.prettyPrint) {
				assertThat(stmt.getCypher()).isEqualTo(
						"MATCH (snowWhite:`Movie` {title: 'Snow White'}) MATCH (movie:`Movie`) SEARCH movie IN (VECTOR INDEX moviePlots FOR snowWhite.embedding WHERE (movie.releaseDate < date('2000') AND movie.rating >= snowWhite.rating) LIMIT 4) RETURN movie.title AS title, movie.releaseDate.year AS year");
			}
			else {
				var expected = """
						MATCH (snowWhite:Movie {
						  title: 'Snow White'
						})
						MATCH (movie:Movie)
						  SEARCH movie IN (
						    VECTOR INDEX moviePlots
						    FOR snowWhite.embedding
						    WHERE (movie.releaseDate < date('2000')
						      AND movie.rating >= snowWhite.rating)
						    LIMIT 4
						  )
						RETURN movie.title AS title, movie.releaseDate.year AS year""";
				assertThat(Renderer.getRenderer(Configuration.prettyPrinting()).render(stmt)).isEqualTo(expected);
			}
		}

	}

}
