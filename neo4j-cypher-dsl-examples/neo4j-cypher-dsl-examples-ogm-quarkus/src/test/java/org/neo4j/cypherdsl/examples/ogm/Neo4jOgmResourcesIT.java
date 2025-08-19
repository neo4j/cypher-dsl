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
package org.neo4j.cypherdsl.examples.ogm;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Objects;

import io.quarkus.test.junit.QuarkusTest;
import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import jakarta.inject.Inject;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.neo4j.driver.Driver;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
@QuarkusTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
public class Neo4jOgmResourcesIT {

	private static final int NUMBER_OF_INITIAL_MOVIES = 38;

	@Inject
	Driver driver;

	@BeforeAll
	public void createData() throws IOException {

		var movies = Files.readString(
				Path.of(Objects.requireNonNull(Neo4jOgmResourcesIT.class.getResource("/movies.cypher")).getPath()));

		try (var session = this.driver.session(); var tx = session.beginTransaction()) {
			tx.run("MATCH (n) DETACH DELETE n");
			tx.run(movies);
			tx.commit();
		}
	}

	@Test
	public void getMoviesShouldWork() {
		var response = RestAssured.given().when().get("/api/movies").then().statusCode(200).extract().response();

		var json = response.jsonPath();
		assertThat(json.<List<?>>getJsonObject("$").size()).isEqualTo(NUMBER_OF_INITIAL_MOVIES);
		var allTitles = json.<List<String>>getJsonObject("title");
		assertThat(allTitles.contains("Cloud Atlas")).isTrue();
	}

	@Test
	public void getMovieWithANativeTypeShouldWork() {
		var response = RestAssured.given()
			.when()
			.get("/api/movies/The Matrix")
			.then()
			.statusCode(200)
			.extract()
			.response();

		var json = response.jsonPath();
		assertThat(json.<String>get("watchedOn")).isNotNull();
	}

	@Test
	public void createPersonShouldWork() {

		var response = RestAssured.given()
			.body("{\"name\":\"Lieschen Müller\",\"born\":2020}")
			.contentType(ContentType.JSON)
			.when()
			.post("/api/people")
			.then()
			.statusCode(201)
			.extract()
			.response();

		var json = response.jsonPath();
		assertThat(json.getObject("id", Long.class)).isNotNull();
	}

}
