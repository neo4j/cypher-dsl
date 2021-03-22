/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
package org.neo4j.cypherdsl.examples.sdn6;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIf;
import org.neo4j.cypherdsl.examples.sdn6.movies.Movie;
import org.neo4j.cypherdsl.examples.sdn6.movies.Person;
import org.neo4j.cypherdsl.examples.sdn6.movies.PersonDetails;
import org.neo4j.harness.Neo4j;
import org.neo4j.harness.Neo4jBuilders;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.ResourceLoader;
import org.springframework.data.neo4j.repository.config.EnableNeo4jRepositories;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.util.FileCopyUtils;

/**
 * @author Michael J. Simons
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@EnabledIf("is606OrHigher")
class ApplicationIT {

	private static Neo4j neo4j;

	static boolean is606OrHigher() {
		String version = Optional.of(EnableNeo4jRepositories.class)
			.map(Class::getPackage).map(Package::getImplementationVersion)
			.map(String::trim)
			.filter(v -> !v.isEmpty())
			.orElse("0");
		var matcher = Pattern.compile("(\\d+)\\.(\\d+)\\.(\\d+)(?:.*)").matcher(version);
		return matcher.matches() && Integer.parseInt(matcher.group(1)) >= 6 &&
			(Integer.parseInt(matcher.group(2)) == 0 && Integer.parseInt(matcher.group(3)) >= 6 || Integer.parseInt(matcher.group(2)) >= 1);
	}

	@BeforeAll
	static void startNeo4j() throws IOException {
		String movies;
		ResourceLoader r = new DefaultResourceLoader();
		try (var in = new InputStreamReader(r.getResource("classpath:movies.cypher").getInputStream())) {
			movies = FileCopyUtils.copyToString(in);
		}
		neo4j = Neo4jBuilders.newInProcessBuilder()
			.withFixture(movies)
			.build();
	}

	@DynamicPropertySource
	static void neo4jProperties(DynamicPropertyRegistry registry) {
		registry.add("spring.neo4j.uri=", neo4j::boltURI);
		registry.add("spring.neo4j.authentication.username", () -> "neo4j");
		registry.add("spring.neo4j.authentication.password", () -> "");
	}

	@Test
	@DisplayName("Retrieving mapped objects sorted by a static field.")
	void getMoviesShouldWork(@Autowired TestRestTemplate restTemplate) {

		var exchange = restTemplate
			.exchange("/api/movies", HttpMethod.GET, null, new ParameterizedTypeReference<List<Movie>>() {
			});
		assertThat(exchange.getStatusCode()).isEqualTo(HttpStatus.OK);
		assertThat(exchange.getBody())
			.hasSize(38)
			.element(0).extracting(Movie::getTitle).isEqualTo("A Few Good Men");
		assertThat(exchange.getBody())
			.last().extracting(Movie::getTitle).isEqualTo("You've Got Mail");
	}

	@Test
	@DisplayName("Running a complex query via the Neo4j template itself.")
	void getRelatedToShouldWork(@Autowired TestRestTemplate restTemplate) {

		var exchange = restTemplate.exchange("/api/movies/relatedTo/{name}", HttpMethod.GET, null,
			new ParameterizedTypeReference<List<Movie>>() {
			}, "Tom Hanks");
		assertThat(exchange.getStatusCode()).isEqualTo(HttpStatus.OK);
		assertThat(exchange.getBody()).extracting(Movie::getTitle)
			.containsAll(List.of(
				"A League of Their Own",
				"Apollo 13",
				"Cast Away",
				"Charlie Wilson's War",
				"Cloud Atlas",
				"Joe Versus the Volcano",
				"Sleepless in Seattle",
				"That Thing You Do",
				"The Da Vinci Code",
				"The Green Mile",
				"The Polar Express",
				"You've Got Mail"
			));
	}

	@Test
	@DisplayName("Running a complex query for a projection via the Cypher DSL executor")
	void getDetailsShouldWork(@Autowired TestRestTemplate restTemplate) {

		var exchange = restTemplate
			.exchange("/api/people/details/{name}", HttpMethod.GET, null, PersonDetails.class, "Tom Hanks");
		assertThat(exchange.getStatusCode()).isEqualTo(HttpStatus.OK);
		var details = exchange.getBody();

		assertThat(details.getName()).isEqualTo("Tom Hanks");
		assertThat(details.getActedIn()).hasSize(12);
		assertThat(details.getDirected()).extracting(Movie::getTitle).containsExactly("That Thing You Do");
		assertThat(details.getRelated()).hasSize(35);
		assertThat(details.getBorn()).isEqualTo(1956);
	}

	@Test
	@DisplayName("Using conditions pt1.")
	void findPeopleBornInThe70tiesOrShouldWork1(@Autowired TestRestTemplate restTemplate) {

		var exchange = restTemplate
			.exchange("/api/people/findPeopleBornInThe70tiesOr/", HttpMethod.GET, null, new ParameterizedTypeReference<List<Person>>() {
			});
		assertThat(exchange.getStatusCode()).isEqualTo(HttpStatus.OK);
		var people = exchange.getBody();
		assertThat(people).hasSize(17);
	}

	@Test
	@DisplayName("Using conditions pt2.")
	void findPeopleBornInThe70tiesOrShouldWork2(@Autowired TestRestTemplate restTemplate) {

		var exchange = restTemplate
			.exchange("/api/people/findPeopleBornInThe70tiesOr?name={name}", HttpMethod.GET, null, new ParameterizedTypeReference<List<Person>>() {
			}, "Natalie Portman");
		assertThat(exchange.getStatusCode()).isEqualTo(HttpStatus.OK);
		var people = exchange.getBody();
		assertThat(people).hasSize(18);
	}

	@AfterAll
	static void stopNeo4j() {
		neo4j.close();
	}
}
