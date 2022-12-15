/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.examples.sdn6;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.io.InputStreamReader;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Optional;
import java.util.regex.Pattern;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIf;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Functions;
import org.neo4j.cypherdsl.examples.sdn6.movies.GenreRepository;
import org.neo4j.cypherdsl.examples.sdn6.movies.Genre_;
import org.neo4j.cypherdsl.examples.sdn6.movies.Movie;
import org.neo4j.cypherdsl.examples.sdn6.movies.Person;
import org.neo4j.cypherdsl.examples.sdn6.movies.PersonDetails;
import org.neo4j.cypherdsl.examples.sdn6.movies.NewPersonCmd;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.GraphDatabase;
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
import org.testcontainers.containers.Neo4jContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * @author Michael J. Simons
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@EnabledIf("is606OrHigher")
@Testcontainers(disabledWithoutDocker = true)
class ApplicationIT {

	@Container
	private static final Neo4jContainer<?> neo4j = new Neo4jContainer<>("neo4j:4.4").withReuse(true);

	@SuppressWarnings("unused") // It is used via {@code @EnableIf}
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
	static void loadMovies() throws IOException {
		ResourceLoader r = new DefaultResourceLoader();
		try (var in = new InputStreamReader(r.getResource("classpath:movies.cypher").getInputStream());
			var driver = GraphDatabase.driver(neo4j.getBoltUrl(), AuthTokens.basic("neo4j", neo4j.getAdminPassword()));
			var session = driver.session()
		) {
			var movies = FileCopyUtils.copyToString(in);
			session.run(movies);
		}
	}

	@SuppressWarnings("unused") // Used via Spring magic
	@DynamicPropertySource
	static void neo4jProperties(DynamicPropertyRegistry registry) {
		registry.add("spring.neo4j.uri=", neo4j::getBoltUrl);
		registry.add("spring.neo4j.authentication.username", () -> "neo4j");
		registry.add("spring.neo4j.authentication.password", () -> neo4j.getAdminPassword());
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

		assertThat(details).isNotNull();
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

	@Test
	@DisplayName("Using conditions pt3.")
	void findPeopleBornAfterThe70tiesShouldWork(@Autowired TestRestTemplate restTemplate) {

		// tag::exchange1[]
		var exchange = restTemplate.exchange(
			"/api/people/v1/findPeopleBornAfterThe70ties?conditions={conditions}",
			HttpMethod.GET,
			null, new ParameterizedTypeReference<List<Person>>() { },
			"person.name contains \"Ricci\" OR person.name ends with 'Hirsch'"
		);
		// end::exchange1[]
		assertThat(exchange.getStatusCode()).isEqualTo(HttpStatus.OK);
		var people = exchange.getBody();
		assertThat(people).hasSize(2);
	}

	@Test
	@DisplayName("Using conditions pt4.")
	void findPeopleBornAfterThe70tiesV2ShouldWork(@Autowired TestRestTemplate restTemplate) {

		var exchange = restTemplate
			.exchange("/api/people/v2/findPeopleBornAfterThe70ties?conditions={conditions}", HttpMethod.GET, null, new ParameterizedTypeReference<List<Person>>() {
			}, "name contains \"Ricci\" OR name ends with 'Hirsch'");
		assertThat(exchange.getStatusCode()).isEqualTo(HttpStatus.OK);
		var people = exchange.getBody();
		assertThat(people).hasSize(2);
	}

	@Test
	@DisplayName("Using parameters")
	void usingTemporalsAsParameterShouldWork(@Autowired TestRestTemplate restTemplate) {

		var dob = ZonedDateTime.of(1990, 10, 31, 23, 42, 0, 0, ZoneId.of("Europe/Berlin"));
		var result = restTemplate.postForObject("/api/people/createNewPerson", new NewPersonCmd(
			"Liv Lisa Fries", dob),
			Person.class);

		assertThat(result.getBorn()).isEqualTo(1990);
		assertThat(result.getDob()).isEqualTo(dob);
	}

	@Test // GH-315
	void usingCypherDSLExecutor(@Autowired GenreRepository genreRepository) {

		var genreModel = Genre_.GENRE
			.withProperties(Genre_.GENRE.NAME, Cypher.literalOf("Comedy"));
		var byStatment = Cypher.merge(genreModel)
			.onCreate()
			.set(genreModel.ID.to(Functions.randomUUID()))
			.returning(genreModel)
			.build();
		var newGenre = genreRepository.findOne(byStatment);
		assertThat(newGenre).hasValueSatisfying(genre -> {
			assertThat(genre.getId()).isNotNull();
			assertThat(genre.getName()).isEqualTo("Comedy");
		});
	}
}
