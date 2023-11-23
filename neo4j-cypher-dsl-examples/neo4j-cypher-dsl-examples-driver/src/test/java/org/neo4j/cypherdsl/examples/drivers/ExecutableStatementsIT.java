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
package org.neo4j.cypherdsl.examples.drivers;

import static org.assertj.core.api.Assertions.assertThat;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.ResultStatement;
import org.neo4j.cypherdsl.core.executables.ExecutableStatement;
import org.neo4j.cypherdsl.core.executables.ReactiveExecutableStatement;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.async.AsyncSession;
import org.neo4j.driver.reactivestreams.ReactiveSession;
import org.testcontainers.containers.BindMode;
import org.testcontainers.containers.Neo4jContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * @author Michael J. Simons
 * @soundtrack Motörhead - Live At Brixton Academy
 */
@Testcontainers(disabledWithoutDocker = true)
class ExecutableStatementsIT {

	@Container
	private static final Neo4jContainer<?> neo4j = new Neo4jContainer<>("neo4j:4.4")
		.withClasspathResourceMapping("/books.csv", "/import/books.csv", BindMode.READ_ONLY)
		.withReuse(true);

	private static Driver driver;

	@BeforeAll
	static void loadMovies() throws IOException, URISyntaxException {

		// tag::create-driver[]
		driver = GraphDatabase.driver(neo4j.getBoltUrl(), AuthTokens.basic("neo4j", neo4j.getAdminPassword()));
		// end::create-driver[]
		try (var session = driver.session()) {
			var movies = Files.readString(Path.of(ExecutableStatementsIT.class.getResource("/movies.cypher").toURI()));
			session.run(movies);
		}

		Objects.requireNonNull(Placeholder.class);
	}

	@AfterAll
	static void closeDriver() {
		driver.close();
	}

	@Test
	void statementWithoutResult() {

		// tag::statement-without-result[]
		var m = Cypher.node("Test").named("m");
		var statement = ExecutableStatement.of(Cypher.create(m)
			.set(m.property("name").to(Cypher.anonParameter("statementWithoutResult")))
			.build());
		// end::statement-without-result[]

		// tag::statement-without-result-unmanaged-tx[]
		try (var session = driver.session();
			var tx = session.beginTransaction() // <.>
		) {
			var summary = statement.executeWith(tx); // <.>
			tx.commit(); // <.>

			assertThat(summary.counters().nodesCreated()).isEqualTo(1);
		}
		// end::statement-without-result-unmanaged-tx[]
	}

	@Test
	void statementWithoutResultWithTxFunction() {

		var m = Cypher.node("Test").named("m");
		var statement = ExecutableStatement.of(Cypher.create(m)
			.set(m.property("name").to(Cypher.anonParameter("statementWithoutResultWithTxFunction")))
			.build());

		// tag::statement-without-result-tx-function[]
		try (var session = driver.session()) { // <.>
			var summary = session.executeWrite(statement::executeWith); // <.>
			assertThat(summary.counters().nodesCreated()).isEqualTo(1); // <.>
		}
		// end::statement-without-result-tx-function[]
	}

	@Test
	void statementsWithResultListed() {

		// tag::statement-with-result[]
		var m = Cypher.node("Movie").named("m");
		var statement = ExecutableStatement.of(Cypher.match(m)
			.returning(m.property("title").as("title")).build());
		// end::statement-with-result[]

		// tag::statement-with-result-unmanaged-tx[]
		try (var session = driver.session();
			var tx = session.beginTransaction()) {

			var moviesTitles = statement.fetchWith(
				tx, // <.>
				r -> r.get("title").asString() // <.>
			);
			tx.commit();

			assertMovieTitleList(moviesTitles);
		}
		// end::statement-with-result-unmanaged-tx[]
	}

	@Test
	void statementsWithResultListedWithTxFunction() {

		var m = Cypher.node("Movie").named("m");
		var statement = ExecutableStatement.of(Cypher.match(m).returning(m.property("title").as("title")).build());

		// tag::statement-with-result-tx-function[]
		try (var session = driver.session()) { // <.>

			var moviesTitles = session.executeRead(statement::fetchWith) // <.>
				.stream() // <.>
				.map(r -> r.get("title").asString())
				.collect(Collectors.toList());

			assertMovieTitleList(moviesTitles);
		}
		// end::statement-with-result-tx-function[]
	}

	@Test
	void statementsWithResultStreamed() {

		var m = Cypher.node("Movie").named("m");
		var statement = ExecutableStatement.of(Cypher.match(m)
			.returning(m.property("title").as("title")).build());

		// tag::statement-with-result-auto-commit-stream[]
		try (var session = driver.session()) {

			var summary = statement.streamWith(session, stream -> {
				var moviesTitles = stream
					.map(r -> r.get("title").asString())
					.collect(Collectors.toList());
				assertMovieTitleList(moviesTitles);
			});
			assertThat(summary.query().text()).isEqualTo(statement.getCypher());
		}
		// end::statement-with-result-auto-commit-stream[]
	}

	@Test
	void statementsWithResultStreamedInTxFunction() {

		var m = Cypher.node("Movie").named("m");
		var statement = ExecutableStatement.of(Cypher.match(m)
			.returning(m.property("title").as("title")).build());

		// tag::statement-with-result-tx-function-stream[]
		try (var session = driver.session()) { // <.>

			var summary = session.executeRead(tx -> // <.>
				statement.streamWith(tx, s -> { // <.>
					var moviesTitles = s.map(r -> r.get("title").asString())
						.collect(Collectors.toList()); // <.>
					assertMovieTitleList(moviesTitles);
				})
			);
			assertThat(summary.query().text()).isEqualTo(statement.getCypher());
		}
		// end::statement-with-result-tx-function-stream[]
	}

	@Test
	void statementsWithResultStreamedProfiled() {

		var m = Cypher.node("Movie").named("m");
		var statement = ExecutableStatement.of((ResultStatement) Cypher.match(m)
			.returning(m.property("title").as("title")).profile());

		try (var session = driver.session()) {

			var summary = statement.streamWith(session, s -> {
				// We don't consume the stream but only are interested in the profile
			});
			assertThat(summary.query().text()).isEqualTo(statement.getCypher());
			assertThat(summary.profile()).isNotNull();
		}
	}

	@Test
	void statementsWithoutResultReactive() {

		// tag::create-statement-without-result-reactive[]
		var m = Cypher.node("Test").named("m");
		var statement = ReactiveExecutableStatement.of(Cypher.create(m)
			.set(m.property("name").to(Cypher.anonParameter("statementsWithoutResultReactive")))
			.build());
		// end::create-statement-without-result-reactive[]

		// tag::statement-without-result-tx-function-reactive-stream[]
		Mono.usingWhen(
			Mono.fromSupplier(() -> driver.session(ReactiveSession.class)), // <.>
			s -> Mono.fromDirect(s.executeWrite(statement::executeWith)), // <.>
			ReactiveSession::close // <.>
		).as(StepVerifier::create)
			.expectNextMatches(r -> r.counters().nodesCreated() == 1) // <.>
			.verifyComplete();
		// end::statement-without-result-tx-function-reactive-stream[]
	}

	@Test
	void statementsWithResultReactive() {

		var m = Cypher.node("Movie").named("m");
		var statement = ReactiveExecutableStatement.of(Cypher.match(m)
			.returning(m.property("title").as("title")).build());

		Flux.usingWhen(Mono.fromSupplier(() -> driver.session(ReactiveSession.class)), statement::fetchWith, ReactiveSession::close)
			.as(StepVerifier::create)
			.expectNextCount(38)
			.verifyComplete();
	}

	@Test
	void statementsWithResultReactiveTxFunction() {

		var m = Cypher.node("Movie").named("m");
		var statement = ReactiveExecutableStatement.of(Cypher.match(m)
			.returning(m.property("title").as("title")).build());

		// tag::statement-with-result-tx-function-reactive-stream[]
		Flux.usingWhen(
			Mono.fromSupplier(() -> driver.session(ReactiveSession.class)),
			s -> s.executeRead(statement::fetchWith),
			ReactiveSession::close
		)
			.skip(2)
			.take(30)
			.as(StepVerifier::create)
			.expectNextCount(30)
			.verifyComplete();
		// end::statement-with-result-tx-function-reactive-stream[]
	}

	@Test
	void usingPeriodicCommit() {

		// tag::periodic-commit-statement[]
		var row = Cypher.name("row");
		var a = Cypher.node("Author").withProperties("name", Cypher.trim(Cypher.name("author"))).named("a");
		var m = Cypher.node("Book").withProperties("name", row.property("Title")).named("b");

		var statement = ReactiveExecutableStatement.of(Cypher.usingPeriodicCommit(10)
			.loadCSV(URI.create("file:///books.csv"), true).as(row).withFieldTerminator(";")
			.create(m)
			.with(m.getRequiredSymbolicName(), row)
			.unwind(Cypher.split(row.property("Authors"), "&")).as("author")
			.merge(a)
			.create(a.relationshipTo(m, "WROTE").named("r"))
			.returningDistinct(m.property("name").as("name"))
			.build());

		Flux.using(() -> driver.session(ReactiveSession.class), statement::fetchWith, ReactiveSession::close)
			.as(StepVerifier::create)
			.expectNextCount(50)
			.verifyComplete();
		// end::periodic-commit-statement[]

		try (var session = driver.session()) {

			var books = session.run("MATCH (b:Book) RETURN count(b)").single().get(0).asLong();
			var authors = session.run("MATCH (a:Author) RETURN count(a)").single().get(0).asLong();

			assertThat(books).isEqualTo(50L);
			assertThat(authors).isEqualTo(5L);
		}
	}

	@Test
	void statementWithoutResultAsync() {

		var m = Cypher.node("Test").named("m");
		var statement = ReactiveExecutableStatement.of(Cypher.create(m)
			.set(m.property("name").to(Cypher.anonParameter("statementWithoutResult")))
			.build());

		var session = driver.session(AsyncSession.class);

		var futureResultSummary = statement.executeWith(session);
		assertThat(futureResultSummary)
			.succeedsWithin(Duration.ofSeconds(3))
			.matches(r -> r.counters().nodesCreated() == 1);

		var closingSession = session.closeAsync().toCompletableFuture();
		assertThat(closingSession)
			.succeedsWithin(Duration.ofSeconds(3));
	}

	@Test
	void statementWithResultAsync() {

		var m = Cypher.node("Movie").named("m");
		var statement = ExecutableStatement.of(Cypher.match(m)
			.returning(m.property("title").as("title")).build());

		var session = driver.session(AsyncSession.class);

		var futureResultSummary = statement.fetchWith(session);
		assertThat(futureResultSummary)
			.succeedsWithin(Duration.ofSeconds(3))
			.matches(r -> r.size() == 38);

		var closingSession = session.closeAsync().toCompletableFuture();
		assertThat(closingSession)
			.succeedsWithin(Duration.ofSeconds(3));
	}

	void assertMovieTitleList(List<String> movieTitles) {
		assertThat(movieTitles).hasSize(38);
		assertThat(movieTitles).contains("The Matrix");
	}
}
