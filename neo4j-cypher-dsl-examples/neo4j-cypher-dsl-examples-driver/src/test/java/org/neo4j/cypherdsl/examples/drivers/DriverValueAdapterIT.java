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
package org.neo4j.cypherdsl.examples.drivers;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetTime;
import java.time.Period;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer;
import org.neo4j.cypherdsl.core.renderer.Renderer;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.Value;
import org.neo4j.driver.Values;
import org.testcontainers.containers.Neo4jContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * @author Michael J. Simons
 */
@Testcontainers(disabledWithoutDocker = true)
class DriverValueAdapterIT {

	private final GeneralizedRenderer renderer = Renderer.getRenderer(Configuration.defaultConfig(), GeneralizedRenderer.class);

	@SuppressWarnings("resource")
	@Container
	private static final Neo4jContainer<?> neo4j = new Neo4jContainer<>("neo4j:4.4")
		.withReuse(true);

	private static Driver driver;

	@BeforeAll
	static void openDriver() {

		neo4j.start();
		driver = GraphDatabase.driver(neo4j.getBoltUrl(), AuthTokens.basic("neo4j", neo4j.getAdminPassword()));
	}

	@AfterAll
	static void closeDriver() {
		driver.close();
	}

	@Test
	void nodesAndRelationshipsDontWorkDirectly() {

		try (var session = driver.session()) {
			var row = session.run("CREATE (n:ANode) -[r:FOO] ->(:AnotherNode) RETURN n, r").single();
			var adapter = Cypher.adapt(row.get("n"));
			assertThatIllegalArgumentException().isThrownBy(adapter::asExpression)
				.withMessage("Node values can only be adapted with asNode");
			adapter = Cypher.adapt(row.get("r"));
			assertThatIllegalArgumentException().isThrownBy(adapter::asExpression)
				.withMessage("Relationship values can only be adapted with asRelationship");
		}
	}

	@Test
	void nodesShouldWork() {

		try (var session = driver.session()) {
			var row = session.run("CREATE (n:ANode {name: 'MS'}) -[r:FOO] ->(m:AnotherNode), (x), (y {foo: 'bar'}) RETURN *").single();
			var node = Cypher.adapt(row.get("n")).asNode().named("n");
			assertThat(renderer.render(node)).isEqualTo("(n:`ANode` {name: 'MS'})");
			node = Cypher.adapt(row.get("m")).asNode().named("n");
			assertThat(renderer.render(node)).isEqualTo("(n:`AnotherNode`)");
			node = Cypher.adapt(row.get("x")).asNode();
			assertThat(renderer.render(node)).isEqualTo("()");
			node = Cypher.adapt(row.get("y")).asNode().named("y");
			assertThat(renderer.render(node)).isEqualTo("(y {foo: 'bar'})");
		}
	}

	@Test
	void relationshipsShouldWork() {

		try (var session = driver.session()) {
			var row = session.run("CREATE (n:ANode {name: 'MS'}) -[r:FOO] ->(m:AnotherNode), (x) -[r2:FOO {someProperty: date('2023-03-24')}] -> (y {foo: 'bar'}) RETURN *").single();
			var relationship = Cypher.adapt(row.get("r")).asRelationship().named("r");
			assertThat(renderer.render(relationship)).isEqualTo("()-[r:`FOO`]->()");
			relationship = Cypher.adapt(row.get("r2")).asRelationship().named("r");
			assertThat(renderer.render(relationship)).isEqualTo("()-[r:`FOO` {someProperty: date('2023-03-24')}]->()");
		}
	}


	static Stream<Arguments> allExpressionsShouldWork() {

		return Stream.of(
			Arguments.of(Values.value("Hallo, 'Cypher")),
			Arguments.of(Values.value(23)),
			Arguments.of(Values.value(42L)),
			Arguments.of(Values.value((short) 1)),
			Arguments.of(Values.value(23.0f)),
			Arguments.of(Values.value(42.1)),
			Arguments.of(Values.value(true)),
			Arguments.of(Values.point(7203, 3.0, 0.0)),
			Arguments.of(Values.point(4326, 56.0, 12.0)),
			Arguments.of(Values.point(9157, 0, 4, 1)),
			Arguments.of(Values.point(4979, 56, 12, 1000)),
			Arguments.of(Values.value(LocalDate.of(2023, 3, 24))),
			Arguments.of(Values.value(OffsetTime.of(10, 50, 23, 0, ZoneOffset.of("+2")))),
			Arguments.of(Values.value(LocalTime.of(10, 50, 23))),
			Arguments.of(Values.value(ZonedDateTime.of(LocalDate.of(2023, 3, 24), LocalTime.of(10, 50, 23), ZoneId.of("Europe/Berlin")))),
			Arguments.of(Values.value(LocalDateTime.of(LocalDate.of(2023, 3, 24), LocalTime.of(10, 50, 23)))),
			Arguments.of(Values.value(Period.ofDays(23).plusMonths(1))),
			Arguments.of(Values.value(Period.ofYears(1).plusMonths(23).plusMonths(2).plusDays(400))),
			Arguments.of(Values.value(Duration.ofMinutes(1).plusSeconds(30).plusMillis(500))),
			Arguments.of(Values.value(List.of("Hallo", 123, List.of(Values.value(1)), Values.value(1, 2, 3), Values.point(7203, 3.0, 0.0)))),
			Arguments.of(Values.value(
				Map.of(
					"aList", List.of("Hallo", 123, Values.value(1, 2, 3)),
					"aNumber", 47,
					"aNestedMap", new TreeMap<>(Map.of("aPoint", Values.point(7203, 3.0, 0.0), "aDuration", Values.value(Duration.ofHours(23)))),
					"asd", Values.value("haha")
				)
			)),
			Arguments.of(Values.NULL)
		);
	}

	@ParameterizedTest
	@MethodSource
	void allExpressionsShouldWork(Value value) {

		try (var session = driver.session()) {
			var result = session.run(Cypher.returning(Cypher.adapt(value).asExpression().as("v")).build().getCypher())
				.single().get("v");
			assertThat(result).isEqualTo(value);
		}
	}
}
