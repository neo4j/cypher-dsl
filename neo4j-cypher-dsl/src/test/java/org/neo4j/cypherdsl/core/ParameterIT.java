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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.util.Map;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Andreas Berger
 * @author Michael J. Simons
 */
@SuppressWarnings("removal") // This is for the parameter methods. I want to keep calling them this way until they are gone.
class ParameterIT {

	private static final Node userNode = Cypher.node("User").named("u");

	@Test
	void shouldCollectParameters() {
		Statement statement = Cypher
				.match(userNode)
				.where(userNode.property("name").isEqualTo(Cypher.parameter("name", "Neo")))
				.returning(userNode)
				.limit(Cypher.parameter("param").withValue(5)).build();

		assertThat(statement.getCatalog().getParameters())
			.containsEntry("param", 5)
			.containsEntry("name", "Neo");

		assertThat(statement.getCatalog().getParameterNames())
			.containsExactlyInAnyOrder("param", "name");
	}

	@Test
	void shouldDealWithNullValues() {
		Statement statement = Cypher
				.match(userNode)
				.set(userNode.property("name").to(Cypher.parameter("param").withValue(null)))
				.returning(userNode)
				.build();

		assertThat(Renderer.getDefaultRenderer().render(statement))
				.isEqualTo("MATCH (u:`User`) SET u.name = $param RETURN u");
		assertThat(statement.getCatalog().getParameters()).containsEntry("param", null);
	}

	private static Stream<Arguments> conflictingParameters() {
		return Stream.of("x", null)
			.flatMap(v -> Stream.of(
				Arguments.of(
					Cypher.match(userNode)
						.set(
							userNode.property("name").to(Cypher.parameter("param").withValue(v)),
							userNode.property("firstName").to(Cypher.parameter("param"))
						).returning(userNode).build()
				),
				Arguments.of(
					Cypher.match(userNode)
						.set(
							userNode.property("firstName").to(Cypher.parameter("param")),
							userNode.property("name").to(Cypher.parameter("param").withValue(v))
						).returning(userNode).build()
				)
			));
	}


	@ParameterizedTest
	@MethodSource("conflictingParameters")
	void shouldFailWithNoValueVsNull(Statement statement) {

		assertThatExceptionOfType(ConflictingParametersException.class)
			.isThrownBy(statement::getCatalog);
	}

	@Test
	void shouldNotFailWithSameNameAndMultipleNulLValues() {

		Statement statement = Cypher
			.match(userNode)
			.set(
				userNode.property("name").to(Cypher.parameter("param").withValue(null)),
				userNode.property("firstName").to(Cypher.parameter("param").withValue(null))
			)
			.returning(userNode)
			.build();

		assertThat(Renderer.getDefaultRenderer().render(statement))
			.isEqualTo("MATCH (u:`User`) SET u.name = $param, u.firstName = $param RETURN u");
		assertThat(statement.getCatalog().getParameters()).containsEntry("param", null);
	}

	@Test
	void shouldNotFailWithSameNameAndNoValue() {

		Statement statement = Cypher
			.match(userNode)
			.set(
				userNode.property("name").to(Cypher.parameter("param")),
				userNode.property("firstName").to(Cypher.parameter("param"))
			)
			.returning(userNode)
			.build();

		assertThat(Renderer.getDefaultRenderer().render(statement))
			.isEqualTo("MATCH (u:`User`) SET u.name = $param, u.firstName = $param RETURN u");
		assertThat(statement.getCatalog().getParameters()).isEmpty();
		assertThat(statement.getCatalog().getParameterNames()).containsExactlyInAnyOrder("param");
	}

	@Test
	void shouldFailOnDifferentBoundValues() {
		Statement statement = Cypher
				.match(userNode)
				.returning(userNode)
				.skip(Cypher.parameter("param").withValue(1))
				.limit(Cypher.parameter("param").withValue(5)).build();

		assertThatExceptionOfType(ConflictingParametersException.class)
				.isThrownBy(statement::getCatalog)
				.satisfies(e -> {
					Map<String, Set<Object>> erroneousParameters = e.getErroneousParameters();
					assertThat(erroneousParameters).containsKey("param");
					Set<Object> values = erroneousParameters.get("param");
					assertThat(values).containsExactlyInAnyOrder(1, 5);
				});
	}

	@SuppressWarnings("deprecation")
	@Test
	void shouldFailOnDifferentBoundValuesWhenSameValueIsUsedTwice() {
		Statement statement = Cypher
				.match(userNode)
				.where(userNode.internalId().isEqualTo(Cypher.parameter("param").withValue(5)))
				.returning(userNode)
				.skip(Cypher.parameter("param").withValue(1))
				.limit(Cypher.parameter("param").withValue(1)).build();

		assertThatExceptionOfType(ConflictingParametersException.class)
				.isThrownBy(statement::getCatalog)
				.satisfies(e -> {
					Map<String, Set<Object>> erroneousParameters = e.getErroneousParameters();
					assertThat(erroneousParameters).containsKey("param");
					Set<Object> values = erroneousParameters.get("param");
					assertThat(values).containsExactlyInAnyOrder(1, 5);
				});
	}

	@Test
	void shouldWorkWithUnions() {

		final Node bikeNode = Cypher.node("Bike").named("b");

		Statement statement1 = Cypher.match(bikeNode)
				.where(bikeNode.property("a").isEqualTo(Cypher.parameter("p1").withValue("A")))
				.returning(bikeNode)
				.build();
		assertThat(statement1.getCatalog().getParameters()).containsEntry("p1", "A");

		Statement statement2 = Cypher.match(bikeNode)
				.where(bikeNode.property("b").isEqualTo(Cypher.parameter("p2").withValue("B")))
				.returning(bikeNode)
				.build();
		assertThat(statement2.getCatalog().getParameters()).containsEntry("p2", "B");

		Statement statement3 = Cypher.match(bikeNode)
				.where(bikeNode.property("c").isEqualTo(Cypher.parameter("p3").withValue("C")))
				.returning(bikeNode)
				.build();
		assertThat(statement3.getCatalog().getParameters()).containsEntry("p3", "C");

		Statement statement = Cypher.union(statement1, statement2, statement3);

		assertThat(Renderer.getDefaultRenderer().render(statement))
				.isEqualTo(
						"MATCH (b:`Bike`) WHERE b.a = $p1 RETURN b UNION MATCH (b:`Bike`) WHERE b.b = $p2 RETURN b UNION MATCH (b:`Bike`) WHERE b.c = $p3 RETURN b");

		Map<String, Object> expectedParams = Map.of(
			"p1", "A",
			"p2", "B",
			"p3", "C"
		);
		assertThat(statement.getCatalog().getParameters()).containsAllEntriesOf(expectedParams);
	}
}
