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
package org.neo4j.cypherdsl.core;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.parameter.ConflictingParametersException;
import org.neo4j.cypherdsl.core.parameter.ParameterCollector;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

/**
 * @author Andreas Berger
 * @author Michael J. Simons
 */
class ParameterIT {

	private final Node userNode = Cypher.node("User").named("u");

	@Test
	void shouldCollectParameters() {
		Statement statement = Cypher
				.match(userNode)
				.where(userNode.property("name").isEqualTo(Cypher.parameter("name", "Neo")))
				.returning(userNode)
				.limit(Cypher.parameter("param").withValue(5)).build();

		Map<String, Object> usedParams = ParameterCollector.collectBoundParameters(statement);
		assertThat(usedParams)
			.containsEntry("param", 5)
			.containsEntry("name", "Neo");
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
		Map<String, Object> usedParams = ParameterCollector.collectBoundParameters(statement);
		assertThat(usedParams).containsEntry("param", null);
	}

	@Test
	void shouldFailOnDifferentBoundValues() {
		Statement statement = Cypher
				.match(userNode)
				.returning(userNode)
				.skip(Cypher.parameter("param").withValue(1))
				.limit(Cypher.parameter("param").withValue(5)).build();

		assertThatExceptionOfType(ConflictingParametersException.class)
				.isThrownBy(() -> ParameterCollector.collectBoundParameters(statement))
				.satisfies(e -> {
					Map<String, Set<Object>> erroneousParameters = e.getErroneousParameters();
					assertThat(erroneousParameters).containsKey("param");
					Set<Object> values = erroneousParameters.get("param");
					assertThat(values).containsExactlyInAnyOrder(1, 5);
				});
	}

	@Test
	void shouldFailOnDifferentBoundValuesWhenSameValueIsUsedTwice() {
		Statement statement = Cypher
				.match(userNode)
				.where(userNode.internalId().isEqualTo(Cypher.parameter("param").withValue(5)))
				.returning(userNode)
				.skip(Cypher.parameter("param").withValue(1))
				.limit(Cypher.parameter("param").withValue(1)).build();

		assertThatExceptionOfType(ConflictingParametersException.class)
				.isThrownBy(() -> ParameterCollector.collectBoundParameters(statement))
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
		assertThat(ParameterCollector.collectBoundParameters(statement1)).containsEntry("p1", "A");

		Statement statement2 = Cypher.match(bikeNode)
				.where(bikeNode.property("b").isEqualTo(Cypher.parameter("p2").withValue("B")))
				.returning(bikeNode)
				.build();
		assertThat(ParameterCollector.collectBoundParameters(statement2)).containsEntry("p2", "B");

		Statement statement3 = Cypher.match(bikeNode)
				.where(bikeNode.property("c").isEqualTo(Cypher.parameter("p3").withValue("C")))
				.returning(bikeNode)
				.build();
		assertThat(ParameterCollector.collectBoundParameters(statement3)).containsEntry("p3", "C");

		Statement statement = Cypher.union(statement1, statement2, statement3);

		assertThat(Renderer.getDefaultRenderer().render(statement))
				.isEqualTo(
						"MATCH (b:`Bike`) WHERE b.a = $p1 RETURN b UNION MATCH (b) WHERE b.b = $p2 RETURN b UNION MATCH (b) WHERE b.c = $p3 RETURN b");

		Map<String, Object> usedParams = ParameterCollector.collectBoundParameters(statement);
		Map<String, Object> expectedParams = new HashMap<>();
		expectedParams.put("p1", "A");
		expectedParams.put("p2", "B");
		expectedParams.put("p3", "C");
		assertThat(usedParams).containsAllEntriesOf(expectedParams);
	}
}
