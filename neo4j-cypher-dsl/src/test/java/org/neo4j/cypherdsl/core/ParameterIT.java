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
				.returning(userNode)
				.limit(Cypher.parameter("param").withValue(5)).build();

		Map<String, Object> usedParams = ParameterCollector.collectBoundParameters(statement);
		assertThat(usedParams).containsEntry("param", 5);
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
}
