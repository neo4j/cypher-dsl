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

import java.util.stream.Stream;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
// tag::dialect-example[]
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;
// end::dialect-example[]

/**
 * @author Michael J. Simons
 */
// tag::dialect-example[]

class DialectIT {

	// end::dialect-example[]

	static Stream<Arguments> nPropExists() {
		return Stream.of(
			Arguments.of(Dialect.NEO4J_4, false, "MATCH (n:`Movie`) WHERE exists(n.title) RETURN n"),
			Arguments.of(Dialect.NEO4J_4, true, "MATCH (n:`Movie`) WHERE NOT (exists(n.title)) RETURN n"),
			Arguments.of(Dialect.NEO4J_5, false, "MATCH (n:`Movie`) WHERE n.title IS NOT NULL RETURN n"),
			Arguments.of(Dialect.NEO4J_5, true, "MATCH (n:`Movie`) WHERE n.title IS NULL RETURN n")
		);
	}

	@ParameterizedTest
	@MethodSource
	void nPropExists(Dialect dialect, boolean negate, String expected) {

		Node n = Cypher.node("Movie").named("n");
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
		Condition condition = Cypher.exists(n.property("title"));
		if (negate) {
			condition = condition.not();
		}
		String cypher = renderer.render(
			Cypher.match(n).where(condition).returning(n).build());
		assertThat(cypher).isEqualTo(expected);
	}

	static Stream<Arguments> distanceFunction() {
		return Stream.of(
			Arguments.of(Dialect.NEO4J_4, "MATCH (n) RETURN distance(n.a, n.b)"),
			Arguments.of(Dialect.NEO4J_5, "MATCH (n) RETURN point.distance(n.a, n.b)")
		);
	}

	@ParameterizedTest
	@MethodSource
	void distanceFunction(Dialect dialect, String expected) {

		Node n = Cypher.anyNode("n");
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
		String cypher = renderer.render(
			Cypher.match(n).returning(Cypher.distance(n.property("a"), n.property("b"))).build());
		assertThat(cypher).isEqualTo(expected);
	}


	static Stream<Arguments> elementId() {
		return Stream.of(
			Arguments.of(Dialect.NEO4J_4, "MATCH (n) RETURN toString(id(n))"),
			Arguments.of(Dialect.NEO4J_5, "MATCH (n) RETURN elementId(n)")
		);
	}

	@ParameterizedTest
	@MethodSource
	void elementId(Dialect dialect, String expected) {

		Node n = Cypher.anyNode("n");

		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
		String cypher = renderer.render(
			Cypher.match(n).returning(n.elementId()).build());
		assertThat(cypher).isEqualTo(expected);
	}

	@Test // GH-539
	// tag::dialect-example[]
	void shouldRenderElementId() {
		var screen = Cypher.node("ScreenStateNode").named("screen");
		var id = Cypher.literalOf("4:d32903f5-48ef-40fb-9ce5-9a3039852c46:2");
		var statement = Cypher.match(screen)
			.where(Cypher.elementId(screen).eq(id))
			.returning(Cypher.elementId(screen))
			.build();
		// Config and renderer is thread safe, you can store it somewhere global
		var rendererConfig = Configuration.newConfig().withDialect(Dialect.NEO4J_5).build();
		var renderer = Renderer.getRenderer(rendererConfig);
		var cypher = renderer.render(statement);
		assertThat(cypher)
			.isEqualTo(
				"MATCH (screen:`ScreenStateNode`) " +
				"WHERE elementId(screen) = '4:d32903f5-48ef-40fb-9ce5-9a3039852c46:2' " +
				"RETURN elementId(screen)"
			);
	}
}
// end::dialect-example[]
