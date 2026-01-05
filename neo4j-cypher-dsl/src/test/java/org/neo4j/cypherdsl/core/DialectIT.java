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

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

/**
 * @author Michael J. Simons
 */

// tag::dialect-example[]
class DialectIT {

	// end::dialect-example[]

	static Stream<Arguments> nPropExists() {
		return Stream.of(Arguments.of(Dialect.NEO4J_4, false, "MATCH (n:`Movie`) WHERE exists(n.title) RETURN n"),
				Arguments.of(Dialect.NEO4J_4, true, "MATCH (n:`Movie`) WHERE NOT (exists(n.title)) RETURN n"),
				Arguments.of(Dialect.NEO4J_5, false, "MATCH (n:`Movie`) WHERE n.title IS NOT NULL RETURN n"),
				Arguments.of(Dialect.NEO4J_5, true, "MATCH (n:`Movie`) WHERE n.title IS NULL RETURN n"));
	}

	static Stream<Arguments> distanceFunction() {
		return Stream.of(Arguments.of(Dialect.NEO4J_4, "MATCH (n) RETURN distance(n.a, n.b)"),
				Arguments.of(Dialect.NEO4J_5, "MATCH (n) RETURN point.distance(n.a, n.b)"));
	}

	static Stream<Arguments> elementId() {
		return Stream.of(Arguments.of(Dialect.NEO4J_4, "MATCH (n) RETURN toString(id(n))"),
				Arguments.of(Dialect.NEO4J_5, "MATCH (n) RETURN elementId(n)"));
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
		String cypher = renderer.render(Cypher.match(n).where(condition).returning(n).build());
		assertThat(cypher).isEqualTo(expected);
	}

	@ParameterizedTest
	@MethodSource
	void distanceFunction(Dialect dialect, String expected) {

		Node n = Cypher.anyNode("n");
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
		String cypher = renderer
			.render(Cypher.match(n).returning(Cypher.distance(n.property("a"), n.property("b"))).build());
		assertThat(cypher).isEqualTo(expected);
	}

	@ParameterizedTest
	@MethodSource
	void elementId(Dialect dialect, String expected) {

		Node n = Cypher.anyNode("n");

		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
		String cypher = renderer.render(Cypher.match(n).returning(n.elementId()).build());
		assertThat(cypher).isEqualTo(expected);
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
			NEO4J_4,MATCH p = shortestPath((wos:`Station`)-[:`LINK`]->(bmv:`Station`)) RETURN p
			NEO4J_5,MATCH p = shortestPath((wos:`Station`)-[:`LINK`]->(bmv:`Station`)) RETURN p
			NEO4J_5_23,MATCH p = SHORTEST 1 (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_26,CYPHER 5 MATCH p = SHORTEST 1 (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_DEFAULT_CYPHER,MATCH p = SHORTEST 1 (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_CYPHER_5,CYPHER 5 MATCH p = SHORTEST 1 (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_CYPHER_25,CYPHER 25 MATCH p = SHORTEST 1 (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			""")
	void shortestPath(Dialect dialect, String expected) {

		var stmt = Cypher.match(Cypher.shortestK(1)
			.named("p")
			.definedBy(Cypher.node("Station").named("wos").relationshipTo(Cypher.node("Station").named("bmv"), "LINK")))
			.returning(Cypher.name("p"))
			.build();
		var renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
		assertThat(renderer.render(stmt)).isEqualTo(expected);
	}

	@ParameterizedTest
	@CsvSource(textBlock = """
			NEO4J_4,MATCH p = allShortestPaths((wos:`Station`)-[:`LINK`]->(bmv:`Station`)) RETURN p
			NEO4J_5,MATCH p = allShortestPaths((wos:`Station`)-[:`LINK`]->(bmv:`Station`)) RETURN p
			NEO4J_5_23,MATCH p = ALL SHORTEST (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_26,CYPHER 5 MATCH p = ALL SHORTEST (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_DEFAULT_CYPHER,MATCH p = ALL SHORTEST (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_CYPHER_5,CYPHER 5 MATCH p = ALL SHORTEST (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			NEO4J_5_CYPHER_25,CYPHER 25 MATCH p = ALL SHORTEST (wos:`Station`)-[:`LINK`]->(bmv:`Station`) RETURN p
			""")
	void allShortestPath(Dialect dialect, String expected) {
		var stmt = Cypher.match(Cypher.allShortest()
			.named("p")
			.definedBy(Cypher.node("Station").named("wos").relationshipTo(Cypher.node("Station").named("bmv"), "LINK")))
			.returning(Cypher.name("p"))
			.build();
		var renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(dialect).build());
		assertThat(renderer.render(stmt)).isEqualTo(expected);
	}

	@ParameterizedTest
	@CsvSource(quoteCharacter = '@',
			textBlock = """
					false, NEO4J_4,                MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					false, NEO4J_5,                MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					false, NEO4J_5_23,             MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					false, NEO4J_5_26,             @CYPHER 5 MATCH (m:`Movie`) SET m:$($x):$($y):$($z):$(['f', 'g']):$('h'):$(['i', 'j']):$($foo):$($bar) RETURN *@
					false, NEO4J_5_CYPHER_5,       @CYPHER 5 MATCH (m:`Movie`) SET m:$($x):$($y):$($z):$(['f', 'g']):$('h'):$(['i', 'j']):$($foo):$($bar) RETURN *@
					false, NEO4J_5_DEFAULT_CYPHER, @MATCH (m:`Movie`) SET m:$($x):$($y):$($z):$(['f', 'g']):$('h'):$(['i', 'j']):$($foo):$($bar) RETURN *@
					false, NEO4J_5_CYPHER_25,      @CYPHER 25 MATCH (m:`Movie`) SET m:$($x):$($y):$($z):$(['f', 'g']):$('h'):$(['i', 'j']):$($foo):$($bar) RETURN *@
					true, NEO4J_4,                 MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					true, NEO4J_5,                 MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					true, NEO4J_5_23,              MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					true, NEO4J_5_26,              CYPHER 5 MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					true, NEO4J_5_CYPHER_5,        CYPHER 5 MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					true, NEO4J_5_DEFAULT_CYPHER,  MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					true, NEO4J_5_CYPHER_25,       CYPHER 25 MATCH (m:`Movie`) SET m:`a`:`b`:`c`:`d`:`e`:`f`:`g`:`h`:`i`:`j`:`k`:`l`:`m` RETURN *
					""")
	void labelRenderingShouldWork(boolean disableDynamicLabels, Dialect dialect, String expected) {
		var m = Cypher.node(Cypher.exactlyLabel("Movie")).named("m");
		var parameterWithListOfStrings = Cypher.parameter("x",
				Cypher.listOf(Cypher.literalOf("a"), Cypher.literalOf("b")));
		var parameterWithASingleString = Cypher.parameter("y", Cypher.literalOf("c"));
		var parameterWithListLiteral = Cypher.parameter("z",
				Cypher.literalOf(List.of(Cypher.literalOf("d"), Cypher.literalOf("e"))));
		var parameterWithAListOfJavaStrings = Cypher.parameter("foo", List.of("k", "l"));

		var stmt = Cypher.match(m)
			.set(m, Labels.all(parameterWithListOfStrings)
				.conjunctionWith(Labels.all(parameterWithASingleString))
				.conjunctionWith(Labels.all(parameterWithListLiteral))
				.conjunctionWith(Labels.all(Cypher.listOf(Cypher.literalOf("f"), Cypher.literalOf("g"))))
				.conjunctionWith(Labels.all(Cypher.literalOf("h")))
				.conjunctionWith(Labels.all(Cypher.literalOf(List.of(Cypher.literalOf("i"), Cypher.literalOf("j")))))
				.conjunctionWith(Labels.all(parameterWithAListOfJavaStrings))
				.conjunctionWith(Labels.all(Cypher.parameter("bar", "m"))))
			.returning(Cypher.asterisk())
			.build();

		var renderer = Renderer.getRenderer(
				Configuration.newConfig().disableDynamicLabels(disableDynamicLabels).withDialect(dialect).build());
		assertThat(renderer.render(stmt)).isEqualTo(expected);
	}

	@Test
	void expressionsMightNotBeRenderedInLabels() {
		var m = Cypher.node("Movie").named("m");
		var stmt = Cypher.match(m).set(m, Labels.all(m.property("x"))).returning(Cypher.asterisk()).build();

		var renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(Dialect.NEO4J_4).build());
		assertThatIllegalArgumentException().isThrownBy(() -> renderer.render(stmt))
			.withMessage("Cannot render the given Labels in a Cypher pre 5.26 compatible way");
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
		assertThat(cypher).isEqualTo("MATCH (screen:`ScreenStateNode`) "
				+ "WHERE elementId(screen) = '4:d32903f5-48ef-40fb-9ce5-9a3039852c46:2' " + "RETURN elementId(screen)");
	}

}
// end::dialect-example[]
