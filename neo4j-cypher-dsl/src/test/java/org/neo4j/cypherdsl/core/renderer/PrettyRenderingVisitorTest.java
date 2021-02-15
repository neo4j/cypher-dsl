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
package org.neo4j.cypherdsl.core.renderer;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Statement;

import static org.assertj.core.api.Assertions.assertThat;


/**
 * @author Andreas Berger
 */
class PrettyRenderingVisitorTest {

	private final PrettyRenderingVisitor visitor = new PrettyRenderingVisitor();

	private static final Renderer cypherRenderer = Renderer.getPrettyRenderer();
	private final Node bikeNode = Cypher.node("Bike").named("b");
	private final Node userNode = Cypher.node("User").named("u");

	@ParameterizedTest
	@CsvSource({
			"ALabel, ALabel",
			"A Label, `A Label`",
			"A `Label, `A ``Label`",
			"`A `Label, ```A ``Label`",
			"Spring Data Neo4j⚡️RX, `Spring Data Neo4j⚡️RX`"
	})
	void shouldCorrectlyEscapeNames(String name, String expectedEscapedName) {

		assertThat(visitor.escapeName(name)).hasValue(expectedEscapedName);
	}

	@Test
	void shouldNotTryToEscapeNullNames() {

		assertThat(visitor.escapeName(null)).isEmpty();
	}

	@Test
	void shouldPrettyPrintStatement() {

		Node otherNode = Cypher.anyNode("other");
		Statement statement = Cypher.match(userNode)
				.where(userNode.property("name").isEqualTo(Cypher.literalOf("Max")))
				.and(userNode.property("lastName").isEqualTo(Cypher.literalOf("Mustermann")))
				.set(userNode.property("lastName").to(Cypher.parameter("newName")))
				.with(userNode)
				.match(this.bikeNode)
				.create(userNode.relationshipTo(this.bikeNode, "LIKES"))
				.returning(userNode.project(
						"name",
						userNode.property("name"),
						"nesting1",
						Cypher.mapOf(
								"name",
								userNode.property("name"),
								"nesting2",
								Cypher.mapOf(
										"name", this.bikeNode.property("name"),
										"pattern", Cypher
												.listBasedOn(userNode.relationshipTo(otherNode, "LIKES"))
												.where(otherNode.property("foo").isEqualTo(Cypher.parameter("foo")))
												.returning(otherNode.project("x", "y"))
								)
						))).build();

		assertThat(cypherRenderer.render(statement))
				.isEqualTo("MATCH (u:User)\n" +
						"WHERE (u.name = 'Max' \n" +
						"\tAND u.lastName = 'Mustermann') \n" +
						"SET u.lastName = $newName \n" +
						"WITH u \n" +
						"MATCH (b:Bike) \n" +
						"CREATE (u)-[:LIKES]->(b) \n" +
						"RETURN u {\n" +
						"\tname: u.name, \n" +
						"\tnesting1:  {\n" +
						"\t\tname: u.name, \n" +
						"\t\tnesting2:  {\n" +
						"\t\t\tname: b.name, \n" +
						"\t\t\tpattern: [(u)-[:LIKES]->(other) WHERE other.foo = $foo | other {\n" +
						"\t\t\t\t.x, \n" +
						"\t\t\t\t.y\n" +
						"\t\t\t}]\n" +
						"\t\t}\n" +
						"\t}\n" +
						"}");

	}
}
