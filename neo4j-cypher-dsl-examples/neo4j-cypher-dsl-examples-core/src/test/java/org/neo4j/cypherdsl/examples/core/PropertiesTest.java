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
package org.neo4j.cypherdsl.examples.core;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Map;
import java.util.regex.Pattern;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Functions;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 * @soundtrack Pearl Jam - Lightning Bolt
 */
class PropertiesTest {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Test
	void propertiesOnNodesAndRel() {

		// tag::properties-on-nodes-and-rel[]
		var personNode = Cypher.node("Person").named("p");
		var movieNode = Cypher.node("Movie");
		var ratedRel = personNode.relationshipTo(movieNode, "RATED").named("r");
		var statement = Cypher.match(ratedRel)
			.returning(
				personNode.property("name"), // <.>
				ratedRel.property("rating")) // <.>
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (p:`Person`)-[r:`RATED`]->(:`Movie`) RETURN p.name, r.rating");
		// end::properties-on-nodes-and-rel[]
	}

	@Test
	void propertiesOnNodesAndRelUnnamned() {

		// tag::properties-on-nodes-and-rel-unnamed[]
		var personNode = Cypher.node("Person");
		var movieNode = Cypher.node("Movie");
		var ratedRel = personNode.relationshipTo(movieNode, "RATED");
		var statement = Cypher.match(ratedRel)
			.returning(
				personNode.property("name"), // <.>
				ratedRel.property("rating")) // <.>
			.build();

		assertThat(cypherRenderer.render(statement)).matches(
			Pattern.quote("MATCH (") + "\\w+" + Pattern.quote(":`Person`)-[") + "\\w+" + Pattern
				.quote(":`RATED`]->(:`Movie`) RETURN ") + "\\w+" + Pattern.quote(".name, ") + "\\w+" + Pattern
				.quote(".rating"));
		// end::properties-on-nodes-and-rel-unnamed[]
	}

	@Test
	void propertiesOfExpressions() {

		// tag::properties-on-expressions[]
		var epochSeconds = Cypher.property(Functions.datetime(), "epochSeconds"); // <.>
		var statement = Cypher.returning(epochSeconds).build();
		Assertions.assertThat(cypherRenderer.render(statement))
			.isEqualTo("RETURN datetime().epochSeconds");
		// end::properties-on-expressions[]
	}

	@Test
	void nestedProperties() {

		// tag::nested-properties[]
		var node = Cypher.node("Person").named("p");

		var locationPropV1 = Cypher.property(node.getRequiredSymbolicName(), "home.location", "y");
		var locationPropV2 = Cypher.property("p", "home.location", "y");

		var statement = Cypher.match(node)
			.where(locationPropV1.gt(Cypher.literalOf(50)))
			.returning(locationPropV2).build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (p:`Person`) WHERE p.`home.location`.y > 50 RETURN p.`home.location`.y");
		// end::nested-properties[]
	}

	@Test
	void usingExistingJavaMaps() {

		var node = Cypher
			.node("ANode")
			.named("n")
			.withProperties(Map.of("aProperty", 23));

		assertThat(Cypher.match(node).returning(node).build().getCypher())
			.isEqualTo("MATCH (n:`ANode` {aProperty: 23}) RETURN n");
	}
}
