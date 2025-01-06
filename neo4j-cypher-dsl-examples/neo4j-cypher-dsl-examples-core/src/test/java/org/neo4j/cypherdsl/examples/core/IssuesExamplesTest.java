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
package org.neo4j.cypherdsl.examples.core;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class IssuesExamplesTest {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Test
	void gh48() {
		var n = Cypher.node("Label").named("n");
		var statement = Cypher.match(n)
			.set(n, Cypher.mapOf("a", Cypher.literalOf("bar"), "b", Cypher.literalOf("baz")))
			.returning(n)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (n:`Label`) SET n = {a: 'bar', b: 'baz'} RETURN n");
	}

	@Test
	void gh51() {

		var n = Cypher.anyNode("n");
		var foobarProp = Cypher.property("n", "foobar");
		var statement = Cypher.match(n)
			.where(foobarProp.contains(Cypher.literalOf("baz")))
			.or(foobarProp.startsWith(Cypher.literalOf("a")))
			.or(foobarProp.endsWith(Cypher.literalOf("b")))
			.returning(n)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (n) WHERE (n.foobar CONTAINS 'baz' OR n.foobar STARTS WITH 'a' OR n.foobar ENDS WITH 'b') RETURN n");
	}

	@SuppressWarnings("deprecation")
	@Test
	void gh59() {
		var change = Cypher.node("Change").named("change");
		var code = Cypher.anyNode().named("code");
		var codeRelation = change.relationshipBetween(code, "CODE").named("codeRelation");
		var changeDetail = Cypher.anyNode().named("changeDetail");
		var changeDetailsRelation = change.relationshipBetween(changeDetail, "CHANGE_DETAILS")
			.named("changeDetailsRelation");

		var idIsEqualTo147 = code.internalId().isEqualTo(Cypher.literalOf(147));
		var statement = Cypher.match(codeRelation)
			.where(idIsEqualTo147)
			.optionalMatch(changeDetailsRelation)
			.where(idIsEqualTo147)
			.returning(change, codeRelation, changeDetailsRelation, changeDetail)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (change:`Change`)-[codeRelation:`CODE`]-(code) "
					+ "WHERE id(code) = 147 "
					+ "OPTIONAL MATCH (change)-[changeDetailsRelation:`CHANGE_DETAILS`]-(changeDetail) "
					+ "WHERE id(code) = 147 RETURN change, codeRelation, changeDetailsRelation, changeDetail");
	}

	@Test
	void gh60() {

		var src = Cypher.anyNode().withProperties("id", Cypher.literalOf(10));
		var n = Cypher.anyNode().named("n");
		var statement = Cypher.match(src.relationshipTo(n, "LINKS_WITH").named("r"))
			.where(Cypher.literalOf("France").in(Cypher.property("r", "markets")))
			.returning(n)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH ( {id: 10})-[r:`LINKS_WITH`]->(n) WHERE 'France' IN r.markets RETURN n");
	}
}
