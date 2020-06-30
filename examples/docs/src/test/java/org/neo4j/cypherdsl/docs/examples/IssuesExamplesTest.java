/*
 * Copyright (c) 2019-2020 "Neo4j,"
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
package org.neo4j.cypherdsl.docs.examples;

import static org.assertj.core.api.Assertions.*;
import static org.neo4j.cypherdsl.Cypher.*;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.Cypher;
import org.neo4j.cypherdsl.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class IssuesExamplesTest {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Test
	void gh48() {
		var n = node("Label").named("n");
		var statement = Cypher.match(n)
			.set(n, mapOf("a", literalOf("bar"), "b", literalOf("baz")))
			.returning(n)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (n:`Label`) SET n = {a: 'bar', b: 'baz'} RETURN n");
	}

	@Test
	void gh51() {

		var n = anyNode("n");
		var foobarProp = property("n", "foobar");
		var statement = Cypher.match(n)
			.where(foobarProp.contains(literalOf("baz")))
			.or(foobarProp.startsWith(literalOf("a")))
			.or(foobarProp.endsWith(literalOf("b")))
			.returning(n)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (n) WHERE (n.foobar CONTAINS 'baz' OR n.foobar STARTS WITH 'a' OR n.foobar ENDS WITH 'b') RETURN n");
	}

	@Test
	void gh59() {
		var change = node("Change").named("change");
		var code = anyNode().named("code");
		var codeRelation = change.relationshipBetween(code, "CODE").named("codeRelation");
		var changeDetail = anyNode().named("changeDetail");
		var changeDetailsRelation = change.relationshipBetween(changeDetail, "CHANGE_DETAILS")
			.named("changeDetailsRelation");

		var idIsEqualTo147 = code.internalId().isEqualTo(literalOf(147));
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

		var src = anyNode().withProperties("id", literalOf(10));
		var n = anyNode().named("n");
		var statement = Cypher.match(src.relationshipTo(n, "LINKS_WITH").named("r"))
			.where(literalOf("France").in(property("r", "markets")))
			.returning(n)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH ( {id: 10})-[r:`LINKS_WITH`]->(n) WHERE 'France' IN r.markets RETURN n");
	}
}
