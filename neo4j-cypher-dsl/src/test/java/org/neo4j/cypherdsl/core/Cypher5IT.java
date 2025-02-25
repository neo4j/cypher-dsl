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
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * Tests around GH-1153
 */
class Cypher5IT {

	private final Renderer renderer = Renderer.getRenderer(Configuration.newConfig()
		.alwaysEscapeNames(false)
		.withDialect(Dialect.NEO4J_5_26).build());

	@Test
	void cypher5PrefixOnSimpleStatements() {
		var n = Cypher.node("Movie").named("n");
		var stmt = Cypher.match(n).returning(n).build();
		assertThat(renderer.render(stmt)).isEqualTo("CYPHER 5 MATCH (n:Movie) RETURN n");
	}

	@Test
	void unionShouldNotHaveDoublePrefixes() {
		Statement statement1 = Cypher.match(CypherIT.BIKE_NODE)
			.where(CypherIT.BIKE_NODE.property("a").isEqualTo(Cypher.literalOf("A")))
			.returning(CypherIT.BIKE_NODE)
			.build();

		Statement statement2 = Cypher.match(CypherIT.BIKE_NODE)
			.where(CypherIT.BIKE_NODE.property("b").isEqualTo(Cypher.literalOf("B")))
			.returning(CypherIT.BIKE_NODE)
			.build();

		Statement statement3 = Cypher.match(CypherIT.BIKE_NODE)
			.where(CypherIT.BIKE_NODE.property("c").isEqualTo(Cypher.literalOf("C")))
			.returning(CypherIT.BIKE_NODE)
			.build();
		Statement statement;
		statement = Cypher.union(statement1, statement2, statement3);

		assertThat(renderer.render(statement))
			.isEqualTo(
				"CYPHER 5 MATCH (b:Bike) WHERE b.a = 'A' RETURN b UNION MATCH (b:Bike) WHERE b.b = 'B' RETURN b UNION MATCH (b:Bike) WHERE b.c = 'C' RETURN b");
	}

	@Test
	void useShouldWork() {

		var statement = Cypher.match(Cypher.anyNode("n")).returning("n").build();
		var cypher = renderer.render(Cypher.use("neo4j", statement));
		assertThat(cypher).isEqualTo("CYPHER 5 USE neo4j MATCH (n) RETURN n");
	}

	@Test
	void explainShouldWork() {

		var statement = Cypher.match(Cypher.anyNode("n")).returning("n").build();
		var cypher = renderer.render(DecoratedQuery.explain(statement));
		assertThat(cypher).isEqualTo("CYPHER 5 EXPLAIN MATCH (n) RETURN n");
	}
}
