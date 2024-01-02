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
package org.neo4j.cypherdsl.examples.drivers;

import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.testcontainers.containers.Neo4jContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * @author Michael J. Simons
 */
@Testcontainers(disabledWithoutDocker = true)
class GeneratedConditionsIT {

	@SuppressWarnings("resource")
	@Container
	private static final Neo4jContainer<?> neo4j = new Neo4jContainer<>("neo4j:4.4")
		.withReuse(true);

	private static Driver driver;

	@BeforeAll
	static void openDriver() {

		neo4j.start();
		driver = GraphDatabase.driver(neo4j.getBoltUrl(), AuthTokens.basic("neo4j", neo4j.getAdminPassword()));
	}

	@AfterAll
	static void closeDriver() {
		driver.close();
	}

	@Test
	void includeAllShouldWork() {

		var node = Cypher.node("IncludeAllNode").named("testNode");
		var query = Cypher.match(node)
			.where(node.property("listProp").includesAll(Cypher.literalOf(List.of("string 1", "string 2"))))
			.returning(node)
			.build().getCypher();

		try {
			driver.executableQuery("""
				CREATE (n1:IncludeAllNode {listProp: ["string 1", "string 2", "string 3"], name: "n1"})
				CREATE (n2:IncludeAllNode {listProp: ["string 1", "string 3"], name: "n2"})
				RETURN [n1, n2] AS nodes
				"""
			).execute();
			var nodes = driver.executableQuery(query)
					.execute().records().stream().map(r -> r.get("testNode").asNode().get("name").asString());
			Assertions.assertThat(nodes).containsExactly("n1");
		} finally {
			driver.executableQuery("MATCH (n:IncludeAllNode) DELETE n").execute();
		}
	}

	@Test
	void includeAnyShouldWork() {

		var node = Cypher.node("IncludeAnyNode").named("testNode");
		var query = Cypher.match(node)
			.where(node.property("listProp").includesAny(Cypher.literalOf(List.of("string 1"))))
			.returning(node)
			.orderBy(node.property("name"))
			.build().getCypher();

		try {
			driver.executableQuery("""
				CREATE (n1:IncludeAnyNode {listProp: ["string 1", "string 2", "string 3"], name: "n1"})
				CREATE (n2:IncludeAnyNode {listProp: ["string 1", "string 3"], name: "n2"})
				CREATE (n3:IncludeAnyNode {listProp: ["string 4"], name: "n3"})
				RETURN [n1, n2, n3] AS nodes
				"""
			).execute();
			var nodes = driver.executableQuery(query)
				.execute().records().stream().map(r -> r.get("testNode").asNode().get("name").asString());
			Assertions.assertThat(nodes).containsExactly("n1", "n2");
		} finally {
			driver.executableQuery("MATCH (n:IncludeAnyNode) DELETE n").execute();
		}
	}
}
