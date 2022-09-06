/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
package org.neo4j.cypherdsl.support.schema_name;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.DynamicContainer;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Config;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.neo4j.driver.Result;
import org.neo4j.driver.Session;
import org.neo4j.driver.summary.ResultSummary;
import org.neo4j.driver.types.Node;
import org.testcontainers.containers.Neo4jContainer;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * @author Michael J. Simons
 */
@Testcontainers(disabledWithoutDocker = true)
class SchemaNamesIT {

	private static final Config DRIVER_CONFIG = Config.builder().withMaxConnectionPoolSize(1).build();
	private static Map<Neo4jContainer<?>, Driver> CACHED_CONNECTIONS = Collections.synchronizedMap(new HashMap<>());
	private static String LATEST_VERSION = "4.4";

	String[][] TEST_DATA = new String[][] {
		{ "ABC", "ABC" },
		{ "A C", "A C" },
		{ "A` C", "A` C" },
		{ "A`` C", "A` C" },
		{ "ALabel", "ALabel" },
		{ "A Label", "A Label" },
		{ "A `Label", "A `Label" },
		{ "`A `Label", "`A `Label" },
		{ "Spring Data Neo4j⚡️RX", "Spring Data Neo4j⚡️RX" },
		{ "`", "`" },
		{ "\u0060", "`" },
		{ "```", "``" },
		{ "\u0060\u0060\u0060", "``" },
		{ "\\u0060\\u0060\\u0060", "``" },
		{ "Hello`", "Hello`" },
		{ "Hi````there", "Hi``there" },
		{ "Hi`````there", "Hi```there" },
		{ "`a`b`c`", "`a`b`c`" },
		{ "\u0060a`b`c\u0060d\u0060", "`a`b`c`d`" },
		{ "\\u0060a`b`c\\u0060d\\u0060", "`a`b`c`d`" },
		{ "Foo\\\\`bar", "Foo\\`bar" },
		// Escape the unicode literal for java
		{ "admin\\user", "admin\\user" },
		// Escape the unicode literal for cypher
		{ "admin\\\\user", "admin\\user" },
		// The unicode escaped
		{ "\\u0075\\u1456", "uᑖ" },
		// Not supported, "Potential additional rules mentioned in the GQL standard that would be potentially worth considering could take over:"
		// new TestInput("\\u000151", "ő", "ő"),
		{ "\u1456", "ᑖ" },
		{ "something\\u005C\\u00751456", "something\\u1456" },
		// Similar to the above, but creating backtick
		// First is literal unicode backslash, than u0060, will turn into \\u00160, turned into a backtick and quoted
		{ "\u005C\\u0060", "`"},
		// First is escaped unicode backslash, than u0060 but the CIP says only one iteration of resolving
		{ "\\u005Cu0060", "\\u0060"},
		// Escaped literals for the backslash and the backtick following each other
		{ "\\u005C\\u0060", "\\`"},
		{ "x\\y", "x\\y" },
		// Already escaped backslash
		{ "x\\\\y", "x\\y" },
		// Escaped backticks
		{ "x\\`y", "x`y" },
		{ "x\\```y", "x``y" },
		{ "x`\\```y", "x```y" },
		// This is the backtick itself in the string
		{ "Foo \u0060", "Foo `" },
		// This is the backtick unicode escaped so that without further processing `foo \u0060` would end up at Cypher,
		{ "Foo \\u0060", "Foo `" },
	};

	@TestFactory
	Stream<DynamicNode> shouldHaveConsistentResultsOnAllSupportedVersions() {
		return Stream.of("3.5", "4.0", "4.1", "4.2", "4.3", "4.4")
			.map(version -> {
				@SuppressWarnings("resource")
				Neo4jContainer<?> neo4j = new Neo4jContainer<>("neo4j:" + version).withReuse(true);
				neo4j.start();

				Stream<DynamicTest> dynamicTestStream = Arrays.stream(TEST_DATA)
					.map(v -> {
						String[] majorMinor = version.split("\\.");
						int major = -1;
						int minor = -1;
						// Latest supported must work without config
						if (!LATEST_VERSION.equals(version)) {
							major = Integer.parseInt(majorMinor[0]);
							minor = Integer.parseInt(majorMinor[1]);
						}
						String schemaName = SchemaNames.sanitize(v[0], false, major, minor).orElseThrow(NoSuchElementException::new);
						return DynamicTest.dynamicTest(schemaName, () -> {
								Driver driver = CACHED_CONNECTIONS.computeIfAbsent(neo4j,
									server -> GraphDatabase.driver(server.getBoltUrl(),
										AuthTokens.basic("neo4j", neo4j.getAdminPassword()), DRIVER_CONFIG));
								try (Session session = driver.session();) {
									Result result = session.run(String.format("CREATE (n:%s) RETURN n", schemaName));
									Node node = result.single().get(0).asNode();
									assertThat(node.labels()).containsExactly(v[1]);
									ResultSummary summary = result.consume();
									assertThat(summary.counters().nodesCreated()).isOne();
								}
							}
						);
					});
				return DynamicContainer.dynamicContainer(version, dynamicTestStream);
			});
	}

	@AfterAll
	static void closeConnections() {
		CACHED_CONNECTIONS.values().forEach(Driver::close);
	}
}
