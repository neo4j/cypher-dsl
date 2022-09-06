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
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
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
	private static final Map<Neo4jContainer<?>, Driver> CACHED_CONNECTIONS = Collections.synchronizedMap(
		new HashMap<>());
	private static final String LATEST_VERSION = "4.4";
	private static final Map<String, String> CATEGORIES = new HashMap<String, String>() {
		private static final long serialVersionUID = -5617035309491104978L;

		{
			put("Q", "Quoting");
			put("U", "Unicode");
			put("B", "Backslashes");
			put("E", "Escaping");
		}
	};

	static class TestItem {

		private final String category;

		private final String description;

		private final String input;

		private final String expected;

		TestItem(String category, String description, String input, String expected) {
			this.category = category;
			this.description = description;
			this.input = input;
			this.expected = expected;
		}

		public String category() {
			return category;
		}

		public String description() {
			return description;
		}

		public String input() {
			return input;
		}

		public String expected() {
			return expected;
		}
	}

	List<TestItem> TEST_DATA = Arrays.asList(
		new TestItem("Q", "Simple label", "ABC", "ABC"),
		new TestItem("Q", "Simple label with non identifiers", "A Label", "A Label"),
		new TestItem("Q", "Simple label with backticks", "A` C", "A` C"),
		new TestItem("Q", "Escaped quotes", "A`` C", "A` C"),
		new TestItem("Q", "Backticks after blank ", "A `Label", "A `Label"),
		new TestItem("Q", "Non consecutive backticks", "`A `Label", "`A `Label"),
		new TestItem("U", "Single unicode, no quoting needed", "ᑖ", "ᑖ"),
		new TestItem("U", "Single multibyte unicode, quoting needed", "⚡️", "⚡️"),
		new TestItem("U", "Unicode pair inside label", "Spring Data Neo4j⚡️RX", "Spring Data Neo4j⚡️RX"),
		new TestItem("Q", "Single backtick", "`", "`"),
		new TestItem("Q", "Single unicode literal backtick", "\u0060", "`"),
		new TestItem("Q", "One escaped, one unescaped backtick", "```", "``"),
		new TestItem("Q", "One escaped, one unescaped unicode literal backtick", "\u0060\u0060\u0060", "``"),
		new TestItem("U", "One escaped, one unescaped Cypher unicode literal backtick", "\\u0060\\u0060\\u0060", "``"),
		new TestItem("Q", "Backtick at end", "Hello`", "Hello`"),
		new TestItem("Q", "Escaped backticks", "Hi````there", "Hi``there"),
		new TestItem("Q", "Mixed escaped and non escaped backticks", "Hi`````there", "Hi```there"),
		new TestItem("Q", "Even number of scattered backticks", "`a`b`c`", "`a`b`c`"),
		new TestItem("Q", "Even number of scattered backticks (unicode literals)", "\u0060a`b`c\u0060d\u0060", "`a`b`c`d`"),
		new TestItem("Q", "Even number of scattered backticks (Cypher unicode literals)", "\\u0060a`b`c\\u0060d\\u0060", "`a`b`c`d`"),
		new TestItem("B", "Escaped backslash followed by backtick", "Foo\\\\`bar", "Foo\\`bar"),
		new TestItem("U", "Escaped (invalid) unicode literal", "admin\\user", "admin\\user"),
		new TestItem("U", "Escaped (invalid) Cypher unicode literal", "admin\\\\user", "admin\\user"),
		new TestItem("U", "Cypher unicode literals", "\\u0075\\u1456", "uᑖ"),
		new TestItem("U", "Unicode literal", "\u1456", "ᑖ"),
		new TestItem("U", "Non recursive Cypher unicode literals", "something\\u005C\\u00751456", "something\\u1456"),
		new TestItem("U", "Unicode literals creating a backtick unicode", "\u005C\\u0060", "`"),
		new TestItem("U", "Unicode literals creating only the literal text of a a unicode literal", "\\u005Cu0060", "\\u0060"),
		new TestItem("U", "Cypher unicode literals creating unicode backtick literal", "\\u005C\\u0060", "\\`"),
		new TestItem("B", "Single backslash", "x\\y", "x\\y"),
		new TestItem("B", "Escaped single backslash", "x\\\\y", "x\\y"),
		new TestItem("B", "Escaped multiple backslash", "x\\\\\\\\y", "x\\\\y"),
		new TestItem("E", "Escaped backticks (Future Neo4j)", "x\\`y", "x`y"),
		new TestItem("E", "Escaped backticks (Future Neo4j)", "x\\```y", "x``y"),
		new TestItem("E", "Escaped backticks (Future Neo4j)", "x`\\```y", "x```y"),
		new TestItem("Q", "Unicode literal backtick at end", "Foo \u0060", "Foo `"),
		new TestItem("Q", "Cypher unicode literal backtick at end", "Foo \\u0060", "Foo `")
	);

	@TestFactory
	Stream<DynamicNode> shouldHaveConsistentResultsOnAllSupportedVersions() {
		return Stream.of("3.5", "4.0", "4.1", "4.2", "4.3", "4.4")
			.map(version -> {
				@SuppressWarnings("resource")
				Neo4jContainer<?> neo4j = new Neo4jContainer<>("neo4j:" + version).withReuse(true);
				neo4j.start();

				String[] majorMinor = version.split("\\.");
				int major;
				int minor;
				// Latest supported must work without config
				if (LATEST_VERSION.equals(version)) {
					major = -1;
					minor = -1;
				} else {
					major = Integer.parseInt(majorMinor[0]);
					minor = Integer.parseInt(majorMinor[1]);
				}

				Stream<DynamicNode> categories = TEST_DATA.stream().collect(Collectors.groupingBy(TestItem::category))
					.entrySet().stream().map(entry -> {
						Stream<DynamicTest> nested = entry.getValue().stream().map(item ->
							DynamicTest.dynamicTest(item.description() + " (" + item.input + ")", () -> {
								String schemaName = SchemaNames.sanitize(item.input(), false, major, minor).orElseThrow(NoSuchElementException::new);
								Driver driver = CACHED_CONNECTIONS.computeIfAbsent(neo4j, SchemaNamesIT::newDriverInstance);
								try (Session session = driver.session();) {
									Result result = session.run(String.format("CREATE (n:%s) RETURN n", schemaName));
									Node node = result.single().get(0).asNode();
									assertThat(node.labels()).containsExactly(item.expected());
									ResultSummary summary = result.consume();
									assertThat(summary.counters().nodesCreated()).isOne();
								}
							}));
						return DynamicContainer.dynamicContainer(CATEGORIES.getOrDefault(entry.getKey(), "Misc"), nested);
					});
				return DynamicContainer.dynamicContainer(version, categories);
			});
	}

	private static Driver newDriverInstance(Neo4jContainer<?> server) {
		return GraphDatabase.driver(server.getBoltUrl(), AuthTokens.basic("neo4j", server.getAdminPassword()), DRIVER_CONFIG);
	}

	@AfterAll
	static void closeConnections() {
		CACHED_CONNECTIONS.values().forEach(Driver::close);
	}
}
