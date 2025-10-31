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
package org.neo4j.cypherdsl.support.schema_name;

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
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.neo4j.Neo4jContainer;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author Michael J. Simons
 */
@Testcontainers(disabledWithoutDocker = true)
class SchemaNamesIT {

	private static final Config DRIVER_CONFIG = Config.builder().withMaxConnectionPoolSize(1).build();

	private static final Map<Neo4jContainer, Driver> CACHED_CONNECTIONS = Collections.synchronizedMap(new HashMap<>());

	private static final String LATEST_VERSION = "4.4";

	private static final List<TestItem> TEST_DATA = Arrays.asList(
			new TestItem(Category.Q, "Simple label", "ABC", "ABC"),
			new TestItem(Category.Q, "Simple label with non identifiers", "A Label", "A Label"),
			new TestItem(Category.Q, "Simple label with backticks", "A` C", "A` C"),
			new TestItem(Category.Q, "Escaped quotes", "A`` C", "A` C"),
			new TestItem(Category.Q, "Backticks after blank ", "A `Label", "A `Label"),
			new TestItem(Category.Q, "Non consecutive backticks", "`A `Label", "`A `Label"),
			new TestItem(Category.U, "Single unicode, no quoting needed", "ᑖ", "ᑖ"),
			new TestItem(Category.U, "Single multibyte unicode, quoting needed", "⚡️", "⚡️"),
			new TestItem(Category.U, "Unicode pair inside label", "Spring Data Neo4j⚡️RX", "Spring Data Neo4j⚡️RX"),
			new TestItem(Category.Q, "Single backtick", "`", "`"),
			new TestItem(Category.Q, "Single unicode literal backtick", "\u0060", "`"),
			new TestItem(Category.Q, "One escaped, one unescaped backtick", "```", "``"),
			new TestItem(Category.Q, "One escaped, one unescaped unicode literal backtick", "\u0060\u0060\u0060", "``"),
			new TestItem(Category.U, "One escaped, one unescaped Cypher unicode literal backtick",
					"\\u0060\\u0060\\u0060", "``"),
			new TestItem(Category.Q, "Backtick at end", "Hello`", "Hello`"),
			new TestItem(Category.Q, "Escaped backticks", "Hi````there", "Hi``there"),
			new TestItem(Category.Q, "Mixed escaped and non escaped backticks", "Hi`````there", "Hi```there"),
			new TestItem(Category.Q, "Even number of scattered backticks", "`a`b`c`", "`a`b`c`"),
			new TestItem(Category.Q, "Even number of scattered backticks (unicode literals)",
					"\u0060a`b`c\u0060d\u0060", "`a`b`c`d`"),
			new TestItem(Category.Q, "Even number of scattered backticks (Cypher unicode literals)",
					"\\u0060a`b`c\\u0060d\\u0060", "`a`b`c`d`"),
			new TestItem(Category.B, "Escaped backslash followed by backtick", "Foo\\\\`bar", "Foo\\`bar"),
			new TestItem(Category.U, "Escaped (invalid) unicode literal", "admin\\user", "admin\\user"),
			new TestItem(Category.U, "Escaped (invalid) Cypher unicode literal", "admin\\\\user", "admin\\user"),
			new TestItem(Category.U, "Cypher unicode literals", "\\u0075\\u1456", "uᑖ"),
			new TestItem(Category.U, "Unicode literal", "\u1456", "ᑖ"),
			new TestItem(Category.U, "Non recursive Cypher unicode literals", "something\\u005C\\u00751456",
					"something\\u1456"),
			new TestItem(Category.U, "Unicode literals creating a backtick unicode", "\u005C\\u0060", "`"),
			new TestItem(Category.U, "Unicode literals creating only the literal text of a a unicode literal",
					"\\u005Cu0060", "\\u0060"),
			new TestItem(Category.U, "Cypher unicode literals creating unicode backtick literal", "\\u005C\\u0060",
					"\\`"),
			new TestItem(Category.B, "Single backslash", "x\\y", "x\\y"),
			new TestItem(Category.B, "Escaped single backslash", "x\\\\y", "x\\y"),
			new TestItem(Category.B, "Escaped multiple backslash", "x\\\\\\\\y", "x\\\\y"),
			new TestItem(Category.E, "Escaped backticks (Future Neo4j)", "x\\`y", "x`y"),
			new TestItem(Category.E, "Escaped backticks (Future Neo4j)", "x\\```y", "x``y"),
			new TestItem(Category.E, "Escaped backticks (Future Neo4j)", "x`\\```y", "x```y"),
			new TestItem(Category.Q, "Unicode literal backtick at end", "Foo \u0060", "Foo `"),
			new TestItem(Category.Q, "Cypher unicode literal backtick at end", "Foo \\u0060", "Foo `"));

	private static Driver newDriverInstance(Neo4jContainer server) {
		return GraphDatabase.driver(server.getBoltUrl(), AuthTokens.basic("neo4j", server.getAdminPassword()),
				DRIVER_CONFIG);
	}

	@AfterAll
	static void closeConnections() {
		CACHED_CONNECTIONS.values().forEach(Driver::close);
	}

	@TestFactory
	Stream<DynamicNode> shouldHaveConsistentResultsOnAllSupportedVersions() {
		Stream<String> versions;
		if (Boolean.getBoolean("SCHEMA_NAMES_TEST_ALL_VERSIONS")) {
			versions = Stream.of("3.5", "4.0", "4.1", "4.2", "4.3", LATEST_VERSION);
		}
		else {
			versions = Stream.of(LATEST_VERSION);
		}
		return versions.map(version -> {
			@SuppressWarnings("resource")
			Neo4jContainer neo4j = new Neo4jContainer("neo4j:" + version).withReuse(true);
			neo4j.start();

			String[] majorMinor = version.split("\\.");
			int major;
			int minor;
			// Latest supported must work without config
			if (SchemaNamesIT.LATEST_VERSION.equals(version)) {
				major = -1;
				minor = -1;
			}
			else {
				major = Integer.parseInt(majorMinor[0]);
				minor = Integer.parseInt(majorMinor[1]);
			}

			Stream<DynamicNode> categories = TEST_DATA.stream()
				.collect(Collectors.groupingBy(TestItem::category))
				.entrySet()
				.stream()
				.map(entry -> {
					Stream<DynamicTest> nested = entry.getValue()
						.stream()
						.map(item -> DynamicTest.dynamicTest(item.description() + " (" + item.input + ")", () -> {
							String schemaName = SchemaNames.sanitize(item.input(), false, major, minor)
								.orElseThrow(NoSuchElementException::new);
							Driver driver = CACHED_CONNECTIONS.computeIfAbsent(neo4j, SchemaNamesIT::newDriverInstance);
							try (Session session = driver.session()) {
								Result result = session.run(String.format("CREATE (n:%s) RETURN n", schemaName));
								Node node = result.single().get(0).asNode();
								assertThat(node.labels()).containsExactly(item.expected());
								ResultSummary summary = result.consume();
								assertThat(summary.counters().nodesCreated()).isOne();
							}
						}));
					return DynamicContainer.dynamicContainer(entry.getKey().value, nested);
				});
			return DynamicContainer.dynamicContainer(version, categories);
		});
	}

	enum Category {

		Q("Quoting"), U("Unicode"), B("Backslashes"), E("Escaping");

		private final String value;

		Category(String value) {
			this.value = value;
		}

	}

	record TestItem(Category category, String description, String input, String expected) {
	}

}
