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
package org.neo4j.cypherdsl.parser;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.asciidoctor.Asciidoctor;
import org.asciidoctor.Options;
import org.asciidoctor.ast.Block;
import org.asciidoctor.ast.Document;
import org.asciidoctor.extension.Treeprocessor;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DynamicContainer;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.TestInstance;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

/**
 * This tests reads and parses the README.adoc and verifies the defined content.
 *
 * @author Michael J. Simons
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class TckTests {

	private final Map<String, TestData> testData = new HashMap<>();

	@BeforeAll
	void init() throws IOException, URISyntaxException {

		var asciidoctor = Asciidoctor.Factory.create();
		var collector = new TestDataExtractor();
		asciidoctor.javaExtensionRegistry().treeprocessor(collector);

		var content = Files.readString(Paths.get(getClass().getResource("/README.adoc").toURI()));
		asciidoctor.load(content, Options.builder().build());

		this.testData.putAll(collector.content);
	}

	@TestFactory
	Stream<DynamicNode> tck() {

		return this.testData.entrySet().stream().map(entry -> {
			try {
				Stream<DynamicNode> entries;

				var type = entry.getKey().split("-")[0];
				var method = this.getClass()
					.getDeclaredMethod(type + "ShouldWork", Renderer.class, String.class, String.class);

				var renderer = Renderer
					.getRenderer(Configuration.newConfig().alwaysEscapeNames(entry.getValue().alwaysEscape).build());

				entries = entry.getValue()
					.asArguments()
					.map(arguments -> DynamicTest.dynamicTest(arguments.getKey(), () -> {
						try {
							method.invoke(this, renderer, arguments.getKey(), arguments.getValue());
						}
						catch (InvocationTargetException ex) {
							throw ex.getCause();
						}
					}));

				var name = type;
				if (!entry.getValue().alwaysEscape) {
					name += " (Only escaping when necessary)";
				}
				return DynamicContainer.dynamicContainer(name, entries);
			}
			catch (NoSuchMethodException ex) {
				throw new RuntimeException(ex);
			}
		});
	}

	@SuppressWarnings("unused")
	void nodesShouldWork(Renderer renderer, String input, String expected) {

		var node = CypherParser.parseNode(input);
		assertThat(renderer.render(Cypher.match(node).returning(Cypher.asterisk()).build()))
			.isEqualTo(String.format("MATCH %s RETURN *", expected));
	}

	@SuppressWarnings("unused")
	void expressionsShouldWork(Renderer renderer, String input, String expected) {

		var e = CypherParser.parseExpression(input);
		assertThat(renderer.render(Cypher.returning(e).build())).isEqualTo(String.format("RETURN %s", expected));
	}

	@SuppressWarnings("unused")
	void clausesShouldWork(Renderer renderer, String input, String expected) {

		var cypher = renderer.render(Statement.of(List.of(CypherParser.parseClause(input))));
		Assertions.assertThat(cypher).isEqualTo(expected);
	}

	@SuppressWarnings("unused")
	void statementsShouldWork(Renderer renderer, String input, String expected) {

		var cypher = renderer.render(CypherParser.parseStatement(input));
		assertThat(cypher).isEqualTo(expected);
	}

	@SuppressWarnings("unused")
	void qppShouldWork(Renderer renderer, String input, String expected) {

		var cypher = renderer.render(CypherParser.parseStatement(input));
		assertThat(cypher).isEqualTo(expected.replaceAll("\\s{2,}|\n", " "));
	}

	@SuppressWarnings("unused")
	void unsupportedShouldWork(Renderer renderer, String input, String expected) {

		assertThatExceptionOfType(UnsupportedCypherException.class).isThrownBy(() -> CypherParser.parse(input))
			.withMessageStartingWith("You used one Cypher construct not yet supported by the Cypher-DSL")
			.withMessageContaining(input);
	}

	private static class TestDataExtractor extends Treeprocessor {

		private final Map<String, TestData> content = new HashMap<>();

		TestDataExtractor() {
			super(new HashMap<>()); // Must be mutable
		}

		@Override
		public Document process(Document document) {
			document.findBy(Map.of("context", ":listing", "style", "source"))
				.stream()
				.map(Block.class::cast)
				.filter(b -> "cypher".equals(b.getAttribute("language")))
				.forEach(block -> {
					var id = block.getId().split("-");
					var type = id[0];
					var alwaysEscape = !block.hasAttribute("alwaysEscape")
							|| Boolean.parseBoolean((String) block.getAttribute("alwaysEscape"));
					type = type + "-" + alwaysEscape;
					var test = this.content.computeIfAbsent(type, key -> new TestData(alwaysEscape));
					var separated = block.getAttribute("separated");
					var lines = Boolean.parseBoolean((String) separated) ? Arrays.asList(block.getSource().split(";"))
							: block.getLines();
					switch (id[1]) {
						case "input" -> test.input.addAll(lines);
						case "output" -> test.expected.addAll(lines);
					}
				});

			return document;
		}

	}

	private static class TestData {

		private final boolean alwaysEscape;

		private final List<String> input = new ArrayList<>();

		private final List<String> expected = new ArrayList<>();

		TestData(boolean alwaysEscape) {
			this.alwaysEscape = alwaysEscape;
		}

		Stream<Map.Entry<String, String>> asArguments() {
			if (this.input.size() == this.expected.size()) {
				var result = Stream.<Map.Entry<String, String>>builder();
				for (int i = 0; i < this.input.size(); i++) {
					result.add(new AbstractMap.SimpleImmutableEntry<>(this.input.get(i).trim(),
							this.expected.get(i).trim()));
				}
				return result.build();
			}
			else {
				return this.input.stream().collect(Collectors.toMap(Function.identity(), k -> "")).entrySet().stream();
			}
		}

	}

}
