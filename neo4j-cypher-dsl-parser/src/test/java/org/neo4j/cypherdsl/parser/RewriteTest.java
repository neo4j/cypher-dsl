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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * @author Michael J. Simons
 */
class RewriteTest {

	// tag::enforcing-labels-function[]
	final BiFunction<LabelParsedEventType, Collection<String>, Collection<String>> makeSureALabelIsPresent = (e, c) -> {

		var finalLabels = new LinkedHashSet<>(c);
		switch (e) { // <.>
			case ON_NODE_PATTERN:
				finalLabels.add("ForcedLabel");
				return finalLabels;
			case ON_SET:
				finalLabels.add("Modified");
				return finalLabels;
			case ON_REMOVE:
				finalLabels.remove("ForcedLabel");
				return finalLabels;
			default:
				return c;
		}
	};
	// end::enforcing-labels-function[]

	@Test
	@SuppressWarnings("squid:S5976") // About making it a parameterized test. Used in the docs.
	void shouldRewriteLabelsOnParseNode() {

		// tag::enforcing-on-parse[]
		var options =
			Options.newOptions().withLabelFilter(makeSureALabelIsPresent).build();
		var statement = CypherParser
			.parseStatement("MATCH (n:Movie) RETURN n", options)
			.getCypher();

		assertThat(statement).isEqualTo("MATCH (n:`Movie`:`ForcedLabel`) RETURN n");
		// end::enforcing-on-parse[]
	}

	@Test
	@SuppressWarnings("squid:S5976") // Used in the docs
	void shouldRewriteLabelsOnSetLabels() {

		// tag::enforcing-on-set[]
		var options =
			Options.newOptions().withLabelFilter(makeSureALabelIsPresent).build();
		var statement = CypherParser
			.parseStatement("MATCH (n:Movie) SET n:`Comedy` RETURN n", options)
			.getCypher();

		assertThat(statement).isEqualTo("MATCH (n:`Movie`:`ForcedLabel`) SET n:`Comedy`:`Modified` RETURN n");
		// end::enforcing-on-set[]
	}

	@Test
	@SuppressWarnings("squid:S5976") // Used in the docs
	void shouldRewriteLabelsOnRemoveLabels() {

		// tag::enforcing-on-remove[]
		var options = Options.newOptions().withLabelFilter(makeSureALabelIsPresent).build();
		var statement = CypherParser
			.parseStatement("MATCH (n:Movie) REMOVE n:`Comedy`:`ForcedLabel` RETURN n", options)
			.getCypher();

		assertThat(statement).isEqualTo("MATCH (n:`Movie`:`ForcedLabel`) REMOVE n:`Comedy` RETURN n");
		// end::enforcing-on-remove[]
	}

	@Test
	void shouldRewriteTypes() {

		var statement = CypherParser
			.parseStatement("MATCH (p:Person) -[:HAT_GESPIELT_IN] -> (n:Movie) RETURN n",
				Options.newOptions()
					.withTypeFilter((e, t) -> t.size() == 1 && t.contains("HAT_GESPIELT_IN") ? Set.of("ACTED_IN") : t)
					.build())
			.getCypher();
		assertThat(statement).isEqualTo("MATCH (p:`Person`)-[:`ACTED_IN`]->(n:`Movie`) RETURN n");
	}

	@Test
	void shouldRewriteVariables() {

		var p = Cypher.node("Person").named("p");
		var parserOptions = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_NEW_VARIABLE, Expression.class, e -> p.property(((SymbolicName) e).getValue()))
			.build();
		var statement = Cypher
			.match(p)
			.where(CypherParser.parseExpression("name = 'Foobar'", parserOptions).asCondition()).returning(p).build().getCypher();
		assertThat(statement).isEqualTo("MATCH (p:`Person`) WHERE p.name = 'Foobar' RETURN p");
	}

	@Test
	void shouldRewriteParameters() {
		var counter = new AtomicInteger(1);
		var parserOptions = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_NEW_PARAMETER, Expression.class, e -> Cypher.parameter(String.format("%d", counter.getAndIncrement())))
			.build();
		var statement = CypherParser.parse("CREATE (p:Person {name: $name, height: $height, birthDate: $birthDate, templateEmail: 'Welcome $name!'})", parserOptions).getCypher();
		assertThat(statement).isEqualTo("CREATE (p:`Person` {name: $1, height: $2, birthDate: $3, templateEmail: 'Welcome $name!'})");
	}

	@Test
	void shouldRewriteParameters2() {

		var mapping = Map.of("param1", "foo", "x", "y");
		var parserOptions = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_NEW_PARAMETER, Expression.class, e -> {
				if (e instanceof Parameter<?> p) {
					return Cypher.parameter(mapping.getOrDefault(p.getName(), p.getName()), p.getValue());
				}
				return e;
			})
			.withCallback(ExpressionCreatedEventType.ON_NEW_VARIABLE, Expression.class, e -> {
				if (e instanceof SymbolicName s) {
					return Cypher.name(mapping.getOrDefault(s.getValue(), s.getValue()));
				}
				return e;
			})
			.build();
		var statement = CypherParser.parse("Match (x:Movie) where x.title = $param1 RETURN x", parserOptions).getCypher();
		assertThat(statement).isEqualTo("MATCH (y:`Movie`) WHERE y.title = $foo RETURN y");
	}
}
