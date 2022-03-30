/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.parser;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiFunction;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
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
}
