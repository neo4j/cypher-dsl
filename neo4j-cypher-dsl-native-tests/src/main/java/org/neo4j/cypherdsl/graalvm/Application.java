/*
 * Copyright (c) 2019-2023 "Neo4j,"
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
package org.neo4j.cypherdsl.graalvm;

import java.util.Set;

import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Functions;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;
import org.neo4j.cypherdsl.parser.CypherParser;
import org.neo4j.cypherdsl.parser.Options;

/**
 * @author Michael J. Simons
 * @soundtrack Bad Religion - Faith Alone 2020
 */
public class Application {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@SuppressWarnings("checkstyle:regexp")
	public static void main(String... a) {

		System.out.println(cypherRenderer.render(findAllMovies()));
		var statement = generateQueryWithParams();
		statement.getParameters().forEach((k, v) -> System.out.println(k + "=" + v));
		statement.getParameterNames().forEach(System.out::println);
		System.out.println(cypherRenderer.render(statement));
		System.out.println(cypherRenderer.render(generateComplexQuery()));
		System.out.println(CypherParser.parse("MATCH (p:Parser) RETURN p").getCypher());
		try {
			Cypher.returning((Expression) null);
		} catch (Exception e) {
			System.out.println(e.getMessage());
		}
		System.out.println(useParserForRewrite());
		System.out.println(generateDialectBasedQuery());
	}

	private static Statement findAllMovies() {

		var m = Cypher.node("Movie").named("m");
		return Cypher.match(m)
			.returning(m)
			.build();
	}

	private static Statement generateQueryWithParams() {

		var m = Cypher.node("Movie").named("m");
		return Cypher.match(m)
			.where(m.property("title").isEqualTo(Cypher.parameter("title")))
			.or(m.property("title").isEqualTo(Cypher.parameter("pTitle", "someTitle")))
			.or(m.property("title").isEqualTo(Cypher.anonParameter("someOtherTitle")))
			.returning(m)
			.build();
	}

	private static Statement generateComplexQuery() {

		var person = Cypher.node("Person").named("person");
		var location = Cypher.node("Location").named("personLivesIn");
		return Cypher.match(person)
			.returning(
				person.project(
					"livesIn",
					Cypher.subList(
						Cypher.listBasedOn(person.relationshipTo(location, "LIVES_IN"))
							.returning(location.project("name")),
						Cypher.parameter("personLivedInOffset"),
						Cypher.parameter("personLivedInOffset").add(Cypher.parameter("personLivedInFirst"))
					)
				)
			).build();
	}

	private static String useParserForRewrite() {

		return CypherParser
			.parseStatement("MATCH (p:Person) -[:HAT_GESPIELT_IN] -> (n:Movie) RETURN n",
				Options.newOptions()
					.withTypeFilter((e, t) -> t.size() == 1 && t.contains("HAT_GESPIELT_IN") ? Set.of("ACTED_IN") : t)
					.build())
			.getCypher();
	}

	private static String generateDialectBasedQuery() {

		Node n = Cypher.anyNode("n");
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(Dialect.NEO4J_5).build());
		return renderer.render(
			Cypher.match(n).returning(Functions.distance(n.property("a"), n.property("b"))).build());
	}
}
