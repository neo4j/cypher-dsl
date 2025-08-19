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
package org.neo4j.cypherdsl.graalvm;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Comparator;
import java.util.Set;
import java.util.stream.Collectors;

import com.querydsl.core.types.dsl.Expressions;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.renderer.Configuration;
import org.neo4j.cypherdsl.core.renderer.Dialect;
import org.neo4j.cypherdsl.core.renderer.Renderer;
import org.neo4j.cypherdsl.parser.CypherParser;
import org.neo4j.cypherdsl.parser.Options;
import org.neo4j.driver.Values;

/**
 * A random application which will be complied into a native binary. The testing will
 * check the expected output.
 *
 * @author Michael J. Simons
 */
public final class Application {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	private Application() {
	}

	@SuppressWarnings("checkstyle:regexp")
	public static void main(String... a) {

		System.out.println(cypherRenderer.render(findAllMovies()));
		var statement = generateQueryWithParams();
		var catalog = statement.getCatalog();
		catalog.getParameters().forEach((k, v) -> System.out.println(k + "=" + v));
		catalog.getParameterNames().forEach(System.out::println);
		System.out.println(cypherRenderer.render(statement));
		System.out.println(cypherRenderer.render(generateComplexQuery()));
		System.out.println(CypherParser.parse("MATCH (p:Parser) RETURN p").getCypher());
		try {
			// noinspection ResultOfMethodCallIgnored
			Cypher.returning((Expression) null);
		}
		catch (Exception ex) {
			System.out.println(ex.getMessage());
		}
		System.out.println(useParserForRewrite());
		System.out.println(generateDialectBasedQuery());
		statement = CypherParser.parseStatement(
				"MATCH (m:Movie {title: 'The Matrix'}) <- [a:ACTED_IN] - (p:Person) WHERE p.born >= 1979 RETURN m, a, p");
		catalog = statement.getCatalog();
		catalog.getIdentifiableExpressions()
			.stream()
			.filter(SymbolicName.class::isInstance)
			.map(SymbolicName.class::cast)
			.map(SymbolicName::getValue)
			.sorted()
			.forEach(System.out::println);
		catalog.getAllPropertyFilters()
			.entrySet()
			.stream()
			.sorted(Comparator.comparing(o -> o.getKey().name()))
			.forEach(e -> System.out.println(e.getKey().name() + ": "
					+ e.getValue().stream().limit(1).map(cc -> cc.right().toString()).collect(Collectors.joining())));
		System.out
			.println(Cypher.adapt(Values.value(LocalDateTime.of(LocalDate.of(2023, 3, 24), LocalTime.of(10, 50, 23))))
				.asExpression());
		System.out.println(
				Cypher.returning(Cypher.adapt(Expressions.TRUE.isTrue().and(Expressions.FALSE.isTrue())).asExpression())
					.build()
					.getCypher());
	}

	private static Statement findAllMovies() {

		var m = Cypher.node("Movie").named("m");
		return Cypher.match(m).returning(m).build();
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
			.returning(person.project("livesIn",
					Cypher.subList(
							Cypher.listBasedOn(person.relationshipTo(location, "LIVES_IN"))
								.returning(location.project("name")),
							Cypher.parameter("personLivedInOffset"),
							Cypher.parameter("personLivedInOffset").add(Cypher.parameter("personLivedInFirst")))))
			.build();
	}

	private static String useParserForRewrite() {

		return CypherParser
			.parseStatement("MATCH (p:Person) -[:HAT_GESPIELT_IN] -> (n:Movie) RETURN n",
					Options.newOptions()
						.withTypeFilter(
								(e, t) -> ((t.size() == 1) && t.contains("HAT_GESPIELT_IN")) ? Set.of("ACTED_IN") : t)
						.build())
			.getCypher();
	}

	private static String generateDialectBasedQuery() {

		Node n = Cypher.anyNode("n");
		Renderer renderer = Renderer.getRenderer(Configuration.newConfig().withDialect(Dialect.NEO4J_5).build());
		return renderer.render(Cypher.match(n).returning(Cypher.distance(n.property("a"), n.property("b"))).build());
	}

}
