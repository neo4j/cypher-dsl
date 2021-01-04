/*
 * Copyright (c) 2019-2021 "Neo4j,"
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

import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 * @soundtrack Bad Religion - Faith Alone 2020
 */
public class Application {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@SuppressWarnings("checkstyle:regexp")
	public static void main(String... a) {

		System.out.println(cypherRenderer.render(findAllMovies()));
		System.out.println(cypherRenderer.render(generateComplexQuery()));
	}

	private static Statement findAllMovies() {

		var m = Cypher.node("Movie").named("m");
		return Cypher.match(m)
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
						Cypher.listBasedOn(person.relationshipTo(location, "LIVES_IN")).returning(location.project("name")),
						Cypher.parameter("personLivedInOffset"),
						Cypher.parameter("personLivedInOffset").add(Cypher.parameter("personLivedInFirst"))
					)
				)
			).build();
	}
}
