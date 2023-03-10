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
package org.neo4j.cypherdsl.core.renderer;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;

/**
 * @author Michael J. Simons
 */
class ConfigurableRendererTest {

	@Test
	void cacheShouldWork() {

		var statement = Cypher.match(Cypher.node("Movie").named("n"))
			.returning(Cypher.name("n").property("f"))
			.build();

		var cypher1 = Renderer.getDefaultRenderer().render(statement);
		var cypher2 = Renderer.getDefaultRenderer().render(statement);
		assertThat(cypher1).isSameAs(cypher2);

		var statement2 = Cypher.match(Cypher.node("Movie").named("n"))
			.returning(Cypher.name("n").property("f"))
			.build();
		var cypher3 = Renderer.getDefaultRenderer().render(statement2);
		assertThat(cypher2).isNotSameAs(cypher3);
	}

	@Test // GH-596
	void shouldGenerateConstantNamesPerSchema() {

		var actor = Cypher.node("Actor").named("a");
		var cool = Cypher.parameter("cool");
		var statement =
			Cypher.with(Cypher.literalOf("The Matrix").as(Cypher.name("title")))
				.match(
					Cypher.node("Movie").named("n").withProperties("foo", Cypher.literalOf("bazbar"))
						.relationshipFrom(actor, "ACTED_IN")
						.named("r")
				)
				.where(Cypher.name("n").property("title").eq(Cypher.name("title")))
				.and(Cypher.name("a").property("name").eq(Cypher.literalOf("Keanu Reeves")))
				.and(actor.property("born").isEqualTo(Cypher.parameter("born")))
				.and(actor.property("cool").isEqualTo(cool))
				.and(actor.property("cool2").isEqualTo(cool))
				.and(actor.property("whatever").isEqualTo(Cypher.parameter("born")))
				.and(Cypher.name("r").property("x").isEqualTo(Cypher.anonParameter("foo")))
				.returning(Cypher.name("n").property("f"))
				.build();

		var cypher1 = Renderer.getRenderer(Configuration.newConfig().useGeneratedNames(true).build())
			.render(statement);

		var expectedGeneratedNames = "WITH 'The Matrix' AS title " +
			"MATCH (v0:`Movie` {foo: 'bazbar'})<-[v1:`ACTED_IN`]-(v2:`Actor`) " +
			"WHERE (v0.title = title AND v2.name = 'Keanu Reeves' AND v2.born = $p0 AND " +
			"v2.cool = $p1 AND v2.cool2 = $p1 AND v2.whatever = $p0 AND v1.x = $pcdsl01) RETURN v0.f";

		var expectedDefault = "WITH 'The Matrix' AS title " +
			"MATCH (n:`Movie` {foo: 'bazbar'})<-[r:`ACTED_IN`]-(a:`Actor`) " +
			"WHERE (n.title = title AND a.name = 'Keanu Reeves' AND a.born = $born AND " +
			"a.cool = $cool AND a.cool2 = $cool AND a.whatever = $born AND r.x = $pcdsl01) RETURN n.f";


		assertThat(cypher1).isEqualTo(expectedGeneratedNames);
		assertThat(statement.getCatalog().getRenamedParameters())
			.containsExactly(
				Map.entry("born", "p0"),
				Map.entry("cool", "p1")
			);

		// Compare a second rendering
		var cypher2 = Renderer.getRenderer(Configuration.newConfig().useGeneratedNames(true).build())
				.render(statement);
		assertThat(cypher2).isEqualTo(expectedGeneratedNames);
		assertThat(cypher1).isSameAs(cypher2);

		assertThat(statement.getCypher()).isEqualTo(expectedDefault);
	}
}
