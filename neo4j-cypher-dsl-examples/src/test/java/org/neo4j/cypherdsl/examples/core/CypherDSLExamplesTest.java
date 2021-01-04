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
package org.neo4j.cypherdsl.examples.core;

// tag::cypher-dsl-imports[]
import static org.assertj.core.api.Assertions.assertThat;

// end::cypher-dsl-imports[]

import org.junit.jupiter.api.Test;
// tag::cypher-dsl-imports[]
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Conditions;
import org.neo4j.cypherdsl.core.Functions;
import org.neo4j.cypherdsl.core.Statement;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.core.renderer.Renderer;
// end::cypher-dsl-imports[]

/**
 * Examples for the Cypher DSL.
 *
 * @author Michael J. Simons
 * @soundtrack Rage - Lingua Mortis
 * @since 1.0
 */
class CypherDSLExamplesTest {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Test
	void findAllMovies() {

		// tag::cypher-dsl-e1[]
		var m = Cypher.node("Movie").named("m"); // <.>
		var statement = Cypher.match(m) // <.>
			.returning(m)
			.build(); // <.>

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (m:`Movie`) RETURN m");
		// end::cypher-dsl-e1[]
	}

	@Test
	void playMovieGraphFind() {

		// tag::cypher-dsl-e2[]
		var tom = Cypher.anyNode().named("tom").withProperties("name", Cypher.literalOf("Tom Hanks"));
		var statement = Cypher
			.match(tom).returning(tom)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (tom {name: 'Tom Hanks'}) RETURN tom");
		// end::cypher-dsl-e2[]

		// tag::cypher-dsl-e3[]
		var cloudAtlas = Cypher.anyNode().named("cloudAtlas").withProperties("title", Cypher.literalOf("Cloud Atlas"));
		statement = Cypher
			.match(cloudAtlas).returning(cloudAtlas)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (cloudAtlas {title: 'Cloud Atlas'}) RETURN cloudAtlas");
		// end::cypher-dsl-e3[]

		// tag::cypher-dsl-e4[]
		var people = Cypher.node("Person").named("people");
		statement = Cypher
			.match(people)
			.returning(people.property("name"))
			.limit(10)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (people:`Person`) RETURN people.name LIMIT 10");
		// end::cypher-dsl-e4[]

		// tag::cypher-dsl-e5[]
		var nineties = Cypher.node("Movie").named("nineties");
		var released = nineties.property("released");
		statement = Cypher
			.match(nineties)
			.where(released.gte(Cypher.literalOf(1990)).and(released.lt(Cypher.literalOf(2000))))
			.returning(nineties.property("title"))
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (nineties:`Movie`) WHERE (nineties.released >= 1990 AND nineties.released < 2000) RETURN nineties.title");
		// end::cypher-dsl-e5[]
	}

	@Test
	void playMovieGraphQuery() {

		// tag::cypher-dsl-e6[]
		var tom = Cypher.node("Person").named("tom").withProperties("name", Cypher.literalOf("Tom Hanks"));
		var tomHanksMovies = Cypher.anyNode("tomHanksMovies");
		var statement = Cypher
			.match(tom.relationshipTo(tomHanksMovies, "ACTED_IN"))
			.returning(tom, tomHanksMovies)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (tom:`Person` {name: 'Tom Hanks'})-[:`ACTED_IN`]->(tomHanksMovies) RETURN tom, tomHanksMovies");
		// end::cypher-dsl-e6[]

		// tag::cypher-dsl-e7[]
		var cloudAtlas = Cypher.anyNode("cloudAtlas").withProperties("title", Cypher.literalOf("Cloud Atlas"));
		var directors = Cypher.anyNode("directors");
		statement = Cypher
			.match(cloudAtlas.relationshipFrom(directors, "DIRECTED"))
			.returning(directors.property("name"))
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (cloudAtlas {title: 'Cloud Atlas'})<-[:`DIRECTED`]-(directors) RETURN directors.name");
		// end::cypher-dsl-e7[]

		// tag::cypher-dsl-e8[]
		tom = Cypher.node("Person").named("tom").withProperties("name", Cypher.literalOf("Tom Hanks"));
		var movie = Cypher.anyNode("m");
		var coActors = Cypher.anyNode("coActors");
		var people = Cypher.node("Person").named("people");
		statement = Cypher
			.match(tom.relationshipTo(movie, "ACTED_IN").relationshipFrom(coActors, "ACTED_IN"))
			.returning(coActors.property("name"))
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (tom:`Person` {name: 'Tom Hanks'})-[:`ACTED_IN`]->(m)<-[:`ACTED_IN`]-(coActors) RETURN coActors.name");
		// end::cypher-dsl-e8[]

		// tag::cypher-dsl-e9[]
		cloudAtlas = Cypher.node("Movie").withProperties("title", Cypher.literalOf("Cloud Atlas"));
		people = Cypher.node("Person").named("people");
		var relatedTo = people.relationshipBetween(cloudAtlas).named("relatedTo");
		statement = Cypher
			.match(relatedTo)
			.returning(people.property("name"), Functions.type(relatedTo), relatedTo.getRequiredSymbolicName())
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(
				"MATCH (people:`Person`)-[relatedTo]-(:`Movie` {title: 'Cloud Atlas'}) RETURN people.name, type(relatedTo), relatedTo");
		// end::cypher-dsl-e9[]
	}

	@Test
	void playMovieGraphSolve() {

		// tag::cypher-dsl-bacon[]
		var bacon = Cypher.node("Person").named("bacon").withProperties("name", Cypher.literalOf("Kevin Bacon"));
		var hollywood = Cypher.anyNode("hollywood");
		var statement = Cypher
			.match(bacon.relationshipBetween(hollywood).length(1, 4))
			.returningDistinct(hollywood)
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo("MATCH (bacon:`Person` {name: 'Kevin Bacon'})-[*1..4]-(hollywood) RETURN DISTINCT hollywood");
		// end::cypher-dsl-bacon[]
	}

	@Test
	void playMovieGraphRecommend() {

		// tag::cypher-dsl-r[]
		var tom = Cypher.node("Person").named("tom").withProperties("name", Cypher.literalOf("Tom Hanks"));
		var coActors = Cypher.anyNode("coActors");
		var cocoActors = Cypher.anyNode("cocoActors");
		var strength = Functions.count(Cypher.asterisk()).as("Strength");
		var statement = Cypher
			.match(
				tom.relationshipTo(Cypher.anyNode("m"), "ACTED_IN").relationshipFrom(coActors, "ACTED_IN"),
				coActors.relationshipTo(Cypher.anyNode("m2"), "ACTED_IN").relationshipFrom(cocoActors, "ACTED_IN")
			)
			.where(
				Conditions.not(tom.relationshipTo(Cypher.anyNode(), "ACTED_IN").relationshipFrom(cocoActors, "ACTED_IN")))
			.and(tom.isNotEqualTo(cocoActors))
			.returning(
				cocoActors.property("name").as("Recommended"),
				strength
			).orderBy(strength.asName().descending())
			.build();

		assertThat(cypherRenderer.render(statement))
			.isEqualTo(""
				+ "MATCH "
				+ "(tom:`Person` {name: 'Tom Hanks'})-[:`ACTED_IN`]->(m)<-[:`ACTED_IN`]-(coActors), "
				+ "(coActors)-[:`ACTED_IN`]->(m2)<-[:`ACTED_IN`]-(cocoActors) "
				+ "WHERE (NOT (tom)-[:`ACTED_IN`]->()<-[:`ACTED_IN`]-(cocoActors) AND tom <> cocoActors) "
				+ "RETURN cocoActors.name AS Recommended, count(*) AS Strength ORDER BY Strength DESC");
		// end::cypher-dsl-r[]
	}

	@Test // GH-82
	void storedProceduresCanBeCalled() {

		Statement call = Cypher
			.call("dbms.listConfig")
			.withArgs(Cypher.literalOf("browser"))
			.yield("name")
			.build();
		assertThat(cypherRenderer.render(call)).isEqualTo("CALL dbms.listConfig('browser') YIELD name");
	}

	@Test
	void where() {

		SymbolicName name = Cypher.name("name");
		Statement call = Cypher
			.call("dbms.listConfig")
			.withArgs(Cypher.literalOf("browser"))
			.yield(name)
			.where(name.matches("browser\\.allow.*"))
			.returning(Cypher.asterisk())
			.build();
		assertThat(cypherRenderer.render(call))
			.isEqualTo("CALL dbms.listConfig('browser') YIELD name WHERE name =~ 'browser\\\\.allow.*' RETURN *");
	}

}
