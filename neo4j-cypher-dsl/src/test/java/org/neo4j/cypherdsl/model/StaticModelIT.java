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
package org.neo4j.cypherdsl.model;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;

/**
 * @author Michael J. Simons
 */
class StaticModelIT {

	@Test
	void simpleMatchShouldWork() {

		String cypher = Cypher.match(Movie.MOVIE)
			.returning(Movie.MOVIE)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void renamingShouldWork() {

		Movie m1 = Movie.MOVIE.named("m1");
		String cypher = Cypher.match(m1)
			.where(m1.TITLE.isEqualTo(Cypher.literalOf("The Matrix")))
			.returning(m1)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.isEqualTo("MATCH (m1:`Movie`) WHERE m1.title = 'The Matrix' RETURN m1");
	}

	@Test
	void propertiesShouldWorkInMatchWithType() {

		Movie m1 = Movie.MOVIE.withProperties(Movie.MOVIE.TITLE, Cypher.literalOf("The Matrix")).named("m1");
		String cypher = Cypher.match(m1)
			.returning(m1)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.isEqualTo("MATCH (m1:`Movie` {title: 'The Matrix'}) RETURN m1");
	}

	@Test
	void relRropertiesShouldWorkInMatchWithType() {

		ActedIn actedIn = Person.PERSON.ACTED_IN.withProperties(Person.PERSON.ACTED_IN.ROLE, Cypher.literalOf("Neo"))
			.named("n");
		String cypher = Cypher.match(actedIn)
			.returning(Movie.MOVIE)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\)-\\[n:`ACTED_IN` \\{role: 'Neo'}]->\\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void renamingRelationshipsShouldWork() {

		Directed<Movie> directed = Person.PERSON.DIRECTED.named("d");
		String cypher = Cypher.match(directed)
			.returning(Movie.MOVIE)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\)-\\[d:`DIRECTED`]->\\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void matchOnRelationshipsShouldWork() {

		String cypher = Cypher.match(Person.PERSON.DIRECTED)
			.returning(Movie.MOVIE)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\)-\\[\\w+:`DIRECTED`]->\\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void multipleRelationships() {

		String cypher = Cypher.match(Person.PERSON.DIRECTED)
			.match(Person.PERSON.ACTED_IN)
			.returning(Person.PERSON.DIRECTED, Person.PERSON.ACTED_IN)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches(
				"MATCH \\(\\w+:`Person`\\)-\\[\\w+:`DIRECTED`]->\\(\\w+:`Movie`\\) MATCH \\(\\w+\\)-\\[\\w+:`ACTED_IN`]->\\(\\w+\\) RETURN \\w+, \\w+");
	}

	@Test
	void workWithPropertiesShouldBePossible() {

		String cypher = Cypher.match(Person.PERSON)
			.returning(Person.PERSON.NAME, Person.PERSON.BORN)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\) RETURN \\w+\\.name, \\w+\\.born");
	}

	@Test
	void queryingNonStaticInformationAndPathsShouldWork() {

		Person otherPerson = Person.PERSON.named("o");
		String cypher = Cypher.match(Person.PERSON.withProperties(Person.PERSON.NAME, Cypher.literalOf("Tom Hanks"))
			.relationshipTo(otherPerson, "WORKED_WITH"))
			.returning(otherPerson.NAME)
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches(
				"MATCH \\(\\w+:`Person` \\{name: 'Tom Hanks'}\\)-\\[:`WORKED_WITH`]->\\(o:`Person`\\) RETURN o\\.name");
	}

	@Test
	void workingOnTheDelegateShouldMakeSens() {

		String cypher = Cypher.match(Person.PERSON)
			.returning(Person.PERSON.NAME.concat(Cypher.literalOf(" whatever")))
			.build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\) RETURN \\(\\w+\\.name \\+ ' whatever'\\)");
	}

	@Test
	void oldQueryDSLExampleRevisited() {

		Person person = new Person().named("person");
		Assertions.assertThat(
			Cypher.match(person)
				.where(person.FIRST_NAME.eq(Cypher.literalOf("P")).
					and(person.property("age").gt(Cypher.literalOf(25)))).returning(person).build().getCypher()
		).isEqualTo("MATCH (person:`Person`) WHERE (person.firstName = 'P' AND person.age > 25) RETURN person");
	}
}
