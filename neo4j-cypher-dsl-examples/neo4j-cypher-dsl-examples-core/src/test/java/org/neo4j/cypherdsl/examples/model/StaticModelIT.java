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
package org.neo4j.cypherdsl.examples.model;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;

/**
 * @author Michael J. Simons
 */
class StaticModelIT {

	@Test
	void simpleMatchShouldWork() {

		// tag::simple-model[]
		var cypher = Cypher.match(Movie.MOVIE).returning(Movie.MOVIE).build().getCypher();
		// end::simple-model[]

		Assertions.assertThat(cypher).matches("MATCH \\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void simpleMatchRenamedShouldWork() {

		// tag::simple-model-renamed[]
		var movie = Movie.MOVIE.named("m");
		var cypher = Cypher.match(movie).returning(movie).build().getCypher();
		// end::simple-model-renamed[]

		Assertions.assertThat(cypher).isEqualTo("MATCH (m:`Movie`) RETURN m");
	}

	@Test
	void renamingShouldWork() {

		// tag::add-properties[]
		var movie = Movie.MOVIE.named("m");
		var cypher = Cypher.match(movie)
			.where(movie.TITLE.isEqualTo(Cypher.literalOf("The Matrix"))) // <.>
			.returning(movie)
			.build()
			.getCypher();

		Assertions.assertThat(cypher).isEqualTo("MATCH (m:`Movie`) WHERE m.title = 'The Matrix' RETURN m");
		// end::add-properties[]
	}

	@Test
	void propertiesShouldWorkInMatchWithType() {

		// tag::query-node-by-properties[]
		var movie = Movie.MOVIE.withProperties(Movie.MOVIE.TITLE, Cypher.literalOf("The Matrix")).named("m1");
		var cypher = Cypher.match(movie).returning(movie).build().getCypher();

		Assertions.assertThat(cypher).isEqualTo("MATCH (m1:`Movie` {title: 'The Matrix'}) RETURN m1");
		// end::query-node-by-properties[]
	}

	@Test
	void relRropertiesShouldWorkInMatchWithType() {

		var actedIn = Person.PERSON.ACTED_IN.withProperties(Person.PERSON.ACTED_IN.ROLE, Cypher.literalOf("Neo"))
			.named("n");
		var cypher = Cypher.match(actedIn).returning(Movie.MOVIE).build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\)-\\[n:`ACTED_IN` \\{role: 'Neo'}]->\\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void relRropertiesShouldWorkInMatchWithTypeCustomName() {

		// tag::query-rel-by-properties[]
		var actedIn = Person.PERSON.ACTED_IN.withProperties(Person.PERSON.ACTED_IN.ROLE, Cypher.literalOf("Neo"));
		var cypher = Cypher.match(actedIn).returning(Movie.MOVIE).build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\)-\\[\\w+:`ACTED_IN` \\{role: 'Neo'}]->\\(\\w+:`Movie`\\) RETURN \\w+");
		// end::query-rel-by-properties[]
	}

	@Test
	void renamingRelationshipsShouldWork() {

		var directed = Person.PERSON.DIRECTED.named("d");
		var cypher = Cypher.match(directed).returning(Movie.MOVIE).build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\)-\\[d:`DIRECTED`]->\\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void matchOnRelationshipsShouldWork() {

		var cypher = Cypher.match(Person.PERSON.DIRECTED).returning(Movie.MOVIE).build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Person`\\)-\\[\\w+:`DIRECTED`]->\\(\\w+:`Movie`\\) RETURN \\w+");
	}

	@Test
	void matchOnRelationshipsShouldWorkInverse() {

		var cypher = Cypher.match(Person.PERSON.DIRECTED.inverse()).returning(Person.PERSON).build().getCypher();

		Assertions.assertThat(cypher)
			.matches("MATCH \\(\\w+:`Movie`\\)<-\\[:`DIRECTED`]-\\(\\w+:`Person`\\) RETURN \\w+");
	}

	@Test
	void multipleRelationships() {

		// tag::multiple-relationships[]
		var cypher = Cypher.match(Person.PERSON.DIRECTED)
			.match(Person.PERSON.ACTED_IN)
			.returning(Person.PERSON.DIRECTED, Person.PERSON.ACTED_IN)
			.build()
			.getCypher();
		// end::multiple-relationships[]

		Assertions.assertThat(cypher)
			.matches(
					"MATCH \\(\\w+:`Person`\\)-\\[\\w+:`DIRECTED`]->\\(\\w+:`Movie`\\) MATCH \\(\\w+\\)-\\[\\w+:`ACTED_IN`]->\\(\\w+\\) RETURN \\w+, \\w+");
	}

	@Test
	void chaining() {

		// tag::chaining-relationships[]
		var otherPerson = Person.PERSON.named("o");
		var cypher = Cypher.match(Person.PERSON.DIRECTED.inverse().relationshipTo(otherPerson, "FOLLOWS") // <.>
		).where(otherPerson.NAME.isEqualTo(Cypher.literalOf("Someone"))).returning(Person.PERSON).build().getCypher();

		Assertions.assertThat(cypher)
			.matches(
					"MATCH \\(\\w+:`Movie`\\)<-\\[:`DIRECTED`]-\\(\\w+:`Person`\\)-\\[:`FOLLOWS`]->\\(o:`Person`\\) WHERE o\\.name = 'Someone' RETURN \\w+");
		// end::chaining-relationships[]
	}

	@Test
	void workWithPropertiesShouldBePossible() {

		// tag::work-with-properties[]
		var cypher = Cypher.match(Person.PERSON).returning(Person.PERSON.NAME, Person.PERSON.BORN).build().getCypher();

		Assertions.assertThat(cypher).matches("MATCH \\(\\w+:`Person`\\) RETURN \\w+\\.name, \\w+\\.born");
		// end::work-with-properties[]
	}

	@Test
	void queryingNonStaticInformationAndPathsShouldWork() {

		var otherPerson = Person.PERSON.named("o");
		var cypher = Cypher
			.match(Person.PERSON.withProperties(Person.PERSON.NAME, Cypher.literalOf("Tom Hanks"))
				.relationshipTo(otherPerson, "WORKED_WITH")) // <.>
			.returning(otherPerson.NAME)
			.build()
			.getCypher();

		Assertions.assertThat(cypher)
			.matches(
					"MATCH \\(\\w+:`Person` \\{name: 'Tom Hanks'}\\)-\\[:`WORKED_WITH`]->\\(o:`Person`\\) RETURN o\\.name");
	}

	@Test
	void workingOnTheDelegateShouldMakeSens() {

		// tag::deriving-new-properties[]
		var cypher = Cypher.match(Person.PERSON)
			.returning(Person.PERSON.NAME.concat(Cypher.literalOf(" whatever")))
			.build()
			.getCypher();

		Assertions.assertThat(cypher).matches("MATCH \\(\\w+:`Person`\\) RETURN \\(\\w+\\.name \\+ ' whatever'\\)");
		// end::deriving-new-properties[]
	}

	@Test
	void oldQueryDSLExampleRevisited() {

		var person = new Person().named("person");
		Assertions
			.assertThat(Cypher.match(person)
				.where(person.FIRST_NAME.eq(Cypher.literalOf("P")).and(person.property("age").gt(Cypher.literalOf(25))))
				.returning(person)
				.build()
				.getCypher())
			.isEqualTo("MATCH (person:`Person`) WHERE (person.firstName = 'P' AND person.age > 25) RETURN person");
	}

	@Test
	void inheritanceMappingExampleNodes() {

		var cypher = Cypher.match(Division.DIVISION).returning(Division.DIVISION.NAME).build().getCypher();

		Assertions.assertThat(cypher).matches("MATCH \\(\\w+:`DefaultNode`:`Division`\\) RETURN \\w+\\.name");
	}

	@Test
	void namedInheritedModelShouldWork() {

		var division = Division.DIVISION.named("d");

		var cypher = Cypher.match(division).returning(division.NAME).build().getCypher();

		Assertions.assertThat(cypher).matches("MATCH \\(d:`DefaultNode`:`Division`\\) RETURN d\\.name");
	}

	@Test
	void inheritanceMatchOnRelationshipsShouldWork() {

		var cypher = Cypher.match(Department.DEPARTMENT.BELONGS_TO)
			.returning(Division.DIVISION.asExpression(), Department.DEPARTMENT.BELONGS_TO.CREATED_AT)
			.build()
			.getCypher();

		Assertions.assertThat(cypher)
			.matches(
					"MATCH \\(\\w+:`DefaultNode`:`Department`\\)-\\[\\w+:`COOP_REL`\\|`BELONGS_TO`]->\\(\\w+:`DefaultNode`:`Division`\\) RETURN \\w+, \\w+\\.createdAt");
	}

	@Test
	void inheritanceMatchOnRelationshipsShouldWorkInverse() {

		var inversRelationship = Department.DEPARTMENT.BELONGS_TO.inverse();
		var cypher = Cypher.match(inversRelationship)
			.returning(Division.DIVISION.asExpression(),
					Department.DEPARTMENT.BELONGS_TO.CREATED_AT
						.referencedAs(inversRelationship.getRequiredSymbolicName().getValue()))
			.build()
			.getCypher();

		Assertions.assertThat(cypher)
			.matches(
					"MATCH \\(\\w+:`DefaultNode`:`Division`\\)<-\\[\\w+:`COOP_REL`\\|`BELONGS_TO`]-\\(\\w+:`DefaultNode`:`Department`\\) RETURN \\w+, \\w+\\.createdAt");
	}

}
