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
package org.neo4j.cypherdsl.core;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class ExpressionsIT {

	@Test
	void simpleCountSubquery() {

		var person = Cypher.node("Person").named("person");
		var cypher = Cypher.match(person)
			.where(Cypher.count(person.relationshipTo(Cypher.node("Dog"), "HAS_DOG")).gt(Cypher.literalOf(1)))
			.returning(person.property("name").as("name"))
			.build()
			.getCypher();
		assertThat(cypher).isEqualTo("MATCH (person:`Person`) " + "WHERE COUNT { (person)-[:`HAS_DOG`]->(:`Dog`) } > 1 "
				+ "RETURN person.name AS name");
	}

	@Test
	void countSubqueryWithWhereClause() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog").named("dog");
		var cypher = Cypher.match(person)
			.where(Cypher.count(person.relationshipTo(dog, "HAS_DOG"))
				.where(person.property("name").eq(dog.property("name")))
				.gt(Cypher.literalOf(1)))
			.returning(person.property("name").as("name"))
			.build()
			.getCypher();
		assertThat(cypher).isEqualTo("MATCH (person:`Person`) "
				+ "WHERE COUNT { (person)-[:`HAS_DOG`]->(dog:`Dog`) WHERE person.name = dog.name } > 1 "
				+ "RETURN person.name AS name");
	}

	@Test
	void countSubqueryWithUnion() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog").named("dog");
		var cat = Cypher.node("Cat").named("cat");
		var inner = Cypher.union(
				Cypher.match(person.relationshipTo(dog, "HAS_DOG"))
					.returning(dog.property("name").as("petName"))
					.build(),
				Cypher.match(person.relationshipTo(cat, "HAS_CAT"))
					.returning(cat.property("name").as("petName"))
					.build());

		var cypher = Cypher.match(person)
			.returning(person.property("name").as("name"), Cypher.count(inner).as("numPets"))
			.build()
			.getCypher();
		assertThat(cypher).isEqualTo("MATCH (person:`Person`) " + "RETURN " + "person.name AS name, " + "COUNT { "
				+ "MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`) " + "RETURN dog.name AS petName " + "UNION "
				+ "MATCH (person)-[:`HAS_CAT`]->(cat:`Cat`) " + "RETURN cat.name AS petName " + "} AS numPets");
	}

	@Test
	void countSubqueryWithWith() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog").named("d");

		var dogName = Cypher.literalOf("Ozzy").as("dogName");
		var cypher = Cypher.match(person)
			.where(Cypher.subqueryWith(dogName)
				.count(person.relationshipTo(dog, "HAS_DOG"))
				.where(dog.property("name").eq(dogName))
				.eq(Cypher.literalOf(1)))
			.returning(person.property("name").as("name"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo("MATCH (person:`Person`) " + "WHERE COUNT { " + "WITH 'Ozzy' AS dogName "
				+ "MATCH (person)-[:`HAS_DOG`]->(d:`Dog`) " + "WHERE d.name = dogName " + "} = 1 "
				+ "RETURN person.name AS name");
	}

	@Test
	void countSubqueryInReturn() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog");

		var cypher = Cypher.match(person)
			.returning(person.property("name"), Cypher.count(person.relationshipTo(dog, "HAS_DOG")).as("howManyDogs"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo(
				"MATCH (person:`Person`) RETURN person.name, COUNT { (person)-[:`HAS_DOG`]->(:`Dog`) } AS howManyDogs");
	}

	@Test
	void countSubqueryInSet() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog");
		var howManyDogs = person.property("howManyDogs");

		var cypher = Cypher.match(person)
			.where(person.property("name").eq(Cypher.literalOf("Andy")))
			.set(howManyDogs.to(Cypher.count(person.relationshipTo(dog, "HAS_DOG"))))
			.returning(howManyDogs.as("howManyDogs"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo("MATCH (person:`Person`) WHERE person.name = 'Andy' "
				+ "SET person.howManyDogs = COUNT { (person)-[:`HAS_DOG`]->(:`Dog`) } "
				+ "RETURN person.howManyDogs AS howManyDogs");
	}

	@Test
	void countSubqueryInCase() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog");
		var dogCount = Cypher.count(person.relationshipTo(dog, "HAS_DOG"));
		var personName = person.property("name");

		var cypher = Cypher.match(person)
			.returning(Cypher.caseExpression()
				.when(dogCount.gt(Cypher.literalOf(1)))
				.then(Cypher.literalOf("Doglover ").concat(personName))
				.elseDefault(personName)
				.as("result"))
			.build()
			.getCypher();

		assertThat(cypher).isEqualTo("MATCH (person:`Person`) " + "RETURN " + "CASE "
				+ "WHEN COUNT { (person)-[:`HAS_DOG`]->(:`Dog`) } > 1 THEN ('Doglover ' + person.name) "
				+ "ELSE person.name " + "END AS result");
	}

	@Test // GH-578
	void fullStatementsAsCountSubQuery() {

		Node p = Cypher.node("Person").named("person");
		Statement inner = Cypher.match(p.relationshipTo(Cypher.node("Dog"), "HAS_DOG"))
			.returning(p.property("name"))
			.build();
		Statement outer = Cypher.match(p)
			.where(Cypher.count(inner).eq(Cypher.literalOf(1)))
			.returning(p.property("name").as("name"))
			.build();

		String cypher = outer.getCypher();
		assertThat(cypher).isEqualTo(
				"MATCH (person:`Person`) WHERE COUNT { MATCH (person)-[:`HAS_DOG`]->(:`Dog`) RETURN person.name } = 1 RETURN person.name AS name");
	}

	@Test // GH-578
	void fullStatementsAsCountSubQueryWithImports() {

		Node p = Cypher.node("Person").named("person");
		Node d = Cypher.node("Dog").named("d");
		SymbolicName dogName = Cypher.name("dogName");
		Statement inner = Cypher.match(p.relationshipTo(d, "HAS_DOG"))
			.where(d.property("name").eq(dogName))
			.returning(p.property("name"))
			.build();
		Statement outer = Cypher.match(p)
			.where(Cypher.count(inner, Cypher.literalOf("Ozzy").as(dogName)).eq(Cypher.literalOf(1)))
			.returning(p.property("name").as("name"))
			.build();

		String cypher = outer.getCypher();
		assertThat(cypher).isEqualTo(
				"MATCH (person:`Person`) WHERE COUNT { WITH 'Ozzy' AS dogName MATCH (person)-[:`HAS_DOG`]->(d:`Dog`) WHERE d.name = dogName RETURN person.name } = 1 RETURN person.name AS name");
	}

}
