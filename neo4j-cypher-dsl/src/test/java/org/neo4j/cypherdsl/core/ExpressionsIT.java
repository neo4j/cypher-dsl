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
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class ExpressionsIT {

	@Test
	void simpleCountSubquery() {

		var person = Cypher.node("Person").named("person");
		var cypher = Cypher.match(person)
			.where(Expressions.count(person.relationshipTo(Cypher.node("Dog"), "HAS_DOG")).gt(Cypher.literalOf(1)))
			.returning(person.property("name").as("name"))
			.build()
			.getCypher();
		assertThat(cypher).isEqualTo("MATCH (person:`Person`) " +
			"WHERE COUNT { (person)-[:`HAS_DOG`]->(:`Dog`) } > 1 " +
			"RETURN person.name AS name"
		);
	}

	@Test
	void countSubqueryWithWhereClause() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog").named("dog");
		var cypher = Cypher.match(person)
			.where(Expressions.count(person.relationshipTo(dog, "HAS_DOG")).where(person.property("name").eq(dog.property("name"))).gt(Cypher.literalOf(1)))
			.returning(person.property("name").as("name"))
			.build()
			.getCypher();
		assertThat(cypher).isEqualTo("MATCH (person:`Person`) " +
			"WHERE COUNT { (person)-[:`HAS_DOG`]->(dog:`Dog`) WHERE person.name = dog.name } > 1 " +
			"RETURN person.name AS name"
		);
	}

	@Test
	void countSubqueryWithUnion() {

		var person = Cypher.node("Person").named("person");
		var dog = Cypher.node("Dog").named("dog");
		var cat = Cypher.node("Cat").named("cat");
		var inner = Cypher.union(
			Cypher.match(person.relationshipTo(dog, "HAS_DOG"))
				.returning(dog.property("name").as("petName")).build(),
			Cypher.match(person.relationshipTo(cat, "HAS_CAT"))
				.returning(cat.property("name").as("petName")).build());

		var cypher = Cypher.match(person)
			.returning(
				person.property("name").as("name"),
				Expressions.count(inner).as("numPets")
			).build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (person:`Person`) " +
			"RETURN " +
			"person.name AS name, " +
			"COUNT { " +
			"MATCH (person)-[:`HAS_DOG`]->(dog:`Dog`) " +
			"RETURN dog.name AS petName " +
			"UNION " +
			"MATCH (person)-[:`HAS_CAT`]->(cat:`Cat`) " +
			"RETURN cat.name AS petName " +
			"} AS numPets"
		);
	}
}
