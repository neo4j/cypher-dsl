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
package org.neo4j.cypherdsl.examples.sdn6.movies;

// tag::using-person-repo[]
import java.util.Optional;

import org.neo4j.cypherdsl.core.Conditions;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Functions;
// end::using-person-repo[]
import org.springframework.data.domain.Example;
// tag::using-person-repo[]
import org.springframework.stereotype.Service;

// end::using-person-repo[]

/**
 * @author Michael J. Simons
 */
// tag::using-person-repo[]
@Service
final class PeopleService {

	static final Person_ PERSON = Person_.PERSON.named("n");

	private final PeopleRepository peopleRepository;

	PeopleService(PeopleRepository peopleRepository) {
		this.peopleRepository = peopleRepository;
	}

	// end::using-person-repo[]
	Optional<Person> findOne(Example<Person> example) {
		return peopleRepository.findOne(example);
	}

	// tag::using-person-repo[]
	Iterable<Person> findPeopleBornInThe70tiesOr(Optional<String> optionalName) {

		return peopleRepository.findAll(
			PERSON.BORN.gte(Cypher.literalOf(1970)).and(PERSON.BORN.lt(Cypher.literalOf(1980))) // <.>
				.or(optionalName
					.map(name -> PERSON.NAME.isEqualTo(Cypher.anonParameter(name))) // <.>
					.orElseGet(Conditions::noCondition)) // <.>
		);
	}

	Optional<PersonDetails> findDetails(String name) {

		var d = Movie_.MOVIE.named("d");
		var a = Movie_.MOVIE.named("a");
		var m = Movie_.MOVIE.named("movies");
		var r = Cypher.anyNode("relatedPerson");
		var statement = Cypher.match(Person_.PERSON.withProperties("name", Cypher.anonParameter(name)))
			.optionalMatch(d.DIRECTORS)
			.optionalMatch(a.ACTORS)
			.optionalMatch(Person_.PERSON.relationshipTo(m).relationshipFrom(r, ActedIn_.$TYPE))
			.returningDistinct(
				Person_.PERSON.getRequiredSymbolicName(),
				Functions.collectDistinct(d).as("directed"),
				Functions.collectDistinct(a).as("actedIn"),
				Functions.collectDistinct(r).as("related")).build();

		return peopleRepository.findOne(statement, PersonDetails.class); // <.>
	}
}
// end::using-person-repo[]
