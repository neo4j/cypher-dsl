/*
 * Copyright (c) 2019-2024 "Neo4j,"
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
import java.util.function.Function;

import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.parser.CypherParser;
import org.neo4j.cypherdsl.parser.ExpressionCreatedEventType;
import org.neo4j.cypherdsl.parser.Options;

import org.springframework.data.domain.Example;
import org.springframework.data.neo4j.core.mapping.Constants;
import org.springframework.data.neo4j.core.mapping.Neo4jMappingContext;
import org.springframework.stereotype.Service;

// end::using-person-repo[]

/**
 * Example service.
 *
 * @author Michael J. Simons
 */
// tag::using-person-repo[]
@Service
final class PeopleService {

	private final Person_ person;

	private final SymbolicName personRootName;

	private final PeopleRepository peopleRepository;

	PeopleService(PeopleRepository peopleRepository, Neo4jMappingContext mappingContext) {
		this.peopleRepository = peopleRepository;
		this.personRootName = Constants.NAME_OF_TYPED_ROOT_NODE
			.apply(mappingContext.getRequiredPersistentEntity(Person.class));
		this.person = Person_.PERSON.named(this.personRootName);
	}

	// end::using-person-repo[]
	Optional<Person> findOne(Example<Person> example) {
		return this.peopleRepository.findOne(example);
	}

	// tag::using-parser-with-spring[]
	Iterable<Person> findPeopleBornAfterThe70tiesAnd(String additionalConditions) {

		return this.peopleRepository.findAll(this.person.BORN.gte(Cypher.literalOf(1980))
			.and(CypherParser.parseExpression(additionalConditions).asCondition()) // <.>
		);
	}
	// end::using-parser-with-spring[]

	// tag::using-parser-with-spring2[]
	Iterable<Person> findPeopleBornAfterThe70tiesAndV2(String additionalConditions) {

		Function<Expression, Expression> enforceReference = e -> this.personRootName
			.property(((SymbolicName) e).getValue()); // <.>
		var parserOptions = Options.newOptions()
			.withCallback(ExpressionCreatedEventType.ON_NEW_VARIABLE, Expression.class, enforceReference) // <.>
			.build();

		return this.peopleRepository.findAll(this.person.BORN.gte(Cypher.literalOf(1980))
			.and(CypherParser.parseExpression(additionalConditions, parserOptions // <.>
			).asCondition()));
	}
	// end::using-parser-with-spring2[]

	// tag::using-person-repo[]
	Iterable<Person> findPeopleBornInThe70tiesOr(Optional<String> optionalName) {

		return this.peopleRepository.findAll(this.person.BORN.gte(Cypher.literalOf(1970))
			.and(this.person.BORN.lt(Cypher.literalOf(1980))) // <.>
			.or(optionalName.map(name -> this.person.NAME.isEqualTo(Cypher.anonParameter(name))) // <.>
				.orElseGet(Cypher::noCondition)) // <.>
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
			.returningDistinct(Person_.PERSON.getRequiredSymbolicName(), Cypher.collectDistinct(d).as("directed"),
					Cypher.collectDistinct(a).as("actedIn"), Cypher.collectDistinct(r).as("related"))
			.build();

		return this.peopleRepository.findOne(statement, PersonDetails.class); // <.>
	}
	// end::using-person-repo[]

	// tag::using-temporals[]
	Optional<Person> createNewPerson(NewPersonCmd newPersonCmd) {
		var p = Person_.PERSON.withProperties(Person_.PERSON.NAME, Cypher.anonParameter(newPersonCmd.getName()) // <.>
		).named("p");

		var statement = Cypher.merge(p)
			.onCreate()
			.set(p.BORN, Cypher.parameter("arbitraryName").withValue(newPersonCmd.getDob().getYear()), // <.>
					p.DOB, Cypher.anonParameter(newPersonCmd.getDob()) // <.>
			)
			.returning(p)
			.build();
		return this.peopleRepository.findOne(statement);
	}
	// end::using-temporals[]

	// tag::using-person-repo[]

}
// end::using-person-repo[]
