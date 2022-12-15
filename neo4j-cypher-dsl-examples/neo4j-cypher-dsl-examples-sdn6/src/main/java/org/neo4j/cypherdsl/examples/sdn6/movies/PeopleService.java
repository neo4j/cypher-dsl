/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.examples.sdn6.movies;

// tag::using-person-repo[]
import java.util.Optional;
import java.util.function.Function;

import org.neo4j.cypherdsl.core.Conditions;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Functions;
// end::using-person-repo[]
import org.neo4j.cypherdsl.core.SymbolicName;
import org.neo4j.cypherdsl.parser.CypherParser;
import org.neo4j.cypherdsl.parser.ExpressionCreatedEventType;
import org.neo4j.cypherdsl.parser.Options;
import org.springframework.data.domain.Example;
// tag::using-person-repo[]
import org.springframework.data.neo4j.core.mapping.Constants;
import org.springframework.data.neo4j.core.mapping.Neo4jMappingContext;
import org.springframework.stereotype.Service;

// end::using-person-repo[]

/**
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
		this.personRootName = Constants.NAME_OF_TYPED_ROOT_NODE.apply(
			mappingContext.getRequiredPersistentEntity(Person.class));
		this.person = Person_.PERSON.named(personRootName);
	}

	// end::using-person-repo[]
	Optional<Person> findOne(Example<Person> example) {
		return peopleRepository.findOne(example);
	}


	// tag::using-parser-with-spring[]
	Iterable<Person> findPeopleBornAfterThe70tiesAnd(String additionalConditions) {

		return peopleRepository.findAll(
			person.BORN.gte(Cypher.literalOf(1980))
				.and(CypherParser.parseExpression(additionalConditions).asCondition()) // <.>
		);
	}
	// end::using-parser-with-spring[]

	// tag::using-parser-with-spring2[]
	Iterable<Person> findPeopleBornAfterThe70tiesAndV2(String additionalConditions) {

		Function<Expression, Expression> enforceReference =
			e -> personRootName.property(((SymbolicName) e).getValue()); // <.>
		var parserOptions = Options.newOptions()
			.withCallback(
				ExpressionCreatedEventType.ON_NEW_VARIABLE,
				Expression.class,
				enforceReference
			) // <.>
			.build();

		return peopleRepository.findAll(
			person.BORN.gte(Cypher.literalOf(1980)).and(
				CypherParser.parseExpression(
					additionalConditions,
					parserOptions // <.>
				).asCondition()
			)
		);
	}
	// end::using-parser-with-spring2[]

	// tag::using-person-repo[]
	Iterable<Person> findPeopleBornInThe70tiesOr(Optional<String> optionalName) {

		return peopleRepository.findAll(
			person.BORN.gte(Cypher.literalOf(1970)).and(person.BORN.lt(Cypher.literalOf(1980))) // <.>
				.or(optionalName
					.map(name -> person.NAME.isEqualTo(Cypher.anonParameter(name))) // <.>
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
	// end::using-person-repo[]

	// tag::using-temporals[]
	Optional<Person> createNewPerson(NewPersonCmd newPersonCmd) {
		var p = Person_.PERSON.withProperties(
			Person_.PERSON.NAME, Cypher.anonParameter(newPersonCmd.getName()) // <.>
		).named("p");

		var statement = Cypher.merge(p)
			.onCreate().set(
				p.BORN, Cypher.parameter("arbitraryName")
					.withValue(newPersonCmd.getDob().getYear()), // <.>
				p.DOB, Cypher.anonParameter(newPersonCmd.getDob()) // <.>
			).returning(p).build();
		return peopleRepository.findOne(statement);
	}
	// end::using-temporals[]

	// tag::using-person-repo[]
}
// end::using-person-repo[]
