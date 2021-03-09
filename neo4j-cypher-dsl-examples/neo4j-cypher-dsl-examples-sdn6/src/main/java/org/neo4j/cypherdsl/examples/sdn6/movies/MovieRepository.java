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

import java.util.List;
import java.util.Map;

import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.Functions;
import org.springframework.data.neo4j.core.Neo4jTemplate;
import org.springframework.data.neo4j.repository.Neo4jRepository;

/**
 * @author Michael J. Simons
 */
interface MovieRepository extends Neo4jRepository<Movie, String>, MovieRepositoryExt {
}

interface MovieRepositoryExt {
	List<Movie> findAllMoviesRelatedTo(Person person);
}

// tag::complex-queries[]
class MovieRepositoryExtImpl implements MovieRepositoryExt {

	private final Neo4jTemplate neo4jTemplate;

	MovieRepositoryExtImpl(Neo4jTemplate neo4jTemplate) {

		this.neo4jTemplate = neo4jTemplate;
	}

	@Override
	public List<Movie> findAllMoviesRelatedTo(Person personOfInterest) {

		var person = Person_.PERSON.named("p");
		var actedIn = Movie_.MOVIE.named("a");
		var directed = Movie_.MOVIE.named("d");
		var m = Cypher.name("m");

		var statement = Cypher.match(person)
			.where(person.NAME.isEqualTo(Cypher.parameter("name")))
			.with(person)
			.optionalMatch(new ActedIn_(person, actedIn))
			.optionalMatch(new Directed_(person, directed))
			.with(Functions.collect(actedIn).add(Functions.collect(directed))
				.as("movies"))
			.unwind("movies").as(m)
			.returningDistinct(m)
			.orderBy(Movie_.MOVIE.named(m).TITLE).ascending()
			.build();

		return this.neo4jTemplate.findAll(
			statement, Map.of("name", personOfInterest.getName()), Movie.class);
	}
}
// end::complex-queries[]
