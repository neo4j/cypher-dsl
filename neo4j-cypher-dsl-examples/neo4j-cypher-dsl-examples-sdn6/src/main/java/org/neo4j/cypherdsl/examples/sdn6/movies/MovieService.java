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

// tag::as-property[]

import java.util.Collection;
import java.util.List;

import org.neo4j.cypherdsl.core.Cypher;

import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

// end::as-property[]

/**
 * Example service.
 *
 * @author Michael J. Simons
 */
// tag::as-property[]
@Service
final class MovieService {

	private final MovieRepository movieRepository;

	MovieService(MovieRepository movieRepository) {
		this.movieRepository = movieRepository;
	}

	List<Movie> findAll() {

		return this.movieRepository.findAll(Sort.by(Movie_.MOVIE.TITLE.getName()).ascending()); // <.>
	}
	// end::as-property[]

	// tag::more-examples[]
	Collection<Movie> findAllRelatedTo(Person person) {

		var p = Person_.PERSON.named("p");
		var a = Movie_.MOVIE.named("a");
		var d = Movie_.MOVIE.named("d");
		var m = Cypher.name("m");

		var statement = Cypher.match(p)
			.where(p.NAME.isEqualTo(Cypher.anonParameter(person.getName()))) // <.>
			.with(p)
			.optionalMatch(new ActedIn_(p, a))
			.optionalMatch(new Directed_(p, d))
			.with(Cypher.collect(a).add(Cypher.collect(d)).as("movies"))
			.unwind("movies")
			.as(m)
			.returningDistinct(m)
			.orderBy(Movie_.MOVIE.named(m).TITLE)
			.ascending()
			.build();

		return this.movieRepository.findAll(statement);
	}
	// end::more-examples[]
	// tag::as-property[]

}
// end::as-property[]
