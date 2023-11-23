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

// tag::as-property[]
import java.util.Collection;
import java.util.List;

import org.neo4j.cypherdsl.core.Cypher;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

// end::as-property[]

/**
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

		return movieRepository
			.findAll(Sort.by(Movie_.MOVIE.TITLE.getName()).ascending()); // <.>
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
			.with(Cypher.collect(a).add(Cypher.collect(d))
				.as("movies"))
			.unwind("movies").as(m)
			.returningDistinct(m)
			.orderBy(Movie_.MOVIE.named(m).TITLE).ascending()
			.build();

		return this.movieRepository.findAll(statement);
	}
	// end::more-examples[]
	// tag::as-property[]
}
// end::as-property[]
