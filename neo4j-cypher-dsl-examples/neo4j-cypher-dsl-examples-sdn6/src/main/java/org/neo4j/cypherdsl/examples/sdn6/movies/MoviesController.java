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

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.data.domain.Example;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Michael J. Simons
 */
@RestController
@RequestMapping("/api/movies")
public final class MoviesController {

	private final PeopleService peopleService;

	private final MovieService movieService;

	public MoviesController(PeopleService peopleService, MovieService movieService) {
		this.peopleService = peopleService;
		this.movieService = movieService;
	}

	@GetMapping({ "", "/" })
	public List<Movie> get() {
		return movieService.findAll();
	}

	@GetMapping({ "/relatedTo/{name}" })
	public List<Movie> relatedTo(@PathVariable String name) {

		return peopleService.findOne(Example.of(new Person(name, null)))
			.stream()
			.flatMap(p -> movieService.findAllRelatedTo(p).stream())
			.collect(Collectors.toList());
	}
}
