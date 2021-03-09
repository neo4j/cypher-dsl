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
import java.util.stream.Collectors;

import org.springframework.data.domain.Example;
import org.springframework.data.domain.Sort;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author Michael J. Simons
 */
// tag::as-property[]
@RestController
@RequestMapping("/api/movies")
public final class MoviesController {

	private final MovieRepository movieRepository;

	private final PeopleRepository peopleRepository;

	MoviesController(MovieRepository movieRepository, PeopleRepository peopleRepository) {
		this.movieRepository = movieRepository;
		this.peopleRepository = peopleRepository;
	}

	@GetMapping({ "", "/" })
	public List<Movie> get() {
		return movieRepository
			.findAll(Sort.by(Movie_.MOVIE.TITLE.getName()).ascending()); // <.>
	}
	// end::as-property[]

	@GetMapping({ "/relatedTo/{name}" })
	public List<Movie> relatedTo(@PathVariable String name) {

		return peopleRepository.findOne(Example.of(new Person(name, null)))
			.stream()
			.flatMap(p -> movieRepository.findAllMoviesRelatedTo(p).stream())
			.collect(Collectors.toList());
	}

	// tag::as-property[]
}
// end::as-property[]
