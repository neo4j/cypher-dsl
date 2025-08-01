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
package org.neo4j.cypherdsl.examples.ogm.movies;

import java.util.Collection;

import jakarta.enterprise.context.RequestScoped;
import jakarta.inject.Inject;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.PathParam;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

/**
 * @author Michael J. Simons
 */
@RequestScoped
@Path("/api/movies")
public class MovieResource {

	private final MovieRepository movieRepository;

	/**
	 * @param movieRepository the repository to retrieve movies from
	 */
	@Inject
	public MovieResource(MovieRepository movieRepository) {
		this.movieRepository = movieRepository;
	}

	/**
	 * @return All movies
	 */
	@GET
	@Produces(MediaType.APPLICATION_JSON)
	public Collection<Movie> getMovies() {

		return movieRepository.findAll();
	}

	@GET
	@Path("/{title}")
	@Produces(MediaType.APPLICATION_JSON)
	public Movie getMovie(@PathParam("title") String title) {
		return movieRepository.findByTitle(title);
	}
}
