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
import java.util.Map;

import jakarta.enterprise.context.ApplicationScoped;

import org.neo4j.ogm.session.SessionFactory;

/**
 * @author Michael J. Simons
 */
@ApplicationScoped
class MovieRepository {

	private final SessionFactory sessionFactory;

	MovieRepository(SessionFactory sessionFactory) {
		this.sessionFactory = sessionFactory;
	}

	Collection<Movie> findAll() {

		return sessionFactory.openSession().loadAll(Movie.class);
	}

	Movie findByTitle(String title) {

		return sessionFactory.openSession().queryForObject(Movie.class,
			"MATCH (m:$($label)) WHERE m[$property] = $propertyValue RETURN m",
			Map.of("label", Movie_.$PRIMARY_LABEL, "property", Movie_.MOVIE.TITLE.getName(), "propertyValue", title)
		);
	}
}
