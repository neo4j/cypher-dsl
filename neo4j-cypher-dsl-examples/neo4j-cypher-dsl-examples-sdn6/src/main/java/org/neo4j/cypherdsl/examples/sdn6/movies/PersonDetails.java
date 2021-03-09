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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * This is a DTO based projection, containing a couple of additional details,
 * like the list of movies a person acted in, the movies they direct and which other
 * people they acted with
 *
 * @author Michael J. Simons
 */
public final class PersonDetails {

	private final String name;

	private final Integer born;

	private final List<Movie> actedIn;

	private final List<Movie> directed;

	private final List<Person> related;

	public PersonDetails(String name, Integer born, List<Movie> actedIn,
		List<Movie> directed, List<Person> related) {
		this.name = name;
		this.born = born;
		this.actedIn = new ArrayList<>(actedIn);
		this.directed = new ArrayList<>(directed);
		this.related = new ArrayList<>(related);
	}

	public String getName() {
		return name;
	}

	public Integer getBorn() {
		return born;
	}

	public List<Movie> getActedIn() {
		return Collections.unmodifiableList(actedIn);
	}

	public List<Movie> getDirected() {
		return Collections.unmodifiableList(directed);
	}

	public List<Person> getRelated() {
		return Collections.unmodifiableList(related);
	}
}
