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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

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

	@JsonCreator
	public PersonDetails(
		@JsonProperty("name") String name, @JsonProperty("born") Integer born, @JsonProperty("actedIn") List<Movie> actedIn,
		@JsonProperty("directed") List<Movie> directed, @JsonProperty("related") List<Person> related) {
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
