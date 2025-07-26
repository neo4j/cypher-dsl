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
package org.neo4j.cypherdsl.examples.quarkus.movies;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Property;
import org.neo4j.ogm.annotation.Relationship;

/**
 * @author Michael J. Simons
 */
@NodeEntity
public final class Movie {

	@Id
	private String title;

	@Property("tagline")
	private String description;

	@Relationship(value = "ACTED_IN", direction = Relationship.Direction.INCOMING)
	private List<Actor> actors = new ArrayList<>();

	@Relationship(value = "DIRECTED", direction = Relationship.Direction.INCOMING)
	private List<Person> directors = new ArrayList<>();

	public Movie(String title) {
		this.title = title;
	}

	/**
	 * Make OGM happy.
	 */
	Movie() {
	}

	private Integer released;

	public String getTitle() {
		return title;
	}

	public String getDescription() {
		return description;
	}

	public List<Actor> getActors() {
		return Collections.unmodifiableList(this.actors);
	}

	public List<Person> getDirectors() {
		return Collections.unmodifiableList(this.directors);
	}

	public Integer getReleased() {
		return released;
	}

	public void setReleased(Integer released) {
		this.released = released;
	}

	public Movie addActors(Collection<Actor> newActors) {
		this.actors.addAll(newActors);
		return this;
	}

	public Movie addDirectors(Collection<Person> newDirectors) {
		this.directors.addAll(newDirectors);
		return this;
	}
}
