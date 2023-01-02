/*
 * Copyright (c) 2019-2023 "Neo4j,"
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
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;
import org.springframework.data.neo4j.core.schema.Relationship;
import org.springframework.data.neo4j.core.schema.Relationship.Direction;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Michael J. Simons
 */
@Node
public final class Movie {

	@Id
	private final String title;

	@Property("tagline")
	private final String description;

	@Relationship(value = "ACTED_IN", direction = Direction.INCOMING)
	private final List<Actor> actors;

	@Relationship(value = "DIRECTED", direction = Direction.INCOMING)
	private final List<Person> directors;

	private Integer released;

	public Movie(String title, String description) {
		this.title = title;
		this.description = description;
		this.actors = new ArrayList<>();
		this.directors = new ArrayList<>();
	}

	@JsonCreator
	@PersistenceCreator
	public Movie(@JsonProperty("title") String title,
		@JsonProperty("description") String description,
		@JsonProperty("actors") List<Actor> actors,
		@JsonProperty("directors") List<Person> directors) {
		this.title = title;
		this.description = description;
		this.actors = actors == null ? List.of() : new ArrayList<>(actors);
		this.directors = directors == null ? List.of() : new ArrayList<>(directors);
	}

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
