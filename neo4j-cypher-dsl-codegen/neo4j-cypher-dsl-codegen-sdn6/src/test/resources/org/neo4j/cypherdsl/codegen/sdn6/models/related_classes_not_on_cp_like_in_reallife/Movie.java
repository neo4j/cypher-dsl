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
package org.neo4j.cypherdsl.codegen.sdn6.models.related_classes_not_on_cp_like_in_reallife;

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

/**
 * @author Michael J. Simons
 */
@Node
public final class Movie {

	@Id
	private final String title;

	@Property("tagline")
	private final String description;

	@Relationship(value = "DIRECTED", direction = Direction.INCOMING)
	private final List<Person> directors;

	public enum Type {
		AKTION_FILM, KOMÖDIE, SCHNULZE
	}

	private Type type;

	private Integer released;

	public Movie(String title, String description) {
		this.title = title;
		this.description = description;
		this.directors = new ArrayList<>();
	}

	@PersistenceCreator
	public Movie(String title, String description, List<Person> directors) {
		this.title = title;
		this.description = description;
		this.directors = directors == null ? Collections.emptyList() : new ArrayList<>(directors);
	}

	public String getTitle() {
		return title;
	}

	public String getDescription() {
		return description;
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

	public Type getType() {
		return type;
	}

	public void setType(Type type) {
		this.type = type;
	}

	public Movie addDirectors(Collection<Person> newDirectors) {
		this.directors.addAll(newDirectors);
		return this;
	}
}
