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

import java.util.Collections;
import java.util.List;

import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.RelationshipProperties;
import org.springframework.data.neo4j.core.schema.TargetNode;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Michael J. Simons
 */
@RelationshipProperties
public final class Actor {

	@Id @GeneratedValue Long id;

	@TargetNode
	@JsonIgnore
	private final Person person;

	private final List<String> roles;

	public Actor(Person person, List<String> roles) {
		this.person = person;
		this.roles = roles;
	}

	public Person getPerson() {
		return person;
	}

	@JsonProperty
	public String getName() {
		return person.getName();
	}

	public List<String> getRoles() {
		return Collections.unmodifiableList(roles);
	}
}
