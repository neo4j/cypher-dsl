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

import java.util.List;

import jakarta.json.bind.annotation.JsonbProperty;
import jakarta.json.bind.annotation.JsonbTransient;
import org.neo4j.ogm.annotation.EndNode;
import org.neo4j.ogm.annotation.GeneratedValue;
import org.neo4j.ogm.annotation.Id;
import org.neo4j.ogm.annotation.RelationshipEntity;
import org.neo4j.ogm.annotation.StartNode;

/**
 * Example type.
 *
 * @author Michael J. Simons
 */
@RelationshipEntity("ACTED_IN")
public final class Actor {

	@Id
	@GeneratedValue
	@JsonbTransient
	private Long id;

	private List<String> roles;

	@StartNode
	@JsonbTransient
	private Person person;

	@EndNode
	private Movie movie;

	/**
	 * {@return name of this actor}
	 */
	@JsonbProperty
	public String getName() {
		return this.person.getName();
	}

	/**
	 * {@return read only view of all the roles this actor played}
	 */
	public List<String> getRoles() {
		return List.copyOf(this.roles);
	}

}
