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
package org.neo4j.cypherdsl.examples.sdn6.movies;

import java.time.ZonedDateTime;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;

/**
 * OGM example type.
 *
 * @author Michael J. Simons
 */
@Node
public final class Person {

	@Id
	@GeneratedValue
	private final Long id;

	private final String name;

	private Integer born;

	private ZonedDateTime dob;

	@PersistenceCreator
	private Person(Long id, String name, Integer born) {
		this.id = id;
		this.born = born;
		this.name = name;
	}

	@JsonCreator
	public Person(@JsonProperty("name") String name, @JsonProperty("born") Integer born) {
		this(null, name, born);
	}

	public Long getId() {
		return this.id;
	}

	public String getName() {
		return this.name;
	}

	public Integer getBorn() {
		return this.born;
	}

	public void setBorn(Integer born) {
		this.born = born;
	}

	public ZonedDateTime getDob() {
		return this.dob;
	}

	public void setDob(ZonedDateTime dob) {
		this.dob = dob;
	}

}
