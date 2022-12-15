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

import java.time.ZonedDateTime;

import org.springframework.data.annotation.PersistenceCreator;
import org.springframework.data.neo4j.core.schema.GeneratedValue;
import org.springframework.data.neo4j.core.schema.Id;
import org.springframework.data.neo4j.core.schema.Node;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Michael J. Simons
 */
@Node
public final class Person {

	@Id @GeneratedValue
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
		return id;
	}

	public String getName() {
		return name;
	}

	public Integer getBorn() {
		return born;
	}

	public void setBorn(Integer born) {
		this.born = born;
	}

	public ZonedDateTime getDob() {
		return dob;
	}

	public void setDob(ZonedDateTime dob) {
		this.dob = dob;
	}
}
