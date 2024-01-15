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
package org.neo4j.cypherdsl.examples.model;

import java.util.List;

import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * @author Michael J. Simons
 * @soundtrack Höhner - Die ersten 30 Jahre
 */
// tag::simple-model[]
public final class Person extends NodeBase<Person> {

	public static final Person PERSON = new Person();

	public final Directed<Movie> DIRECTED = new Directed<>(this, Movie.MOVIE);

	public final ActedIn ACTED_IN = new ActedIn(this, Movie.MOVIE); // <.>

	public final Property NAME = this.property("name");

	public final Property FIRST_NAME = this.property("firstName");

	public final Property BORN = this.property("born");
	// end::simple-model[]

	public Person() {
		super("Person");
	}

	private Person(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
		super(symbolicName, labels, properties);
	}

	@Override
	public Person named(SymbolicName newSymbolicName) {

		return new Person(newSymbolicName, getLabels(), getProperties());
	}

	@Override
	public Person withProperties(MapExpression newProperties) {

		return new Person(getSymbolicName().orElse(null), getLabels(), Properties.create(newProperties));
	}
	// tag::simple-model[]
}
// end::simple-model[]
