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
package org.neo4j.cypherdsl.examples.model;

// tag::simple-model[]
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.RelationshipBase;
import org.neo4j.cypherdsl.core.SymbolicName;

// end::simple-model[]
/**
 * @author Michael J. Simons
 * @soundtrack Höhner - Die ersten 30 Jahre
 */
// tag::simple-model[]
public final class ActedIn extends RelationshipBase<Person, Movie, ActedIn> { // <.>

	public static final String $TYPE = "ACTED_IN";

	public final Property ROLE = this.property("role"); // <.>

	protected ActedIn(Person start, Movie end) {
		super(start, $TYPE, end); // <.>
	}

	private ActedIn(SymbolicName symbolicName, Node start, Properties properties, Node end) { // <.>
		super(symbolicName, start, $TYPE, properties, end);
	}

	@Override
	public ActedIn named(SymbolicName newSymbolicName) { // <.>

		return new ActedIn(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
	}

	@Override
	public ActedIn withProperties(MapExpression newProperties) { // <.>

		return new ActedIn(getSymbolicName().orElse(null), getLeft(), Properties.create(newProperties), getRight());
	}
}
// end::simple-model[]
