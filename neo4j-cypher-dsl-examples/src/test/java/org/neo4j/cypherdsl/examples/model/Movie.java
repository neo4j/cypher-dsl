/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
import java.util.List;

import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Properties;
// end::simple-model[]
// tag::add-properties[]
import org.neo4j.cypherdsl.core.Property;

// end::add-properties[]
// tag::simple-model[]
import org.neo4j.cypherdsl.core.SymbolicName;

// end::simple-model[]

/**
 * @author Michael J. Simons
 */
// tag::simple-model[]
// tag::add-properties[]
public final class Movie extends NodeBase<Movie> { // <.>

	// end::add-properties[]
	public static final Movie MOVIE = new Movie(); // <.>

	// end::simple-model[]
	public final Property TAGLINE = this.property("tagline");

	// tag::add-properties[]
	public final Property TITLE = this.property("title"); // <.>
	// end::add-properties[]

	public final Property RELEASED = this.property("released");

	// tag::simple-model[]
	public Movie() {
		super("Movie"); // <.>
	}

	private Movie(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) { // <.>
		super(symbolicName, labels, properties);
	}

	@Override
	public Movie named(SymbolicName newSymbolicName) { // <.>

		return new Movie(newSymbolicName, getLabels(), getProperties());
	}

	@Override
	public Movie withProperties(MapExpression newProperties) { // <.>

		return new Movie(getSymbolicName().orElse(null), getLabels(), Properties.create(newProperties));
	}
	// tag::add-properties[]
}
// end::simple-model[]
// end::add-properties[]
