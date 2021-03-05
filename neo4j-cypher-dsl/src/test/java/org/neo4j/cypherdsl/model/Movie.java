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
package org.neo4j.cypherdsl.model;

import java.util.List;

import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.NodeImpl;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * @author Michael J. Simons
 */
public final class Movie extends NodeImpl<Movie> {

	public static final Movie MOVIE = new Movie();

	public final Property TAGLINE = this.property("tagline");

	public final Property TITLE = this.property("title");

	public final Property RELEASED = this.property("released");

	public Movie() {
		super("Movie");
	}

	private Movie(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
		super(symbolicName, labels, properties);
	}

	@Override
	public Movie named(SymbolicName newSymbolicName) {

		return new Movie(newSymbolicName, getLabels(), getProperties());
	}

	@Override
	public Movie withProperties(MapExpression newProperties) {

		return new Movie(getSymbolicName().orElse(null), getLabels(), Properties.create(newProperties));
	}
}
