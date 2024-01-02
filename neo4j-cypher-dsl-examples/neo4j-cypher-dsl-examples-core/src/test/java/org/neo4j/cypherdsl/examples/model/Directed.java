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

import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.RelationshipBase;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * I modelled the relationships "DIRECTED" and "ACTED_IN" in this test slightly different to get a feeling how to deal
 * with the same type of relationship between different nodes. A person might as well be the director of a musical etc.
 *
 * @author Michael J. Simons
 * @soundtrack Höhner - Die ersten 30 Jahre
 * @param <E> End node
 */
public final class Directed<E extends NodeBase<?>> extends RelationshipBase<Person, E, Directed<E>> {

	public static final String $TYPE  = "DIRECTED";

	protected Directed(Person start, E end) {
		super(start, $TYPE, end);
	}

	private Directed(SymbolicName symbolicName, Node start, Properties properties, Node end) {
		super(symbolicName, start, $TYPE, properties, end);
	}

	@Override
	public Directed<E> named(SymbolicName newSymbolicName) {

		return new Directed<>(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
	}

	@Override
	public Directed<E> withProperties(MapExpression newProperties) {

		return new Directed<>(getSymbolicName().orElse(null), getLeft(), Properties.create(newProperties), getRight());
	}
}
