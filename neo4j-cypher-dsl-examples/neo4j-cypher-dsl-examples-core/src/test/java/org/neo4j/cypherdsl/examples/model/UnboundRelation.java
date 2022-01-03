/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
 * Basically only the holder of the type.
 *
 * @author Michael J. Simons
 * @param <S> Start node
 * @param <E> End node
 */
public final class UnboundRelation<S extends NodeBase<?>, E extends NodeBase<?>>
	extends RelationshipBase<S, E, UnboundRelation<S, E>> {

	public static final String $TYPE  = "UNBOUND";

	protected UnboundRelation(S start, E end) {
		super(start, $TYPE, end);
	}

	private UnboundRelation(SymbolicName symbolicName, Node start, Properties properties, Node end) {
		super(symbolicName, start, $TYPE, properties, end);
	}

	@Override
	public UnboundRelation<S, E> named(SymbolicName newSymbolicName) {

		return new UnboundRelation<>(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
	}

	@Override
	public UnboundRelation<S, E> withProperties(MapExpression newProperties) {

		return new UnboundRelation<>(getSymbolicName().orElse(null), getLeft(),
			Properties.create(newProperties), getRight());
	}
}
