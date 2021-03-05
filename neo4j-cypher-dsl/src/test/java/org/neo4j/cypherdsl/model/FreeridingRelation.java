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

import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeImpl;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.RelationshipImpl;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * Basically only the holder of the type.
 *
 * @author Michael J. Simons
 * @param <S> Start node
 * @param <E> End node
 */
public final class FreeridingRelation<S extends NodeImpl<?>, E extends NodeImpl<?>>
	extends RelationshipImpl<S, E, FreeridingRelation<S, E>> {

	protected FreeridingRelation(S start, E end) {
		super(start, "FREERIDING", end);
	}

	private FreeridingRelation(SymbolicName symbolicName, Node start, String type, Properties properties, Node end) {
		super(symbolicName, start, type, properties, end);
	}

	@Override
	public FreeridingRelation<S, E> named(SymbolicName newSymbolicName) {

		return new FreeridingRelation<>(newSymbolicName, getLeft(), getRequiredType(), getDetails().getProperties(),
			getRight());
	}

	@Override
	public FreeridingRelation<S, E> withProperties(MapExpression newProperties) {

		return new FreeridingRelation<>(getSymbolicName().orElse(null), getLeft(), getRequiredType(),
			Properties.create(newProperties), getRight());
	}
}
