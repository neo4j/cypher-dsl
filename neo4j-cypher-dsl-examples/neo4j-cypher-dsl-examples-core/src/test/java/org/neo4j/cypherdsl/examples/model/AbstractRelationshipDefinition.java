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

import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.RelationshipBase;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * An example for a set of shared properties for the relationships in a domain.
 *
 * @param <S> start node
 * @param <E> end node
 * @author Michael J. Simons
 */
public abstract class AbstractRelationshipDefinition<S extends AbstractNodeDefinition<?>, E extends AbstractNodeDefinition<?>>
		extends RelationshipBase<S, E, AbstractRelationshipDefinition<S, E>> {

	public static final String $DEFAULT_TYPE = "COOP_REL";

	@SuppressWarnings("this-escape")
	public final Property CREATED_AT = this.property("createdAt");

	protected AbstractRelationshipDefinition(S start, E end, String... additionalTypes) {

		super(start, $DEFAULT_TYPE, end, additionalTypes);
	}

	protected AbstractRelationshipDefinition(SymbolicName symbolicName, Node start, Properties properties, Node end,
			String... additionalTypes) {
		super(symbolicName, start, $DEFAULT_TYPE, properties, end, additionalTypes);
	}

}
