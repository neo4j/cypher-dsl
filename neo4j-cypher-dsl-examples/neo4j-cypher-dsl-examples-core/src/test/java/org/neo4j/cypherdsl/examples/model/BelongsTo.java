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

import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * Defines additional types for a relationship model based on a common type.
 *
 * @author Michael J. Simons
 */
public final class BelongsTo extends AbstractRelationshipDefinition<Department, Division> {

	public static final String $TYPE = "BELONGS_TO";

	protected BelongsTo(Department start, Division end) {
		super(start, end, $TYPE);
	}

	private BelongsTo(SymbolicName symbolicName, Node start, Properties properties, Node end) {
		super(symbolicName, start, properties, end, $TYPE);
	}

	@Override
	public BelongsTo named(SymbolicName newSymbolicName) {

		return new BelongsTo(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
	}

	@Override
	public BelongsTo withProperties(MapExpression newProperties) {

		return new BelongsTo(getSymbolicName().orElse(null), getLeft(), Properties.create(newProperties), getRight());
	}

}
