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
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * Defines additional labels for a node model based on a common type.
 *
 * @author Michael J. Simons
 */
public final class Department extends AbstractNodeDefinition<Department> {

	public static final Department DEPARTMENT = new Department();

	public final BelongsTo BELONGS_TO = new BelongsTo(this, Division.DIVISION);

	public Department() {
		super("Department");
	}

	private Department(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
		super(symbolicName, labels, properties);
	}

	@Override
	public Department named(SymbolicName newSymbolicName) {
		return new Department(newSymbolicName, getLabels(), getProperties());
	}

	@Override
	public Department withProperties(MapExpression newProperties) {
		return new Department(getSymbolicName().orElse(null), getLabels(), Properties.create(newProperties));
	}
}
