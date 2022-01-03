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

import java.util.List;

import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * An example for a set of shared properties for the nodes in a domain.
 *
 * @author Michael J. Simons
 * @param <T> The final type of what is described here
 */
public abstract class AbstractNodeDefinition<T extends AbstractNodeDefinition<T>> extends NodeBase<T> {

	public final Property ID = this.property("id");

	public final Property NAME = this.property("name");

	protected AbstractNodeDefinition(String... additionalLabel) {
		super("DefaultNode", additionalLabel);
	}

	protected AbstractNodeDefinition(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) { // <.>
		super(symbolicName, labels, properties);
	}
}
