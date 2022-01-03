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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.STABLE;

import java.util.ArrayList;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A condition checking for the presence of labels on nodes or types on relationships.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class HasLabelCondition implements Condition {

	private final SymbolicName nodeName;
	private final List<NodeLabel> nodeLabels;

	static HasLabelCondition create(SymbolicName nodeName, String... labels) {

		Assertions.notNull(nodeName, "A symbolic name for the node is required.");
		Assertions.notNull(labels, "Labels to query are required.");
		Assertions.notEmpty(labels, "At least one label to query is required.");

		final List<NodeLabel> nodeLabels = new ArrayList<>(labels.length);
		for (String label : labels) {
			nodeLabels.add(new NodeLabel(label));
		}

		return new HasLabelCondition(nodeName, nodeLabels);
	}

	private HasLabelCondition(SymbolicName nodeName, List<NodeLabel> nodeLabels) {
		this.nodeName = nodeName;
		this.nodeLabels = nodeLabels;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		nodeName.accept(visitor);
		this.nodeLabels.forEach(label -> label.accept(visitor));
		visitor.leave(this);
	}
}
