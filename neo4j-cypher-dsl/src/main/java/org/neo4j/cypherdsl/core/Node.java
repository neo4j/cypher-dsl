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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.util.List;

import org.apiguardian.api.API;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/NodePattern.html">NodePattern</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public interface Node extends PatternElement, PropertyContainer, ExposesProperties<Node>, ExposesRelationships<Relationship> {

	List<NodeLabel> getLabels();

	/**
	 * Creates a copy of this node with a new symbolic name.
	 *
	 * @param newSymbolicName the new symbolic name.
	 * @return The new node.
	 */
	Node named(String newSymbolicName);

	/**
	 * Creates a copy of this node with a new symbolic name.
	 *
	 * @param newSymbolicName the new symbolic name.
	 * @return The new node.
	 */
	Node named(SymbolicName newSymbolicName);

	/**
	 * A condition that checks for the presence of labels on a node.
	 *
	 * @param labelsToQuery A list of labels to query
	 * @return A condition that checks whether this node has all of the labels to query
	 */
	Condition hasLabels(String... labelsToQuery);

	/**
	 * Creates a new condition whether this node is equal to {@literal otherNode}.
	 *
	 * @param otherNode The node to compare this node to.
	 * @return A condition.
	 */
	Condition isEqualTo(Node otherNode);

	/**
	 * Creates a new condition whether this node is not equal to {@literal otherNode}.
	 *
	 * @param otherNode The node to compare this node to.
	 * @return A condition.
	 */
	Condition isNotEqualTo(Node otherNode);

	/**
	 * Creates a new condition based on this node whether it is null.
	 *
	 * @return A condition.
	 */
	Condition isNull();

	/**
	 * Creates a new condition based on this node whether it is not null.
	 *
	 * @return A condition.
	 */
	Condition isNotNull();

	/**
	 * Creates a new sort item of this node in descending order.
	 *
	 * @return A sort item.
	 */
	SortItem descending();

	/**
	 * Creates a new sort item of this node in ascending order.
	 *
	 * @return A sort item.
	 */
	SortItem ascending();

	/**
	 * Creates an alias for this node.
	 *
	 * @param alias The alias to use.
	 * @return The aliased expression.
	 */
	AliasedExpression as(String alias);

	/**
	 * @return A new function invocation returning the internal id of this node.
	 */
	FunctionInvocation internalId();

	/**
	 * @return A new function invocation returning the labels of this node.
	 */
	FunctionInvocation labels();
}
