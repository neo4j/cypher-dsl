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
package org.neo4j.cypherdsl.core;

import java.util.List;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/NodePattern.html">NodePattern</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface Node
		extends PatternElement, PropertyContainer, ExposesProperties<Node>, ExposesRelationships<Relationship> {

	/**
	 * {@return the labels associated with this <code>Node</code>}
	 */
	List<NodeLabel> getLabels();

	/**
	 * Creates a copy of this node with a new symbolic name.
	 * @param newSymbolicName the new symbolic name.
	 * @return the new node.
	 */
	Node named(String newSymbolicName);

	/**
	 * Creates a copy of this node with a new symbolic name.
	 * @param newSymbolicName the new symbolic name.
	 * @return the new node.
	 */
	Node named(SymbolicName newSymbolicName);

	/**
	 * A condition that checks for the presence of labels on a node.
	 * @param labelsToQuery a list of labels to query
	 * @return a condition that checks whether this node has all the labels to query
	 */
	Condition hasLabels(String... labelsToQuery);

	/**
	 * Returns a condition that checks for the presence of a label expression on a node.
	 * @param labels the labels to check
	 * @return a condition that checks whether this node has all the labels to query
	 * @since 2024.3.0
	 * @deprecated use {@link #hasLabels(Labels)}
	 */
	@SuppressWarnings("removal")
	@Deprecated(forRemoval = true)
	Condition hasLabels(LabelExpression labels);

	/**
	 * Returns a condition that checks for the presence of a label expression on a node.
	 * @param labels the labels to check
	 * @return a condition that checks whether this node has all the labels to query
	 * @since 2025.1.0
	 */
	Condition hasLabels(Labels labels);

	/**
	 * Creates a new condition whether this node is equal to {@literal otherNode}.
	 * @param otherNode the node to compare this node to.
	 * @return a condition.
	 */
	Condition isEqualTo(Node otherNode);

	/**
	 * Creates a new condition whether this node is not equal to {@literal otherNode}.
	 * @param otherNode the node to compare this node to.
	 * @return a condition.
	 */
	Condition isNotEqualTo(Node otherNode);

	/**
	 * Creates a new condition based on this node whether it is null.
	 * @return a condition.
	 */
	Condition isNull();

	/**
	 * Creates a new condition based on this node whether it is not null.
	 * @return a condition.
	 */
	Condition isNotNull();

	/**
	 * Creates a new sort item of this node in descending order.
	 * @return a sort item.
	 */
	SortItem descending();

	/**
	 * Creates a new sort item of this node in ascending order.
	 * @return a sort item.
	 */
	SortItem ascending();

	/**
	 * Creates an alias for this node.
	 * @param alias the alias to use.
	 * @return the aliased expression.
	 */
	AliasedExpression as(String alias);

	/**
	 * Returns a new function invocation returning the internal id of this node.
	 * @return a new function invocation returning the internal id of this node
	 * @deprecated Use {@link #elementId}
	 */
	@Deprecated(since = "2022.6.0")
	// The deprecation warning on any client code calling this is actually the point.
	@SuppressWarnings({ "DeprecatedIsStillUsed", "squid:S1133" })
	FunctionInvocation internalId();

	/**
	 * Returns a new function invocation returning the element id of this node.
	 * @return a new function invocation returning the element id of this node
	 * @since 2022.6.0
	 */
	default FunctionInvocation elementId() {
		return Functions.elementId(this);
	}

	/**
	 * {@return a new function invocation returning the labels of this node}
	 */
	FunctionInvocation labels();

}
