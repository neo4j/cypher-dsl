/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import java.util.List;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/NodePattern.html">NodePattern</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public interface Node extends PatternElement, PropertyContainer, ExposesProperties<Node>, ExposesRelationships<Relationship> {

	/**
	 * @return The labels associated with this {@link Node}
	 */
	@NotNull @Contract(pure = true)
	List<NodeLabel> getLabels();

	/**
	 * Creates a copy of this node with a new symbolic name.
	 *
	 * @param newSymbolicName the new symbolic name.
	 * @return The new node.
	 */
	@NotNull @Contract(pure = true)
	Node named(String newSymbolicName);

	/**
	 * Creates a copy of this node with a new symbolic name.
	 *
	 * @param newSymbolicName the new symbolic name.
	 * @return The new node.
	 */
	@NotNull @Contract(pure = true)
	Node named(SymbolicName newSymbolicName);

	/**
	 * A condition that checks for the presence of labels on a node.
	 *
	 * @param labelsToQuery A list of labels to query
	 * @return A condition that checks whether this node has all of the labels to query
	 */
	@NotNull @Contract(pure = true)
	Condition hasLabels(String... labelsToQuery);

	/**
	 * Creates a new condition whether this node is equal to {@literal otherNode}.
	 *
	 * @param otherNode The node to compare this node to.
	 * @return A condition.
	 */
	@NotNull @Contract(pure = true)
	Condition isEqualTo(Node otherNode);

	/**
	 * Creates a new condition whether this node is not equal to {@literal otherNode}.
	 *
	 * @param otherNode The node to compare this node to.
	 * @return A condition.
	 */
	@NotNull @Contract(pure = true)
	Condition isNotEqualTo(Node otherNode);

	/**
	 * Creates a new condition based on this node whether it is null.
	 *
	 * @return A condition.
	 */
	@NotNull @Contract(pure = true)
	Condition isNull();

	/**
	 * Creates a new condition based on this node whether it is not null.
	 *
	 * @return A condition.
	 */
	@NotNull @Contract(pure = true)
	Condition isNotNull();

	/**
	 * Creates a new sort item of this node in descending order.
	 *
	 * @return A sort item.
	 */
	@NotNull @Contract(pure = true)
	SortItem descending();

	/**
	 * Creates a new sort item of this node in ascending order.
	 *
	 * @return A sort item.
	 */
	@NotNull @Contract(pure = true)
	SortItem ascending();

	/**
	 * Creates an alias for this node.
	 *
	 * @param alias The alias to use.
	 * @return The aliased expression.
	 */
	@NotNull @Contract(pure = true)
	AliasedExpression as(String alias);

	/**
	 * @return A new function invocation returning the internal id of this node.
	 * @deprecated Use {@link #elementId}
	 */
	@NotNull @Contract(pure = true)
	@Deprecated(since = "2022.6.0")
	@SuppressWarnings({ "DeprecatedIsStillUsed", "squid:S1133" }) // The deprecation warning on any client code calling this is actually the point.
	FunctionInvocation internalId();

	/**
	 * @return A new function invocation returning the element id of this node.
	 * @since 2022.6.0
	 */
	@NotNull @Contract(pure = true)
	default FunctionInvocation elementId() {
		return Functions.elementId(this);
	}

	/**
	 * @return A new function invocation returning the labels of this node.
	 */
	@NotNull @Contract(pure = true)
	FunctionInvocation labels();

	/**
	 * Creates a new {@link Node node pattern} which including an additional filter. Returns {@code this} node when
	 * {@code predicate} is literal {@code null}
	 * @param predicate the predicate to filter on
	 * @return a new node or this instance if the predicate to this method was literal {@code null}
	 * @since 2023.9.0
	 */
	@NotNull @Contract(pure = true)
	default Node where(@Nullable Expression predicate) {
		throw new UnsupportedOperationException();
	}
}
