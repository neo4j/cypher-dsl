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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * This is the base class for all nodes. It can be used with generics, specifying a valid type.
 * This is useful when using it as a base class for a static meta model.
 *
 * @param <SELF> The type of this node
 * @author Michael J. Simons
 * @soundtrack Queen - The Miracle
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public abstract class NodeBase<SELF extends Node> extends AbstractNode implements Node {

	private volatile SymbolicName symbolicName;

	final List<NodeLabel> labels;

	final Properties properties;

	// ------------------------------------------------------------------------
	// Public API to be used by the static meta model.
	// Non-final methods are ok to be overwritten.
	// ------------------------------------------------------------------------

	protected NodeBase(String primaryLabel, String... additionalLabels) {

		this(null, primaryLabel, null, additionalLabels);
	}

	protected NodeBase(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {

		this.symbolicName = symbolicName;
		this.labels = new ArrayList<>(labels);
		this.properties = properties;
	}

	@Override
	public final SELF named(String newSymbolicName) {

		Assertions.hasText(newSymbolicName, "Symbolic name is required.");
		return named(SymbolicName.of(newSymbolicName));
	}

	/**
	 * This method needs to be implemented to provide new, type safe instances of this node.
	 * @param newSymbolicName the new symbolic name.
	 * @return A new node
	 */
	@Override
	abstract public SELF named(SymbolicName newSymbolicName);

	@Override
	public final SELF withProperties(Object... keysAndValues) {

		MapExpression newProperties = null;
		if (keysAndValues != null && keysAndValues.length != 0) {
			newProperties = MapExpression.create(keysAndValues);
		}
		return withProperties(newProperties);
	}

	@Override
	public final SELF withProperties(Map<String, Object> newProperties) {

		return withProperties(MapExpression.create(newProperties));
	}

	/**
	 * This method needs to be implemented to provide new, type safe instances of this node.
	 * @param newProperties the new properties (can be {@literal null} to remove exiting properties).
	 * @return A new node
	 */
	@Override
	abstract public SELF withProperties(MapExpression newProperties);

	protected final Properties getProperties() {
		return properties;
	}

	@Override
	public final List<NodeLabel> getLabels() {
		return Collections.unmodifiableList(labels);
	}

	@Override
	public final Optional<SymbolicName> getSymbolicName() {
		return Optional.ofNullable(symbolicName);
	}

	@Override
	public final SymbolicName getRequiredSymbolicName() {

		SymbolicName requiredSymbolicName = this.symbolicName;
		if (requiredSymbolicName == null) {
			synchronized (this) {
				requiredSymbolicName = this.symbolicName;
				if (requiredSymbolicName == null) {
					this.symbolicName = SymbolicName.unresolved();
					requiredSymbolicName = this.symbolicName;
				}
			}
		}
		return requiredSymbolicName;
	}

	// ------------------------------------------------------------------------
	// Internal API.
	// ------------------------------------------------------------------------

	NodeBase() {

		this(null, Collections.emptyList(), null);
	}

	NodeBase(SymbolicName symbolicName, String primaryLabel, MapExpression properties, String... additionalLabels) {

		this(symbolicName, assertLabels(primaryLabel, additionalLabels), Properties.create(properties));
	}

	@Override
	public final void accept(Visitor visitor) {

		visitor.enter(this);
		this.getSymbolicName().ifPresent(s -> s.accept(visitor));
		this.labels.forEach(label -> label.accept(visitor));
		Visitable.visitIfNotNull(this.properties, visitor);
		visitor.leave(this);
	}

	private static List<NodeLabel> assertLabels(String primaryLabel, String[] additionalLabels) {

		Assertions.hasText(primaryLabel, "A primary label is required.");

		if (additionalLabels != null) {
			for (String additionalLabel : additionalLabels) {
				Assertions.hasText(additionalLabel, "An empty label is not allowed.");
			}
		}

		List<NodeLabel> labels = new ArrayList<>();
		labels.add(new NodeLabel(primaryLabel));
		labels.addAll(Arrays.stream(additionalLabels).map(NodeLabel::new).collect(Collectors.toList()));

		return labels;
	}
}
