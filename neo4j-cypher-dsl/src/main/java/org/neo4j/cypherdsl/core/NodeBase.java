/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * This is the base class for all nodes. It can be used with generics, specifying a valid
 * type. This is useful when using it as a base class for a static metamodel.
 *
 * @param <SELF> the type of this node
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = STABLE, since = "2021.1.0")
@SuppressWarnings("deprecation") // IDEA is stupid.
public abstract class NodeBase<SELF extends Node> extends AbstractNode implements Node {

	final List<NodeLabel> staticLabels;

	final Labels dynamicLabels;

	final Properties properties;

	final Where innerPredicate;

	@SuppressWarnings("squid:S3077") // Symbolic name is unmodifiable
	private volatile SymbolicName symbolicName;

	// ------------------------------------------------------------------------
	// Public API to be used by the static meta model.
	// Non-final methods are ok to be overwritten.
	// ------------------------------------------------------------------------

	/**
	 * Creates a new base object from a set of labels.
	 * @param primaryLabel the primary label
	 * @param additionalLabels an optional list of additional ones.
	 */
	protected NodeBase(String primaryLabel, String... additionalLabels) {

		this(null, primaryLabel, null, additionalLabels);
	}

	/**
	 * Creates a new base object from a {@link SymbolicName} name, a list of labels and a
	 * set of properties.
	 * @param symbolicName the symbolic name for this node object
	 * @param staticLabels the list of labels, no primary is given
	 * @param properties a set of properties
	 */
	protected NodeBase(SymbolicName symbolicName, List<NodeLabel> staticLabels, Properties properties) {
		this(symbolicName, new ArrayList<>(staticLabels), null, properties, null);
	}

	NodeBase() {

		this(null, Collections.emptyList(), null);
	}

	NodeBase(SymbolicName symbolicName, String primaryLabel, MapExpression properties, String... additionalLabels) {

		this(symbolicName, assertLabels(primaryLabel, additionalLabels), Properties.create(properties));
	}

	NodeBase(SymbolicName symbolicName, List<NodeLabel> staticLabels, Labels dynamicLabels, Properties properties,
			Where innerPredicate) {

		this.symbolicName = symbolicName;
		this.staticLabels = staticLabels;
		this.dynamicLabels = dynamicLabels;
		this.properties = properties;
		this.innerPredicate = innerPredicate;
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
		if (additionalLabels != null) {
			labels.addAll(Arrays.stream(additionalLabels).map(NodeLabel::new).toList());
		}

		return labels;
	}

	@Override
	public final SELF named(String newSymbolicName) {

		Assertions.hasText(newSymbolicName, "Symbolic name is required.");
		return named(SymbolicName.of(newSymbolicName));
	}

	/**
	 * This method needs to be implemented to provide new, type safe instances of this
	 * node.
	 * @param newSymbolicName the new symbolic name.
	 * @return a new node
	 */
	@Override
	// This is overridden to make sure we allow a covariant return type
	@SuppressWarnings("squid:S3038")
	public abstract SELF named(SymbolicName newSymbolicName);

	@Override
	public final SELF withProperties(Object... keysAndValues) {

		MapExpression newProperties = null;
		if (keysAndValues != null && keysAndValues.length != 0) {
			newProperties = MapExpression.create(false, keysAndValues);
		}
		return withProperties(newProperties);
	}

	/**
	 * A new object with a new set of properties.
	 * @param newProperties a map with the new properties
	 * @return a new object
	 */
	@Override
	public final SELF withProperties(Map<String, Object> newProperties) {

		return withProperties(MapExpression.create(newProperties));
	}

	/**
	 * This method needs to be implemented to provide new, type safe instances of this
	 * node.
	 * @param newProperties the new properties (can be {@literal null} to remove exiting
	 * properties).
	 * @return a new node
	 */
	@Override
	// This is overridden to make sure we allow a covariant return type
	@SuppressWarnings("squid:S3038")
	public abstract SELF withProperties(MapExpression newProperties);

	// ------------------------------------------------------------------------
	// Internal API.
	// ------------------------------------------------------------------------

	/**
	 * {@return set of properties for this node}
	 */
	protected final Properties getProperties() {
		return this.properties;
	}

	@Override
	public final List<NodeLabel> getLabels() {
		return (this.staticLabels != null) ? List.copyOf(this.staticLabels) : List.of();
	}

	@Override
	public final Optional<SymbolicName> getSymbolicName() {
		return Optional.ofNullable(this.symbolicName);
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

	@Override
	public final void accept(Visitor visitor) {

		visitor.enter(this);
		this.getSymbolicName().ifPresent(s -> s.accept(visitor));
		if (this.staticLabels != null) {
			this.staticLabels.forEach(label -> label.accept(visitor));
		}
		Visitable.visitIfNotNull(this.dynamicLabels, visitor);
		Visitable.visitIfNotNull(this.properties, visitor);
		Visitable.visitIfNotNull(this.innerPredicate, visitor);
		visitor.leave(this);
	}

}
