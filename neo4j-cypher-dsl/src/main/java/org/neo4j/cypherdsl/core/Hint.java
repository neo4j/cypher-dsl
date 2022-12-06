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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.neo4j.cypherdsl.core.ast.ProvidesAffixes;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Visitable implementing hints. See {@link ExposesHints}.
 *
 * @author Michael J. Simons
 * @soundtrack Pearl Jam - Vitalogy
 * @since 2021.0.0
 */
public final class Hint implements Visitable {

	private enum Type implements Visitable {

		INDEX, INDEX_SEEK, SCAN, JOIN_ON
	}

	private static final class IndexReference implements Visitable {

		private final SymbolicName symbolicName;
		private final NodeLabel optionalLabel;

		IndexReference(SymbolicName symbolicName) {
			this(symbolicName, null);
		}

		IndexReference(SymbolicName symbolicName, NodeLabel optionalLabel) {
			this.symbolicName = symbolicName;
			this.optionalLabel = optionalLabel;
		}

		boolean pointsToSameContainer(SymbolicName otherSymbolicName, NodeLabel otherLabel) {
			return this.symbolicName.equals(otherSymbolicName) && Objects.equals(this.optionalLabel, otherLabel);
		}

		@Override
		public void accept(Visitor visitor) {

			visitor.enter(this);
			this.symbolicName.accept(visitor);
			Visitable.visitIfNotNull(this.optionalLabel, visitor);
			visitor.leave(this);
		}
	}

	private static final class IndexReferences extends TypedSubtree<IndexReference> {
		IndexReferences(List<IndexReference> indexReferences) {
			super(indexReferences);
		}
	}

	/**
	 * Internal helper class to wrap up the properties used inside an index.
	 */
	private static final class IndexProperties extends TypedSubtree<SymbolicName> implements ProvidesAffixes {
		IndexProperties(List<SymbolicName> properties) {
			super(properties);
		}

		@Override
		public Optional<String> getPrefix() {
			return Optional.of("(");
		}

		@Override
		public Optional<String> getSuffix() {
			return Optional.of(")");
		}
	}

	/**
	 * Creates an index hint. Mostly useful when building elements outside the fluent DSL.
	 *
	 * @param seek       Set to true to use the index for seeks only
	 * @param properties The properties to use in the index, must know their container
	 * @return A hint
	 * @since 2021.2.3
	 */
	public static Hint useIndexFor(boolean seek, Property... properties) {

		Assertions.notEmpty(properties, "Cannot use an index without properties!");

		List<SymbolicName> deferencedProperties = new ArrayList<>();
		IndexReference indexReference = null;
		for (Property property : properties) {
			Named container = property.getContainer();

			Assertions.notNull(container, "Cannot use a property without a reference to a container inside an index hint.");
			Assertions.isTrue(property.getNames().size() == 1,
				"One single property is required. Nested properties are not supported.");

			NodeLabel label;
			if (container instanceof Node) {
				List<NodeLabel> labels = ((Node) container).getLabels();
				Assertions.isTrue(labels.size() == 1, "Exactly one label is required to define the index.");
				label = labels.get(0);
			} else if (container instanceof Relationship) {
				List<String> types = ((Relationship) container).getDetails().getTypes();
				Assertions.isTrue(types.size() == 1, "Exactly one type is required to define the index.");
				label = new NodeLabel(types.get(0));
			} else {
				throw new IllegalArgumentException("A property index can only be used for Nodes or Relationships.");
			}

			SymbolicName symbolicName = container.getRequiredSymbolicName();

			if (indexReference == null) {
				indexReference = new IndexReference(symbolicName, label);
			} else if (!indexReference.pointsToSameContainer(symbolicName, label)) {
				throw new IllegalStateException(
					"If you want to use more than one index on different nodes you must use multiple `USING INDEX` statements.");
			}
			deferencedProperties.add(property.getNames().get(0).getPropertyKeyName());
		}

		return new Hint(seek ? Type.INDEX_SEEK : Type.INDEX, Collections.singletonList(indexReference),
			new IndexProperties(deferencedProperties));
	}

	/**
	 * Creates an index scan hint. Mostly useful when building elements outside the fluent DSL.
	 *
	 * @param node The node who's label and name should be used to define the scan hint
	 * @return A hint
	 * @since 2021.2.3
	 */
	public static Hint useScanFor(Node node) {

		Assertions.notNull(node, "Cannot apply a SCAN hint without a node.");
		List<NodeLabel> labels = node.getLabels();
		Assertions.isTrue(labels.size() == 1, "Exactly one label is required for a SCAN hint.");

		return new Hint(Type.SCAN,
			Collections.singletonList(new IndexReference(node.getRequiredSymbolicName(), labels.get(0))),
			null);
	}

	/**
	 * Creates a join hint on one or more symbolic names.
	 *
	 * @param name The names that are supposed to provide the join point
	 * @return A hint
	 * @since 2021.2.3
	 */
	public static Hint useJoinOn(SymbolicName... name) {

		Assertions.notEmpty(name, "At least one name is required to define a JOIN hint.");
		return new Hint(Type.JOIN_ON, Arrays.stream(name).map(IndexReference::new).toList(), null);
	}

	private final Type type;
	private final IndexReferences indexReferences;
	private final IndexProperties optionalProperties;

	private Hint(Type type, List<IndexReference> indexReferences, IndexProperties optionalProperties) {

		this.type = type;
		this.indexReferences = new IndexReferences(indexReferences);
		this.optionalProperties = optionalProperties;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.type.accept(visitor);
		this.indexReferences.accept(visitor);
		Visitable.visitIfNotNull(this.optionalProperties, visitor);
		visitor.leave(this);
	}
}
