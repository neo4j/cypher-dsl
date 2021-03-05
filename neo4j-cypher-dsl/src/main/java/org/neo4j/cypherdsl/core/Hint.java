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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.TypedSubtree;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;
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

		INDEX, INDEX_SEEK, SCAN, JOIN_ON;
	}

	private final static class IndexReference implements Visitable {

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
			return this.symbolicName.equals(otherSymbolicName) &&
					(this.optionalLabel == null && otherLabel == null || this.optionalLabel.equals(otherLabel));
		}

		@Override
		public void accept(Visitor visitor) {

			visitor.enter(this);
			this.symbolicName.accept(visitor);
			Visitable.visitIfNotNull(this.optionalLabel, visitor);
			visitor.leave(this);
		}
	}

	private final static class IndexReferences extends TypedSubtree<IndexReference> {
		IndexReferences(List<IndexReference> indexReferences) {
			super(indexReferences);
		}
	}

	/**
	 * Internal helper class to wrap up the properties used inside an index.
	 */
	@API(status = INTERNAL)
	public final static class IndexProperties extends TypedSubtree<SymbolicName> {
		IndexProperties(List<SymbolicName> properties) {
			super(properties);
		}
	}

	static Hint useIndexFor(boolean seek, Property... properties) {

		Assertions.notEmpty(properties, "Cannot use an index without properties!");

		List<SymbolicName> deferencedProperties = new ArrayList<>();
		IndexReference indexReference = null;
		for (Property property : properties) {
			Named container = property.getContainer();

			Assertions
				.notNull(container, "Cannot use a property without a reference to a container inside an index hint.");
			Assertions.isTrue(container instanceof Node, "Index hints can only be applied to nodes.");

			List<NodeLabel> labels = ((Node) container).getLabels();
			Assertions.isTrue(labels.size() == 1, "Exactly one label is required to define the index.");

			Assertions.isTrue(property.getNames().size() == 1,
				"One single property is required. Nested properties are not supported.");

			NodeLabel label = labels.get(0);
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

	static Hint useScanFor(Node node) {

		Assertions.notNull(node, "Cannot apply a SCAN hint without a node.");
		List<NodeLabel> labels = node.getLabels();
		Assertions.isTrue(labels.size() == 1, "Exactly one label is required for a SCAN hint.");

		return new Hint(Type.SCAN,
			Collections.singletonList(new IndexReference(node.getRequiredSymbolicName(), labels.get(0))),
			null);
	}

	static Hint useJoinOn(SymbolicName... name) {

		Assertions.notEmpty(name, "At least one name is required to define a JOIN hint.");
		return new Hint(Type.JOIN_ON, Arrays.stream(name).map(IndexReference::new).collect(Collectors.toList()), null);
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
