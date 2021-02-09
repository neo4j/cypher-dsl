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
import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/railroad/RelationshipPattern.html">RelationshipPattern</a>.
 *
 * @author Michael J. Simons
 * @author Philipp Tölle
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class Relationship implements RelationshipPattern, PropertyContainer, ExposesProperties<Relationship> {

	/**
	 * While the direction in the schema package is centered around the node, the direction here is the direction between two nodes.
	 *
	 * @since 1.0
	 */
	public enum Direction {
		/**
		 * Left to right
		 */
		LTR("-", "->"),
		/**
		 * Right to left
		 */
		RTL("<-", "-"),
		/**
		 * None
		 */
		UNI("-", "-");

		Direction(String symbolLeft, String symbolRight) {
			this.symbolLeft = symbolLeft;
			this.symbolRight = symbolRight;
		}

		private final String symbolLeft;

		private final String symbolRight;

		/**
		 * @return The symbol to render on the left side of the relationship types.
		 */
		@API(status = INTERNAL)
		public String getSymbolLeft() {
			return symbolLeft;
		}

		/**
		 * @return The symbol to render on the right side of the relationship types.
		 */
		@API(status = INTERNAL)
		public String getSymbolRight() {
			return symbolRight;
		}
	}

	/**
	 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/railroad/RelationshipDetail.html">RelationshipDetail</a>.
	 * This is not a public API and just used internally for structuring the tree.
	 */
	@API(status = INTERNAL, since = "1.0")
	public static final class Details implements Visitable {

		/**
		 * The direction between the nodes of the relationship.
		 */
		private final Direction direction;

		private volatile SymbolicName symbolicName;

		private final RelationshipTypes types;

		private final RelationshipLength length;

		private final Properties properties;

		static Details create(Direction direction, SymbolicName symbolicName,
			RelationshipTypes types) {

			return new Details(direction, symbolicName, types, null, null);
		}

		private Details(Direction direction,
			SymbolicName symbolicName,
			RelationshipTypes types,
			RelationshipLength length,
			Properties properties
		) {

			this.direction = direction;
			this.symbolicName = symbolicName;
			this.types = types;
			this.length = length;
			this.properties = properties;
		}

		/**
		 * Internal helper method indicating whether the details have content or not.
		 *
		 * @return true if any of the details are filled
		 */
		public boolean hasContent() {
			return this.symbolicName != null || this.types != null || this.length != null || this.properties != null;
		}

		Details named(String newSymbolicName) {

			Assertions.hasText(newSymbolicName, "Symbolic name is required.");
			return named(SymbolicName.of(newSymbolicName));
		}

		Details named(SymbolicName newSymbolicName) {

			Assertions.notNull(newSymbolicName, "Symbolic name is required.");
			return new Details(this.direction, newSymbolicName, this.types, this.length, this.properties);
		}

		Details with(Properties newProperties) {

			return new Details(this.direction, this.symbolicName, this.types, this.length, newProperties);
		}

		Details unbounded() {

			return new Details(this.direction, this.symbolicName, this.types, new RelationshipLength(),
				properties);
		}

		Details min(Integer minimum) {

			if (minimum == null && (this.length == null || this.length.getMinimum() == null)) {
				return this;
			}

			RelationshipLength newLength = Optional.ofNullable(this.length)
				.map(l -> new RelationshipLength(minimum, l.getMaximum()))
				.orElseGet(() -> new RelationshipLength(minimum, null));

			return new Details(this.direction, this.symbolicName, this.types, newLength, properties);
		}

		Details max(Integer maximum) {

			if (maximum == null && (this.length == null || this.length.getMaximum() == null)) {
				return this;
			}

			RelationshipLength newLength = Optional.ofNullable(this.length)
				.map(l -> new RelationshipLength(l.getMinimum(), maximum))
				.orElseGet(() -> new RelationshipLength(null, maximum));

			return new Details(this.direction, this.symbolicName, this.types, newLength, properties);
		}

		/**
		 * @return The direction of the relationship.
		 */
		@API(status = INTERNAL)
		public Direction getDirection() {
			return direction;
		}

		Optional<SymbolicName> getSymbolicName() {
			return Optional.ofNullable(symbolicName);
		}

		SymbolicName getRequiredSymbolicName() {

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

		/**
		 * @return The relationship types being matched.
		 */
		@API(status = INTERNAL)
		public RelationshipTypes getTypes() {
			return types;
		}

		/**
		 * @return The properties of this relationship.
		 */
		@API(status = INTERNAL)
		public Properties getProperties() {
			return properties;
		}

		@Override
		public void accept(Visitor visitor) {

			visitor.enter(this);
			Visitable.visitIfNotNull(this.symbolicName, visitor);
			Visitable.visitIfNotNull(this.types, visitor);
			Visitable.visitIfNotNull(this.length, visitor);
			Visitable.visitIfNotNull(this.properties, visitor);
			visitor.leave(this);
		}
	}

	static Relationship create(Node left, Direction direction, Node right, String... types) {

		Assertions.notNull(left, "Left node is required.");
		Assertions.notNull(right, "Right node is required.");

		List<String> listOfTypes = Arrays.stream(types)
			.filter(type -> !(type == null || type.isEmpty()))
			.collect(Collectors.toList());

		Details details = Details.create(
			Optional.ofNullable(direction).orElse(Direction.UNI),
			null,
			listOfTypes.isEmpty() ? null : new RelationshipTypes(listOfTypes));
		return new Relationship(left, details, right);
	}

	private final Node left;

	private final Node right;

	private final Details details;

	Relationship(Node left, Details details, Node right) {
		this.left = left;
		this.right = right;
		this.details = details;
	}

	Node getLeft() {
		return left;
	}

	Node getRight() {
		return right;
	}

	/**
	 * The details contains the types, properties and cardinality.
	 *
	 * @return A wrapper around the details of this relationship.
	 */
	@API(status = INTERNAL)
	public Details getDetails() {
		return details;
	}

	/**
	 * Creates a copy of this relationship with a new symbolic name.
	 *
	 * @param newSymbolicName the new symbolic name.
	 * @return The new relationship.
	 */
	public Relationship named(String newSymbolicName) {

		// Sanity check of newSymbolicName delegated to the details.
		return new Relationship(this.left, this.details.named(newSymbolicName), this.right);
	}

	/**
	 * Creates a copy of this relationship with a new symbolic name.
	 *
	 * @param newSymbolicName the new symbolic name.
	 * @return The new relationship.
	 */
	public Relationship named(SymbolicName newSymbolicName) {

		// Sanity check of newSymbolicName delegated to the details.
		return new Relationship(this.left, this.details.named(newSymbolicName), this.right);
	}

	/**
	 * Creates a new relationship with an unbound length minimum length
	 *
	 * @return the new relationship
	 * @since 1.1.1
	 */
	public Relationship unbounded() {

		return new Relationship(this.left, this.details.unbounded(), this.right);
	}

	/**
	 * Creates a new relationship with a new minimum length
	 *
	 * @param minimum the new minimum
	 * @return the new relationship
	 */
	public Relationship min(Integer minimum) {

		return new Relationship(this.left, this.details.min(minimum), this.right);
	}

	/**
	 * Creates a new relationship with a new maximum length
	 *
	 * @param maximum the new maximum
	 * @return the new relationship
	 */
	public Relationship max(Integer maximum) {

		return new Relationship(this.left, this.details.max(maximum), this.right);
	}

	/**
	 * Creates a new relationship with a new length
	 *
	 * @param minimum the new minimum
	 * @param maximum the new maximum
	 * @return the new relationship
	 */
	public Relationship length(Integer minimum, Integer maximum) {

		return new Relationship(this.left, this.details.min(minimum).max(maximum), this.right);
	}

	@Override
	public Relationship withProperties(MapExpression newProperties) {

		if (newProperties == null && this.details.getProperties() == null) {
			return this;
		}
		return new Relationship(this.left,
			this.details.with(newProperties == null ? null : new Properties(newProperties)), this.right);
	}

	@Override
	public Relationship withProperties(Object... keysAndValues) {

		MapExpression newProperties = null;
		if (keysAndValues != null && keysAndValues.length != 0) {
			newProperties = MapExpression.create(keysAndValues);
		}
		return withProperties(newProperties);
	}

	@Override
	public Optional<SymbolicName> getSymbolicName() {
		return details.getSymbolicName();
	}

	@Override
	public SymbolicName getRequiredSymbolicName() {
		return details.getRequiredSymbolicName();
	}

	@Override
	public RelationshipChain relationshipTo(Node other, String... types) {
		return RelationshipChain
			.create(this)
			.add(this.right.relationshipTo(other, types));
	}

	@Override
	public RelationshipChain relationshipFrom(Node other, String... types) {
		return RelationshipChain
			.create(this)
			.add(this.right.relationshipFrom(other, types));
	}

	@Override
	public RelationshipChain relationshipBetween(Node other, String... types) {
		return RelationshipChain
			.create(this)
			.add(this.right.relationshipBetween(other, types));
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);

		left.accept(visitor);
		details.accept(visitor);
		right.accept(visitor);

		visitor.leave(this);
	}

	/**
	 * Creates a map projection based on this relationship. The relationship needs a symbolic name for this to work.
	 * <p>
	 * Entries of type {@code String} in {@code entries} followed by an {@link Expression} will be treated as map keys
	 * pointing to the expression in the projection, {@code String} entries alone will be treated as property lookups on the node.
	 *
	 * @param entries A list of entries for the projection
	 * @return A map projection.
	 */
	public MapProjection project(Object... entries) {
		return MapProjection.create(this.getRequiredSymbolicName(), entries);
	}
}
