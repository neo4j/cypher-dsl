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
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Optional;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * This is the base class for all relationships. It can be used with generics, specifying valid start and end nodes.
 * This is useful when using it as a base class for a static meta model.
 *
 *
 * @param <S> The type at the start of the relationship
 * @param <E> The type at the pointy end of the relationship
 * @param <T> The type of the persistent relationship itself
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public class RelationshipImpl<S extends NodeImpl<?>, E extends NodeImpl<?>, T extends RelationshipImpl<S, E, T>>
		extends AbstractPropertyContainer implements Relationship {

	private final Node left;

	private final Node right;

	private final Details details;

	// ------------------------------------------------------------------------
	// Public API to be used by the static meta model.
	// Non-final methods are ok to be overwritten.
	// ------------------------------------------------------------------------

	/**
	 * Always creates a relationship from start to end (left to right).
	 *
	 * @param start start node
	 * @param end   end node
	 * @param type  type of the relationship
	 */
	protected RelationshipImpl(S start, String type, E end) {

		this(null, start, Direction.LTR, end, type);
	}

	/**
	 * Always creates a relationship from start to end (left to right).
	 *
	 * @param symbolicName an optional symbolic name
	 * @param start        start node
	 * @param end          end node
	 * @param type         type of the relationship
	 */
	protected RelationshipImpl(SymbolicName symbolicName, Node start, String type, Properties properties, Node end) {

		this(symbolicName, start, Direction.LTR, properties, end, type);
	}

	@Override
	public final T named(String newSymbolicName) {

		Assertions.hasText(newSymbolicName, "Symbolic name is required.");
		return named(SymbolicName.of(newSymbolicName));
	}

	@Override
	@SuppressWarnings("unchecked")
	public T named(SymbolicName newSymbolicName) {

		// Sanity check of newSymbolicName delegated to the details.
		return (T) new RelationshipImpl(this.left, this.details.named(newSymbolicName), this.right);
	}

	@Override
	public final T withProperties(Object... keysAndValues) {

		MapExpression newProperties = null;
		if (keysAndValues != null && keysAndValues.length != 0) {
			newProperties = MapExpression.create(keysAndValues);
		}
		return withProperties(newProperties);
	}

	@Override
	public final T withProperties(Map<String, Object> newProperties) {

		return withProperties(MapExpression.create(newProperties));
	}

	@Override
	@SuppressWarnings("unchecked")
	public T withProperties(MapExpression newProperties) {

		return (T) new RelationshipImpl(this.left, this.details.with(Properties.create(newProperties)), this.right);
	}

	@Override
	public final Node getLeft() {
		return left;
	}

	protected final String getRequiredType() {

		List<String> types = getDetails().getTypes().getValues();
		if (types.size() != 1) {
			throw new NoSuchElementException("No value present");
		}
		return types.get(0);
	}

	@Override
	public final Node getRight() {
		return right;
	}

	@Override
	public final Details getDetails() {
		return details;
	}

	@Override
	public final Relationship unbounded() {

		return new RelationshipImpl(this.left, this.details.unbounded(), this.right);
	}

	@Override
	public final Relationship min(Integer minimum) {

		return new RelationshipImpl(this.left, this.details.min(minimum), this.right);
	}

	@Override
	public final Relationship max(Integer maximum) {

		return new RelationshipImpl(this.left, this.details.max(maximum), this.right);
	}

	@Override
	public final Relationship length(Integer minimum, Integer maximum) {

		return new RelationshipImpl(this.left, this.details.min(minimum).max(maximum), this.right);
	}

	@Override
	public final Optional<SymbolicName> getSymbolicName() {
		return details.getSymbolicName();
	}

	@Override
	public final SymbolicName getRequiredSymbolicName() {
		return details.getRequiredSymbolicName();
	}

	@Override
	public final RelationshipChain relationshipTo(Node other, String... types) {
		return RelationshipChain
				.create(this)
				.add(this.right.relationshipTo(other, types));
	}

	@Override
	public final RelationshipChain relationshipFrom(Node other, String... types) {
		return RelationshipChain
				.create(this)
				.add(this.right.relationshipFrom(other, types));
	}

	@Override
	public final RelationshipChain relationshipBetween(Node other, String... types) {
		return RelationshipChain
				.create(this)
				.add(this.right.relationshipBetween(other, types));
	}

	@Override
	public final Condition asCondition() {
		return new RelationshipPatternCondition(this);
	}

	// ------------------------------------------------------------------------
	// Internal API.
	// ------------------------------------------------------------------------

	RelationshipImpl(SymbolicName symbolicName, Node left, Direction direction, Node right, String... types) {

		this(symbolicName, left, direction, null, right, types);
	}

	RelationshipImpl(SymbolicName symbolicName, Node left, Direction direction, Properties properties, Node right, String... types) {

		this(left, Details.create(direction, symbolicName, types).with(properties), right);
	}

	RelationshipImpl(Node left, Details details, Node right) {

		Assertions.notNull(left, "Left node is required.");
		Assertions.notNull(details, "Details are required.");
		Assertions.notNull(right, "Right node is required.");

		this.left = left;
		this.right = right;
		this.details = details;
	}

	@Override
	public final void accept(Visitor visitor) {

		visitor.enter(this);

		left.accept(visitor);
		details.accept(visitor);
		right.accept(visitor);

		visitor.leave(this);
	}
}
