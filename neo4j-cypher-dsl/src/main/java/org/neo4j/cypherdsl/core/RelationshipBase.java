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

import java.util.Map;
import java.util.Optional;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * This is the base class for all relationships. It can be used with generics, specifying
 * valid start and end nodes. This is useful when using it as a base class for a static
 * metamodel.
 *
 * @param <S> the type at the start of the relationship
 * @param <E> the type at the pointy end of the relationship
 * @param <SELF> the type of the persistent relationship itself
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = STABLE, since = "2021.1.0")
public abstract class RelationshipBase<S extends NodeBase<?>, E extends NodeBase<?>, SELF extends RelationshipBase<S, E, SELF>>
		extends AbstractPropertyContainer implements Relationship {

	final Node left;

	final Node right;

	final Details details;

	final QuantifiedPathPattern.Quantifier quantifier;

	// ------------------------------------------------------------------------
	// Public API to be used by the static metamodel.
	// Non-final methods are ok to be overwritten.
	// ------------------------------------------------------------------------

	/**
	 * Always creates a relationship from start to end (left to right).
	 * @param start start node
	 * @param end end node
	 * @param type type of the relationship
	 * @param additionalTypes additional types to add to the relationship
	 */
	protected RelationshipBase(S start, String type, E end, String... additionalTypes) {

		this(null, start, Direction.LTR, null, end, mergeTypesIfNecessary(type, additionalTypes));
	}

	/**
	 * Always creates a relationship from start to end (left to right).
	 * @param symbolicName an optional symbolic name
	 * @param start start node
	 * @param properties the properties for the relationship
	 * @param end end node
	 * @param type type of the relationship
	 * @param additionalTypes additional types to be added to the relationship, only
	 * meaningful when the object is used for querying, when used in a {@code CREATE} or
	 * {@code MERGE} clause the runtime will throw an exception.
	 */
	protected RelationshipBase(SymbolicName symbolicName, Node start, String type, Properties properties, Node end,
			String... additionalTypes) {

		this(symbolicName, start, Direction.LTR, properties, null, end, mergeTypesIfNecessary(type, additionalTypes));
	}

	/**
	 * Always creates a relationship from start to end (left to right).
	 * @param symbolicName an optional symbolic name
	 * @param start start node
	 * @param properties the properties for the relationship
	 * @param end end node
	 * @param type type of the relationship
	 */
	protected RelationshipBase(SymbolicName symbolicName, String type, Node start, Properties properties, Node end) {
		this(symbolicName, start, Direction.LTR, properties, null, end, type);
	}

	RelationshipBase(SymbolicName symbolicName, Node left, Direction direction,
			QuantifiedPathPattern.Quantifier quantifier, Node right, String... types) {

		this(symbolicName, left, direction, null, quantifier, right, types);
	}

	RelationshipBase(SymbolicName symbolicName, Node left, Direction direction, Properties properties,
			QuantifiedPathPattern.Quantifier quantifier, Node right, String... types) {

		this(left, Details.create(direction, symbolicName, types).with(properties), quantifier, right);
	}

	RelationshipBase(Node left, Details details, QuantifiedPathPattern.Quantifier quantifier, Node right) {

		Assertions.notNull(left, "Left node is required.");
		Assertions.notNull(details, "Details are required.");
		Assertions.notNull(right, "Right node is required.");

		this.left = left;
		this.right = right;
		this.details = details;
		this.quantifier = quantifier;
	}

	private static String[] mergeTypesIfNecessary(String type, String... additionalTypes) {

		if (additionalTypes != null && additionalTypes.length > 0) {
			String[] result = new String[1 + additionalTypes.length];
			result[0] = type;
			System.arraycopy(additionalTypes, 0, result, 1, additionalTypes.length);
			return result;
		}
		return new String[] { type };
	}

	@Override
	public final SELF named(String newSymbolicName) {

		Assertions.hasText(newSymbolicName, "Symbolic name is required.");
		return named(SymbolicName.of(newSymbolicName));
	}

	/**
	 * This method needs to be implemented to provide new, type safe instances of this
	 * relationship.
	 * @param newSymbolicName the new symbolic name.
	 * @return a new relationship
	 */
	@Override
	public abstract SELF named(SymbolicName newSymbolicName);

	@Override
	public final SELF withProperties(Object... keysAndValues) {

		MapExpression newProperties = null;
		if (keysAndValues != null && keysAndValues.length != 0) {
			newProperties = MapExpression.create(false, keysAndValues);
		}
		return withProperties(newProperties);
	}

	@Override
	public final SELF withProperties(Map<String, Object> newProperties) {

		return withProperties(MapExpression.create(newProperties));
	}

	/**
	 * This method needs to be implemented to provide new, type safe instances of this
	 * relationship.
	 * @param newProperties the new properties (can be {@literal null} to remove exiting
	 * properties).
	 * @return a new relationship
	 */
	@Override
	public abstract SELF withProperties(MapExpression newProperties);

	@Override
	public final Node getLeft() {
		return this.left;
	}

	@Override
	public final Node getRight() {
		return this.right;
	}

	@Override
	public final QuantifiedPathPattern.Quantifier getQuantifier() {
		return this.quantifier;
	}

	@Override
	public final Details getDetails() {
		return this.details;
	}

	@Override
	public final Relationship unbounded() {

		return new InternalRelationshipImpl(this.left, this.details.unbounded(), this.quantifier, this.right);
	}

	@Override
	public final Relationship min(Integer minimum) {

		return new InternalRelationshipImpl(this.left, this.details.min(minimum), this.quantifier, this.right);
	}

	@Override
	public final Relationship max(Integer maximum) {

		return new InternalRelationshipImpl(this.left, this.details.max(maximum), this.quantifier, this.right);
	}

	@Override
	public final Relationship length(Integer minimum, Integer maximum) {

		return new InternalRelationshipImpl(this.left, this.details.min(minimum).max(maximum), this.quantifier,
				this.right);
	}

	@Override
	public final Relationship inverse() {

		return new InternalRelationshipImpl(this.right, this.details.inverse(), this.quantifier, this.left);
	}

	@Override
	public final Optional<SymbolicName> getSymbolicName() {
		return this.details.getSymbolicName();
	}

	@Override
	public final SymbolicName getRequiredSymbolicName() {
		return this.details.getRequiredSymbolicName();
	}

	@Override
	public final RelationshipChain relationshipTo(Node other, String... types) {
		return RelationshipChain.create(this).add(this.right.relationshipTo(other, types));
	}

	// ------------------------------------------------------------------------
	// Internal API.
	// ------------------------------------------------------------------------

	@Override
	public final RelationshipChain relationshipFrom(Node other, String... types) {
		return RelationshipChain.create(this).add(this.right.relationshipFrom(other, types));
	}

	@Override
	public final RelationshipChain relationshipBetween(Node other, String... types) {
		return RelationshipChain.create(this).add(this.right.relationshipBetween(other, types));
	}

	@Override
	public final Condition asCondition() {
		return RelationshipPatternCondition.of(this);
	}

	@Override
	public final void accept(Visitor visitor) {

		visitor.enter(this);

		this.left.accept(visitor);
		this.details.accept(visitor);
		Visitable.visitIfNotNull(this.quantifier, visitor);
		this.right.accept(visitor);

		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	@Override
	public Relationship where(Expression predicate) {
		if (predicate == null) {
			return this;
		}
		return new InternalRelationshipImpl(this.left, this.details.where(predicate), this.quantifier, this.right);
	}

	@Override
	public RelationshipPattern quantifyRelationship(QuantifiedPathPattern.Quantifier newQuantifier) {
		if (newQuantifier == null) {
			return this;
		}

		return new InternalRelationshipImpl(this.left, this.details, newQuantifier, this.right);
	}

	@Override
	public QuantifiedPathPattern quantify(QuantifiedPathPattern.Quantifier newQuantifier) {

		return QuantifiedPathPattern.of(this, newQuantifier);
	}

}
