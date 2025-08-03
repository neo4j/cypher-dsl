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

import static org.apiguardian.api.API.Status.STABLE;

import java.util.LinkedList;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.internal.RelationshipPatternCondition;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Represents a chain of relationships. The chain is meant to be in order and the right node of an element is related to
 * the left node of the next element.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class RelationshipChain implements RelationshipPattern, ExposesPatternLengthAccessors<RelationshipChain> {

	private final LinkedList<Relationship> relationships;

	static RelationshipChain create(Relationship firstElement) {

		return new RelationshipChain(firstElement);
	}

	private RelationshipChain(Relationship firstElement) {
		this.relationships = new LinkedList<>();
		this.relationships.add(firstElement);
	}

	private RelationshipChain(List<Relationship> firstElements, Relationship lastElement) {
		this.relationships = new LinkedList<>(firstElements);
		this.relationships.add(lastElement);
	}

	private RelationshipChain(List<Relationship> elements) {
		this.relationships = new LinkedList<>(elements);
	}

	RelationshipChain add(Relationship element) {

		Assertions.notNull(element, "Elements of a relationship chain must not be null.");
		return new RelationshipChain(this.relationships, element);
	}

	RelationshipChain replaceLast(Relationship element) {

		Assertions.notNull(element, "Elements of a relationship chain must not be null.");
		RelationshipChain newChain = new RelationshipChain(this.relationships);
		newChain.relationships.removeLast();
		newChain.relationships.add(element);
		return newChain;
	}

	@Override
	public RelationshipChain relationshipTo(Node other, String... types) {
		return this.add(this.relationships.getLast().getRight().relationshipTo(other, types));
	}

	@Override
	public RelationshipChain relationshipFrom(Node other, String... types) {
		return this.add(this.relationships.getLast().getRight().relationshipFrom(other, types));
	}

	@Override
	public RelationshipChain relationshipBetween(Node other, String... types) {
		return this.add(this.relationships.getLast().getRight().relationshipBetween(other, types));
	}

	/**
	 * Replaces the last element of this chains with a copy of the relationship with the new symbolic name.
	 *
	 * @param newSymbolicName The new symbolic name to use
	 * @return A new chain
	 */
	@Override
	public RelationshipChain named(String newSymbolicName) {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.named(newSymbolicName));
	}

	/**
	 * Replaces the last element of this chains with a copy of the relationship with the new symbolic name.
	 *
	 * @param newSymbolicName The new symbolic name to use
	 * @return A new chain
	 * @since 2021.1.1
	 */
	@Override
	public RelationshipChain named(SymbolicName newSymbolicName) {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.named(newSymbolicName));
	}

	@Override
	public RelationshipChain where(Expression predicate) {

		if (predicate == null) {
			return this;
		}

		var lastElement = this.relationships.getLast();
		return this.replaceLast((Relationship) lastElement.where(predicate));
	}

	@Override
	public RelationshipPattern quantifyRelationship(QuantifiedPathPattern.Quantifier quantifier) {

		if (quantifier == null) {
			return this;
		}

		var lastElement = this.relationships.getLast();
		return this.replaceLast((Relationship) lastElement.quantifyRelationship(quantifier));
	}

	@Override
	public QuantifiedPathPattern quantify(QuantifiedPathPattern.Quantifier newQuantifier) {

		return QuantifiedPathPattern.of(this, newQuantifier);
	}

	/**
	 * Changes the length of the last element of this chain to an unbounded pattern.
	 *
	 * @return A new chain
	 * @since 1.1.1
	 */
	@Override
	public RelationshipChain unbounded() {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.unbounded());
	}

	/**
	 * Changes the length of the last element of this chain to a new minimum length
	 *
	 * @param minimum the new minimum
	 * @return A new chain
	 */
	@Override
	public RelationshipChain min(Integer minimum) {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.min(minimum));
	}

	/**
	 * Changes the length of the last element of this chain to a new maximum length
	 *
	 * @param maximum the new maximum
	 * @return A new chain
	 */
	@Override
	public RelationshipChain max(Integer maximum) {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.max(maximum));
	}

	/**
	 * Changes the length of the last element of this chain
	 *
	 * @param minimum the new minimum
	 * @param maximum the new maximum
	 * @return A new chain
	 */
	@Override
	public RelationshipChain length(Integer minimum, Integer maximum) {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.length(minimum, maximum));
	}

	/**
	 * Adds properties to the last element of this chain.
	 *
	 * @param newProperties the new properties (can be {@literal null} to remove exiting properties).
	 * @return A new chain
	 */
	public RelationshipChain properties(MapExpression newProperties) {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.withProperties(newProperties));
	}

	/**
	 * Adds properties to the last element of this chain.
	 *
	 * @param keysAndValues A list of key and values. Must be an even number, with alternating {@link String} and {@link Expression}.
	 * @return A new chain
	 */
	public RelationshipChain properties(Object... keysAndValues) {

		Relationship lastElement = this.relationships.getLast();
		return this.replaceLast(lastElement.withProperties(keysAndValues));
	}

	@Override
	public Condition asCondition() {
		return RelationshipPatternCondition.of(this);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);

		Node lastNode = null;
		for (Relationship relationship : relationships) {

			visitor.enter(relationship);
			relationship.getLeft().accept(visitor);
			relationship.getDetails().accept(visitor);
			Visitable.visitIfNotNull(relationship.getQuantifier(), visitor);
			visitor.leave(relationship);

			lastNode = relationship.getRight();
		}

		Visitable.visitIfNotNull(lastNode, visitor);

		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}
}
