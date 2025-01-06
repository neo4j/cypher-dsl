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
package org.neo4j.cypherdsl.parser;

import org.neo4j.cypherdsl.core.ExposesPatternLengthAccessors;
import org.neo4j.cypherdsl.core.ExposesProperties;
import org.neo4j.cypherdsl.core.ExposesRelationships;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.QuantifiedPathPattern;
import org.neo4j.cypherdsl.core.Relationship;
import org.neo4j.cypherdsl.core.Relationship.Direction;
import org.neo4j.cypherdsl.core.RelationshipChain;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * A value object for the details of a path.
 *
 * @author Michael J. Simons
 * @soundtrack Pink Floyd - The Division Bell
 * @since 2021.3.0
 */
final class PathAtom implements PatternAtom {

	@SuppressWarnings("squid:S107") // Totally fine with that number of args for this internal API.
	static PathAtom of(SymbolicName name, PathLength length, boolean left, boolean right,
		String[] relTypes, MapExpression properties, boolean negatedType, Expression predicate) {

		if (left && right) {
			throw new IllegalArgumentException("Only left-to-right, right-to-left or unidirectional path elements are supported.");
		}

		Direction direction;
		if (left) {
			direction = Direction.RTL;
		} else if (right) {
			direction = Direction.LTR;
		} else {
			direction = Direction.UNI;
		}

		return new PathAtom(name, length, direction, negatedType, relTypes, properties, predicate, null);
	}

	private final SymbolicName name;

	private final PathLength length;

	private final Direction direction;

	private final boolean negatedType;

	private final String[] types;

	private final MapExpression properties;

	private final Expression predicate;

	private final QuantifiedPathPattern.Quantifier quantifier;

	private PathAtom(SymbolicName name, PathLength length, Direction direction, boolean negatedType, String[] types,
		MapExpression properties, Expression predicate, QuantifiedPathPattern.Quantifier quantifier) {
		this.name = name;
		this.length = length;
		this.direction = direction;
		this.negatedType = negatedType;
		this.types = types;
		this.properties = properties;
		this.predicate = predicate;
		this.quantifier = quantifier;
	}

	ExposesRelationships<?> asRelationshipBetween(ExposesRelationships<?> previous, NodeAtom nodeAtom, boolean alwaysLtr) {
		var node = nodeAtom.value();
		ExposesRelationships<?> relationshipPattern = switch (this.getDirection()) {
			case LTR -> previous.relationshipTo(node, this.getTypes());
			case RTL -> alwaysLtr ?
				node.relationshipTo((Node) previous, this.getTypes()) :
				previous.relationshipFrom(node, this.getTypes());
			case UNI -> previous.relationshipBetween(node, this.getTypes());
		};
		relationshipPattern = applyOptionalName(relationshipPattern);
		relationshipPattern = applyOptionalProperties(relationshipPattern);
		relationshipPattern = applyOptionalPredicate(relationshipPattern);
		relationshipPattern = applyOptionalLength(relationshipPattern);
		return applyOptionalQuantifier(relationshipPattern);
	}

	private ExposesRelationships<?> applyOptionalLength(ExposesRelationships<?> relationshipPattern) {
		if (length == null) {
			return relationshipPattern;
		}
		if (length.isUnbounded()) {
			return ((ExposesPatternLengthAccessors<?>) relationshipPattern).unbounded();
		}
		return ((ExposesPatternLengthAccessors<?>) relationshipPattern).length(length.getMinimum(), length.getMaximum());
	}

	private ExposesRelationships<?> applyOptionalProperties(ExposesRelationships<?> relationshipPattern) {
		if (properties == null) {
			return relationshipPattern;
		}
		if (relationshipPattern instanceof ExposesProperties<?> exposesProperties) {
			return (ExposesRelationships<?>) exposesProperties.withProperties(properties);
		}
		return ((RelationshipChain) relationshipPattern).properties(properties);
	}

	private ExposesRelationships<?> applyOptionalName(ExposesRelationships<?> relationshipPattern) {
		if (name == null) {
			return relationshipPattern;
		}
		if (relationshipPattern instanceof Relationship relationship) {
			return relationship.named(name);
		}
		return ((RelationshipChain) relationshipPattern).named(name);
	}

	private ExposesRelationships<?> applyOptionalPredicate(ExposesRelationships<?> relationshipPattern) {
		if (predicate == null) {
			return relationshipPattern;
		}
		if (relationshipPattern instanceof Relationship relationship) {
			return (ExposesRelationships<?>) relationship.where(predicate);
		}
		return ((RelationshipChain) relationshipPattern).where(predicate);
	}

	private ExposesRelationships<?> applyOptionalQuantifier(ExposesRelationships<?> relationshipPattern) {
		if (quantifier == null) {
			return relationshipPattern;
		}
		if (relationshipPattern instanceof Relationship relationship) {
			return (ExposesRelationships<?>) relationship.quantifyRelationship(quantifier);
		}
		return ((RelationshipChain) relationshipPattern).quantifyRelationship(quantifier);
	}

	public Direction getDirection() {
		return direction;
	}

	public String[] getTypes() {
		return types;
	}

	public MapExpression getProperties() {
		return this.properties;
	}

	public SymbolicName getName() {
		return name;
	}

	public PathAtom withQuantifier(QuantifiedPathPattern.Quantifier newQuantifier) {
		return newQuantifier == null ?
			this :
			new PathAtom(name, length, direction, negatedType, types, properties, predicate, newQuantifier);
	}
}
