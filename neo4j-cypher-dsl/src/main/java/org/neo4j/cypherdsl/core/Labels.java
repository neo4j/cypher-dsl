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

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Represents label expressions, that can be in some way combined and may contain dynamic
 * expressions as described <a href=
 * "https://neo4j.com/docs/cypher-manual/current/clauses/match/#dynamic-match">here</a>.
 * Labels are essentially modelled as tree, so that they can be properly rendered. <br/>
 * We are using the term Labels here in accordance with GQL, in which a Label is a quality
 * for both a node and a relationship element.
 *
 * @author Michael J. Simons
 * @since 2025.1.0
 */
public final class Labels implements Visitable {

	@SuppressWarnings("removal")
	@Deprecated(forRemoval = true)
	static Labels of(LabelExpression labelExpression) {
		if (labelExpression == null) {
			return null;
		}
		var type = Type.valueOf(labelExpression.type().name());
		var value = labelExpression.value();
		List<Value> adaptedValue = (value != null)
				? List.of(new Value(Modifier.STATIC, new TypedSubtree<>(value.stream().map(NodeLabel::new).toList()) {
					@Override
					public String separator() {
						return "";
					}
				})) : List.of();
		return new Labels(type, labelExpression.negated(), adaptedValue, Labels.of(labelExpression.lhs()),
				Labels.of(labelExpression.rhs()));
	}

	static Labels exactly(String label) {
		return new Labels(Modifier.STATIC, new NodeLabel(label));
	}

	/**
	 * Returns a new dynamic label expression matching all labels.
	 * @param expression the labels to match
	 * @return a new dynamic label expression matching all labels
	 */
	static Labels all(Expression expression) {
		return new Labels(Modifier.ALL, expression);
	}

	/**
	 * Returns a new dynamic label expression matching any labels.
	 * @param expression the labels to match
	 * @return a new dynamic label expression matching any labels
	 */
	static Labels any(Expression expression) {
		return new Labels(Modifier.ANY, expression);
	}

	/**
	 * Creates a colon conjunction of all values.
	 * @param values the values for the conjunction
	 * @return a new labels expression
	 */
	public static Labels colonConjunction(Collection<Value> values) {
		return new Labels(Type.COLON_CONJUNCTION, false, (values != null) ? List.copyOf(values) : List.of(), null,
				null);
	}

	/** Whether this is a leaf or another node. */
	private final Type type;

	/** A flag if this subtree is negated. */
	private final boolean negated;

	/** The actual value. */
	private final List<Value> value;

	/**
	 * Optional left hand site of this tree.
	 */
	private final Labels lhs;

	/**
	 * Optional right hand site of this tree.
	 */
	private final Labels rhs;

	Labels(Modifier modifier, Visitable labels) {
		this(Type.LEAF, false, List.of(new Value(modifier, labels)), null, null);
	}

	Labels(Type type, boolean negated, List<Value> value, Labels lhs, Labels rhs) {
		this.lhs = lhs;
		this.type = type;
		this.negated = negated;
		this.value = value;
		this.rhs = rhs;
	}

	/**
	 * Create a conjunction.
	 * @param next the expression to add
	 * @return a new expression
	 */
	public Labels and(Labels next) {
		return new Labels(Type.CONJUNCTION, false, null, this, next);
	}

	/**
	 * Create a disjunction.
	 * @param next the expression to add
	 * @return a new expression
	 */
	public Labels or(Labels next) {
		return new Labels(Type.DISJUNCTION, false, null, this, next);
	}

	/**
	 * Negates this expression.
	 * @return a new expression
	 */
	public Labels negate() {
		return new Labels(this.type, !this.negated, this.value, this.lhs, this.rhs);
	}

	/**
	 * Creates a colon based conjunction.
	 * @param other the other labels to create a conjunction with
	 * @return a new labels expression
	 */
	public Labels conjunctionWith(Labels other) {
		return colonJunction(other, Type.COLON_CONJUNCTION);
	}

	public Labels disjunctionWith(Labels other) {
		return colonJunction(other, Type.COLON_DISJUNCTION);
	}

	private Labels colonJunction(Labels other, Type colonDisjunction) {
		var hlp = new ArrayList<Value>(this.value.size() + other.getValue().size());
		hlp.addAll(this.value);
		hlp.addAll(other.value);
		return new Labels(colonDisjunction, false, List.copyOf(hlp), null, null);
	}

	/**
	 * {@return the optional left hand side of this node}
	 */
	public Labels getLhs() {
		return this.lhs;
	}

	/**
	 * {@return true if this is a negated expression}
	 */
	public boolean isNegated() {
		return this.negated;
	}

	/**
	 * {@return the optional right hand side of this node}
	 */
	public Labels getRhs() {
		return this.rhs;
	}

	public Type getType() {
		return this.type;
	}

	public List<Value> getValue() {
		return this.value;
	}

	/**
	 * {@return the list of all static labels}
	 */
	public Collection<String> getStaticValues() {

		var staticValues = new LinkedHashSet<String>();
		collectLabels(this, staticValues);
		return staticValues;
	}

	private static void collectLabels(Labels l, Set<String> labels) {
		if (l == null) {
			return;
		}
		var current = l.getType();
		collectLabels(l.getLhs(), labels);
		if (current == Labels.Type.LEAF || (l.getLhs() == null && l.getRhs() == null
				&& EnumSet.of(Type.COLON_CONJUNCTION, Type.COLON_DISJUNCTION).contains(l.getType()))) {
			l.getValue().forEach(v -> v.accept(segment -> {
				if (segment instanceof NodeLabel label) {
					labels.add(label.getValue());
				}
			}));
		}
		collectLabels(l.getRhs(), labels);
	}

	boolean canBeUsedInUpdate() {
		var b = this.lhs == null && this.rhs == null
				&& EnumSet.of(Type.LEAF, Type.COLON_CONJUNCTION).contains(this.type);
		if (b) {
			b = !this.value.isEmpty() && this.value.get(0).modifier == Modifier.ALL;
		}
		return b;
	}

	public boolean isEmpty() {
		return (this.value == null || this.value.isEmpty()) && (this.lhs == null || this.lhs.isEmpty())
				&& (this.rhs == null || this.rhs.isEmpty());
	}

	/**
	 * An enum describing whether a {@link Labels} should match all or any labels the
	 * expression resolves to.
	 */
	public enum Modifier {

		/** Dynamically matching all labels. */
		ALL,
		/** Dynamically matching any label. */
		ANY,
		/** Static label expression. */
		STATIC

	}

	/**
	 * The content of this expression. Depending on the modifier, the expression can be
	 * dynamically or statically matched. The actual {@code value} must resolve to an
	 * expression that either is
	 * <ul>
	 * <li>A single string</li>
	 * <li>A list of strings</li>
	 * <li>A parameter holding either a single or a list of strings</li>
	 * </ul>
	 *
	 * @param modifier modifier for the given content
	 * @param visitable the actual value
	 */
	public record Value(Modifier modifier, Visitable visitable) implements Visitable {
		@Override
		public void accept(Visitor visitor) {
			visitor.enter(this);
			this.visitable.accept(visitor);
			visitor.leave(this);
		}
	}

	/**
	 * Type of this expression.
	 */
	public enum Type {

		/**
		 * A leaf.
		 */
		LEAF(""),
		/**
		 * A list of values, conjugated.
		 */
		COLON_CONJUNCTION(":"),
		/**
		 * A list of values, disjoined.
		 */
		COLON_DISJUNCTION(":"),
		/**
		 * A conjunction.
		 */
		CONJUNCTION("&"),
		/**
		 * A disjunction.
		 */
		DISJUNCTION("|");

		private final String value;

		Type(String value) {
			this.value = value;
		}

		/**
		 * {@return a representation of this type}
		 */
		public String getValue() {
			return this.value;
		}

	}

}
