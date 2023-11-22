/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * @author Michael J. Simons
 * @since 2023.9.0
 */
@Neo4jVersion(minimum = "5.9")
@API(status = STABLE, since = "2023.9.0")
public final class QuantifiedPathPattern implements PatternElement {

	/**
	 * Quantifier for path patterns.
	 */
	public sealed interface Quantifier extends Visitable {
	}

	/**
	 * Creates an interval quantifier
	 * @param lowerBound lower bound, must be greater than or equal to 0
	 * @param upperBound upper bound, must be greater than or equal to the lower bound
	 * @return a quantifier
	 */
	public static Quantifier interval(Integer lowerBound, Integer upperBound) {

		return new IntervalQuantifier(lowerBound, upperBound);
	}

	/**
	 * {@return the <code>+</code> quantifier}
	 */
	public static Quantifier plus() {

		return PlusQuantifier.INSTANCE;
	}

	/**
	 * {@return the <code>*</code> quantifier}
	 */
	public static Quantifier star() {

		return StarQuantifier.INSTANCE;
	}

	private final TargetPattern delegate;

	private final Quantifier quantifier;

	static QuantifiedPathPattern of(PatternElement patternElement, @Nullable Quantifier quantifier) {

		var delegate = patternElement instanceof TargetPattern ppp ? ppp : new TargetPattern(patternElement, null);

		return new QuantifiedPathPattern(delegate, quantifier);
	}

	private QuantifiedPathPattern(TargetPattern delegate, Quantifier quantifier) {
		this.delegate = delegate;
		this.quantifier = quantifier;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.delegate.accept(visitor);
		Visitable.visitIfNotNull(quantifier, visitor);
		visitor.leave(this);
	}

	@Override
	public @NotNull PatternElement where(@Nullable Expression predicate) {
		if (predicate == null) {
			return this;
		}
		return of(delegate.where(predicate), quantifier);
	}

	/**
	 * Synthetic element for the Cypher-DSL AST.
	 */
	@API(status = API.Status.INTERNAL)
	public static final class TargetPattern implements PatternElement {

		private final PatternElement delegate;

		@Nullable
		private final Where innerPredicate;

		private TargetPattern(PatternElement delegate, @Nullable Where innerPredicate) {
			this.delegate = delegate;
			this.innerPredicate = innerPredicate;
		}

		@Override
		public void accept(Visitor visitor) {

			visitor.enter(this);
			this.delegate.accept(visitor);
			Visitable.visitIfNotNull(this.innerPredicate, visitor);
			visitor.leave(this);
		}

		@Override
		public @NotNull PatternElement where(@Nullable Expression predicate) {
			if (predicate == null) {
				return this;
			}
			return new TargetPattern(this.delegate, Where.from(predicate));
		}
	}

	/**
	 * Qualifier for an interval.
	 *
	 * @param lowerBound the lower bound to use
	 * @param upperBound the upper bound to use
	 */
	private record IntervalQuantifier(Integer lowerBound, Integer upperBound) implements Quantifier {

		public IntervalQuantifier {
			if (lowerBound != null && lowerBound < 0) {
				throw new IllegalArgumentException("Lower bound must be greater than or equal to zero");
			}
			if (upperBound != null && upperBound <= 0) {
				throw new IllegalArgumentException("Upper bound must be greater than zero");
			}
			if (lowerBound != null && upperBound != null && upperBound < lowerBound) {
				throw new IllegalArgumentException("Upper bound must be greater than or equal to " + lowerBound);
			}
		}

		@Override
		public String toString() {
			var result = "{";
			result += (lowerBound() == null ? "0" : lowerBound());
			result += ",";
			if (upperBound() != null) {
				result += upperBound();
			}
			result += "}";
			return result;
		}
	}

	/**
	 * Specialized quantifier for 1 or more iterations ({@literal +} quantifier).
	 */
	private enum PlusQuantifier implements Quantifier {
		INSTANCE;

		@Override
		public String toString() {
			return "+";
		}
	}

	/**
	 * Specialized quantifier for 0 or more iterations ({@literal *} quantifier).
	 */
	private enum StarQuantifier implements Quantifier {
		INSTANCE;

		@Override
		public String toString() {
			return "*";
		}
	}
}
