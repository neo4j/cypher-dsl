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

import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * @author Michael J. Simons
 * @since 2023.9.0
 */
public final class QuantifiedPathPattern implements PatternElement {

	private final PatternElement delegate;

	private final Quantifier quantifier;

	static QuantifiedPathPattern of(PatternElement patternElement, Quantifier quantifier) {

		var delegate = patternElement instanceof ParenthesizedPathPattern ppp ? ppp : ParenthesizedPathPattern.of(patternElement);

		return new QuantifiedPathPattern(delegate, quantifier);
	}

	private QuantifiedPathPattern(ParenthesizedPathPattern delegate, Quantifier quantifier) {
		this.delegate = delegate;
		this.quantifier = quantifier;
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.delegate.accept(visitor);
		this.quantifier.accept(visitor);
		visitor.leave(this);
	}
}
