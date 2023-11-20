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

import org.neo4j.cypherdsl.core.ast.Visitable;

/**
 * Quantifier for paths.
 *
 * @author Michael J. Simons
 * @since 2023.9.0
 */
public sealed interface Quantifier extends Visitable {

	static Quantifier interval(Integer lowerBound, Integer upperBound) {

		return new IntervalPathQuantifier(lowerBound, upperBound);
	}

	/**
	 * Qualifier for an interval.
	 *
	 * @param lowerBound the lower bound to use
	 * @param upperBound the upper bound to use
	 */
	record IntervalPathQuantifier(Integer lowerBound, Integer upperBound) implements Quantifier {

		public IntervalPathQuantifier {
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
	}
}
