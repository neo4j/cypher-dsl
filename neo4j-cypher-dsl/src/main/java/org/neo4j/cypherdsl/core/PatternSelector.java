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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;

import static org.apiguardian.api.API.Status.STABLE;

/**
 * A type resenting the {@code SHORTEST} keyword and it's variants.
 *
 * @author Michael J. Simons
 * @since 2024.7.0
 *
 */
@API(status = STABLE, since = "2024.7.0")
public sealed interface PatternSelector extends Visitable {

	// Yes, we could use an enum here, but that would lead to two instances with a k, two
	// without,
	// hence, sealed classes are better as they model what we want to express, and in the
	// visitor
	// we can use switch with pattern matching on classes at some point in the future for
	// sure.

	static PatternSelector shortestK(int k) {
		if (k <= 0) {
			throw new IllegalArgumentException("The path count needs to be greater than 0.");
		}
		return new ShortestK(k);
	}

	static PatternSelector allShortest() {
		return new AllShortest();
	}

	static PatternSelector shortestKGroups(int k) {
		if (k <= 0) {
			throw new IllegalArgumentException("The path count needs to be greater than 0.");
		}
		return new ShortestKGroups(k);
	}

	static PatternSelector any() {
		return new Any();
	}

	/**
	 * Representing {@code SHORTEST K}.
	 */
	final class ShortestK implements PatternSelector {

		private final int k;

		public ShortestK(Integer k) {
			this.k = k;
		}

		public int getK() {
			return this.k;
		}

	}

	/**
	 * Representing {@code ALL SHORTEST}.
	 */
	final class AllShortest implements PatternSelector {

		private AllShortest() {
		}

	}

	/**
	 * Representing {@code ANY}.
	 */
	final class Any implements PatternSelector {

		private Any() {
		}

	}

	/**
	 * Representing {@code SHORTEST K GROUPS}.
	 */
	final class ShortestKGroups implements PatternSelector {

		private final int k;

		ShortestKGroups(Integer k) {
			this.k = k;
		}

		public int getK() {
			return this.k;
		}

	}

}
