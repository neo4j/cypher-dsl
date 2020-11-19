/*
 * Copyright (c) 2019-2020 "Neo4j,"
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

import java.util.Optional;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.FunctionInvocation.FunctionDefinition;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Represents a named named path as in {@code p := (a)-->(b)}.
 *
 * @author Michael J. Simons
 * @soundtrack Freddie Mercury - Never Boring
 * @since 1.1
 */
@API(status = EXPERIMENTAL, since = "1.1")
public final class NamedPath implements PatternElement, Named {

	/**
	 * The name of this path expression.
	 */
	private final SymbolicName name;

	/**
	 * The pattern defining this path.
	 */
	private final Visitable pattern;

	static OngoingDefinitionWithName named(String name) {

		return named(SymbolicName.of(name));
	}

	static OngoingDefinitionWithName named(SymbolicName name) {

		Assertions.notNull(name, "A name is required");
		return new Builder(name);
	}

	static OngoingShortestPathDefinitionWithName named(String name, FunctionDefinition algorithm) {

		return new ShortestPathBuilder(SymbolicName.of(name), algorithm);
	}

	static OngoingShortestPathDefinitionWithName named(SymbolicName name, FunctionDefinition algorithm) {

		Assertions.notNull(name, "A name is required");
		return new ShortestPathBuilder(name, algorithm);
	}

	/**
	 * Partial path that has a name ({@code p = }).
	 */
	public interface OngoingDefinitionWithName {

		/**
		 * Create a new named path based on a relationship pattern.
		 *
		 * @param pattern The pattern to be used as named path.
		 * @return A named path.
		 */
		NamedPath definedBy(RelationshipPattern pattern);
	}

	/**
	 * Partial path that has a name ({@code p = }) and is based on a graph algorithm function.
	 */
	public interface OngoingShortestPathDefinitionWithName {

		/**
		 * Create a new named path based on a single relationship.
		 *
		 * @param relationship The relationship to be passed to {@code shortestPath}.
		 * @return A named path.
		 */
		NamedPath definedBy(Relationship relationship);
	}

	private static class Builder implements OngoingDefinitionWithName {

		private final SymbolicName name;

		private Builder(SymbolicName name) {
			this.name = name;
		}

		@Override
		public NamedPath definedBy(RelationshipPattern pattern) {
			return new NamedPath(name, pattern);
		}
	}

	private static class ShortestPathBuilder implements OngoingShortestPathDefinitionWithName {

		private final SymbolicName name;
		private final FunctionDefinition algorithm;

		private ShortestPathBuilder(SymbolicName name, FunctionDefinition algorithm) {
			this.name = name;
			this.algorithm = algorithm;
		}

		@Override
		public NamedPath definedBy(Relationship relationship) {
			return new NamedPath(name, FunctionInvocation.create(algorithm, relationship));
		}
	}

	private NamedPath(SymbolicName name, RelationshipPattern pattern) {
		this.name = name;
		this.pattern = pattern;
	}

	private NamedPath(SymbolicName name, FunctionInvocation algorithm) {
		this.name = name;
		this.pattern = algorithm;
	}

	@Override
	public Optional<SymbolicName> getSymbolicName() {
		return Optional.of(name);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.name.accept(visitor);
		Operator.ASSIGMENT.accept(visitor);
		this.pattern.accept(visitor);
		visitor.leave(this);
	}
}
