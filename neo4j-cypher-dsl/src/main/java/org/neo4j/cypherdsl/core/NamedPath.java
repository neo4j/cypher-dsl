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

import java.util.Optional;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.FunctionInvocation.FunctionDefinition;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * Represents a named path. A named path can be either a {@link RelationshipPattern} that
 * has been assigned to a variable as in {@code p := (a)-->(b)}, a call to functions known
 * to return paths or an existing, symbolic name that might come from an arbitrary
 * procedure returning path elements. <br>
 * <b>Note</b>: We cannot check a value that has been yielded from a procedure upfront to
 * verify that it is a named path. This is up to the caller.
 *
 * @author Michael J. Simons
 * @since 1.1
 */
@API(status = STABLE, since = "1.1")
public final class NamedPath implements PatternElement, Named {

	/**
	 * The name of this path expression.
	 */
	private final SymbolicName name;

	/**
	 * An optional {@code SHORTEST} keyword element.
	 */
	private final PatternSelector optionalPatternSelector;

	/**
	 * The pattern defining this path.
	 */
	private final Visitable optionalPattern;

	private NamedPath(SymbolicName name) {
		this.name = name;
		this.optionalPatternSelector = null;
		this.optionalPattern = null;
	}

	private NamedPath(SymbolicName name, PatternSelector optionalPatternSelector, PatternElement optionalPattern) {
		this.name = name;
		this.optionalPatternSelector = optionalPatternSelector;
		this.optionalPattern = optionalPattern;
	}

	private NamedPath(SymbolicName name, FunctionInvocation algorithm) {
		this.name = name;
		this.optionalPatternSelector = null;
		this.optionalPattern = algorithm;
	}

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

	static OngoingShortestDefinition shortest(int k) {
		return new ShortestBuilder(PatternSelector.shortestK(k));
	}

	static OngoingShortestDefinition allShortest() {
		return new ShortestBuilder(PatternSelector.allShortest());
	}

	static OngoingShortestDefinition shortestKGroups(int k) {
		return new ShortestBuilder(PatternSelector.shortestKGroups(k));
	}

	static OngoingShortestDefinition any() {
		return new ShortestBuilder(PatternSelector.any());
	}

	@API(status = INTERNAL, since = "2024.7.0")
	public static NamedPath select(PatternSelector patternSelector, PatternElement patternElement) {
		if (patternElement instanceof NamedPath namedPath
				&& namedPath.optionalPattern instanceof PatternElement target) {
			return new NamedPath(namedPath.name, patternSelector, target);
		}
		return new NamedPath(SymbolicName.unresolved(), patternSelector, patternElement);
	}

	@Override
	public Optional<SymbolicName> getSymbolicName() {
		return Optional.of(this.name);
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.name.accept(visitor);
		if (this.optionalPattern != null) {
			Operator.ASSIGMENT.accept(visitor);
			if (this.optionalPatternSelector != null) {
				visitor.enter(this.optionalPatternSelector);
			}
			this.optionalPattern.accept(visitor);
			if (this.optionalPatternSelector != null) {
				visitor.leave(this.optionalPatternSelector);
			}
		}
		visitor.leave(this);
	}

	/**
	 * Partial path that has a name, introduced as a superinterface for
	 * {@link OngoingDefinitionWithName} to avoid dragging
	 * {@link OngoingDefinitionWithName#get()}.
	 */
	public interface OngoingNamedDefinition {

		/**
		 * Create a new named path based on a {@link PatternElement}.
		 * @param pattern the pattern to be matched for the named path.
		 * @return a named path.
		 */
		NamedPath definedBy(PatternElement pattern);

	}

	/**
	 * Partial path that has a name ({@code p = }).
	 */
	public interface OngoingDefinitionWithName extends OngoingNamedDefinition {

		/**
		 * Create a new named path that references a given, symbolic name. No checks are
		 * done if the referenced name actually points to a path.
		 * @return a named path.
		 * @since 2020.1.4
		 */
		NamedPath get();

	}

	/**
	 * Partial path that has a name ({@code p = }) and is based on a graph algorithm
	 * function.
	 */
	public interface OngoingShortestPathDefinitionWithName {

		/**
		 * Create a new named path based on a single relationship.
		 * @param relationship the relationship to be passed to {@code shortestPath}.
		 * @return a named path.
		 */
		NamedPath definedBy(Relationship relationship);

	}

	/**
	 * Partial path with the number of paths to match.
	 */
	public interface OngoingShortestDefinition {

		default OngoingNamedDefinition named(String name) {
			return named(Cypher.name(name));
		}

		OngoingNamedDefinition named(SymbolicName name);

	}

	private record Builder(SymbolicName name) implements OngoingDefinitionWithName {

		@Override
		public NamedPath definedBy(PatternElement pattern) {
			if (pattern instanceof NamedPath namedPath) {
				return namedPath;
			}
			return new NamedPath(this.name, null, pattern);
		}

		@Override
		public NamedPath get() {
			return new NamedPath(this.name);
		}
	}

	private record ShortestPathBuilder(SymbolicName name,
			FunctionDefinition algorithm) implements OngoingShortestPathDefinitionWithName {

		@Override
		public NamedPath definedBy(Relationship relationship) {
			return new NamedPath(this.name, FunctionInvocation.create(this.algorithm, relationship));
		}
	}

	private static final class ShortestBuilder implements OngoingShortestDefinition, OngoingNamedDefinition {

		private final PatternSelector shortest;

		private SymbolicName name;

		private ShortestBuilder(PatternSelector shortest) {
			this.shortest = shortest;
		}

		@Override
		public OngoingNamedDefinition named(SymbolicName newName) {
			this.name = newName;
			return this;
		}

		@Override
		public NamedPath definedBy(PatternElement pattern) {
			return new NamedPath(this.name, this.shortest, pattern);
		}

	}

}
