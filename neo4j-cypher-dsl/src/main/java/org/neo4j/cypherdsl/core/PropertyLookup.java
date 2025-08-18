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

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

/**
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M14/railroad/PropertyLookup.html">PropertyLookup</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class PropertyLookup implements Expression {

	private static final PropertyLookup WILDCARD = new PropertyLookup(Asterisk.INSTANCE, false);

	private final Expression propertyKeyName;

	/** This flag is set to true for dynamic lookups via `p['x']` notation. */
	private final boolean dynamicLookup;

	private PropertyLookup(Expression propertyKeyName, boolean dynamicLookup) {

		this.propertyKeyName = propertyKeyName;
		this.dynamicLookup = dynamicLookup;
	}

	/**
	 * This creates a property lookup for a given name. It is mostly usable when building
	 * an AST outside the fluent API. If you need to create property lookup for a
	 * {@link SymbolicName symbolic name}, most likely you can just use the symbolic name.
	 * @param name the name to lookup
	 * @return a property lookup
	 * @since 2021.3.0
	 */
	public static PropertyLookup forName(String name) {

		Assertions.hasText(name, "The property's name is required.");
		return new PropertyLookup(SymbolicName.unsafe(name), false);
	}

	static PropertyLookup forExpression(Expression expression) {

		Assertions.notNull(expression, "The expression is required");
		return new PropertyLookup(expression, true);
	}

	static PropertyLookup wildcard() {

		return WILDCARD;
	}

	@API(status = INTERNAL)
	SymbolicName getPropertyKeyName() {

		Assertions.isTrue(this != WILDCARD, "The wildcard property lookup does not reference a specific property!");
		return (SymbolicName) this.propertyKeyName;
	}

	/**
	 * {@return <code>true</code> if this is a dynamic property}
	 */
	@API(status = INTERNAL)
	public boolean isDynamicLookup() {
		return this.dynamicLookup;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.propertyKeyName.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
