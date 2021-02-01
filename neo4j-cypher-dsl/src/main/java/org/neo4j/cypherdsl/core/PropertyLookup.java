/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * See <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M14/railroad/PropertyLookup.html">PropertyLookup</a>
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = EXPERIMENTAL, since = "1.0")
public final class PropertyLookup implements Expression {

	private static final PropertyLookup WILDCARD = new PropertyLookup(Asterisk.INSTANCE);

	private final Expression propertyKeyName;

	static PropertyLookup forName(String name) {

		Assertions.hasText(name, "The property's name is required.");
		return new PropertyLookup(SymbolicName.unsafe(name));
	}

	static PropertyLookup wildcard() {

		return WILDCARD;
	}

	private PropertyLookup(Expression propertyKeyName) {
		this.propertyKeyName = propertyKeyName;
	}

	@API(status = INTERNAL)
	SymbolicName getPropertyKeyName() {

		Assertions.isTrue(this != WILDCARD, "The wildcard property lookup does not reference a specific property!");
		return (SymbolicName) propertyKeyName;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		propertyKeyName.accept(visitor);
		visitor.leave(this);
	}
}
