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
 * Helper class, only for internal use.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class KeyValueMapEntry implements Expression {

	private final String key;

	private final Expression value;

	private KeyValueMapEntry(String key, Expression value) {
		this.key = key;
		this.value = value;
	}

	/**
	 * Create a new {@link KeyValueMapEntry}. This is hardly useful in direct usage, but
	 * might be handy to compose clauses outside the fluent api.
	 * @param key the key of this entry
	 * @param value the value of this entry
	 * @return a new, immutable map entry.
	 * @since 2021.2.3
	 */
	@API(status = STABLE, since = "2021.2.3")
	public static KeyValueMapEntry create(String key, Expression value) {

		Assertions.notNull(key, "Key is required.");
		Assertions.notNull(value, "Value is required.");

		return new KeyValueMapEntry(key, value);
	}

	/**
	 * {@return the key of this entry}
	 */
	@API(status = INTERNAL)
	public String getKey() {
		return this.key;
	}

	/**
	 * {@return the value of this entry}
	 */
	@API(status = INTERNAL)
	public Expression getValue() {
		return this.value;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.value.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
