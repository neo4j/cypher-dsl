/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import static org.apiguardian.api.API.Status.INTERNAL;
import static org.apiguardian.api.API.Status.STABLE;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.ast.Visitor;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * Represents a map projection as described <a href="https://medium.com/neo4j/loading-graph-data-for-an-object-graph-mapper-or-graphql-5103b1a8b66e">here</a>.
 *
 * @author Michael J. Simons
 */
@API(status = STABLE, since = "1.0")
public final class MapProjection implements Expression {

	private final SymbolicName name;

	private final MapExpression map;

	/**
	 * Create a new map projection with the given, mixed content
	 * @param name The symbolic name of this project
	 * @param content The projected content
	 * @return A new map projection
	 * @since 2021.2.3
	 */
	@API(status = INTERNAL, since = "2023.9.0")
	public static MapProjection create(SymbolicName name, Object... content) {

		return new MapProjection(name, MapExpression.withEntries(createNewContent(false, content)));
	}

	/**
	 * Create a new map projection with the given, mixed content
	 * @param name The symbolic name of this project
	 * @param content The projected content
	 * @return A new map projection
	 * @since 2024.1.1
	 */
	@API(status = INTERNAL, since = "2024.1.1")
	public static MapProjection sorted(SymbolicName name, Object... content) {

		return new MapProjection(name, MapExpression.withEntries(createNewContent(true, content)));
	}

	MapProjection(SymbolicName name, MapExpression map) {
		this.name = name;
		this.map = map;
	}

	/**
	 * Adds additional content. The current projection is left unchanged and a new one is returned.
	 *
	 * @param content The additional content for a new projection.
	 * @return A new map projection with additional content.
	 */
	@NotNull @Contract(pure = true)
	public MapProjection and(Object... content) {
		return new MapProjection(this.name, this.map.addEntries(createNewContent(false, content)));
	}

	@Override
	public void accept(Visitor visitor) {
		visitor.enter(this);
		this.name.accept(visitor);
		this.map.accept(visitor);
		visitor.leave(this);
	}

	@SuppressWarnings("deprecation")
	private static Object contentAt(Object[] content, int i) {

		Object currentObject = content[i];
		if (currentObject instanceof Expression expression) {
			return Expressions.nameOrExpression(expression);
		} else if (currentObject instanceof Named named) {
			return named.getSymbolicName().map(Object.class::cast).orElse(currentObject);
		}
		return currentObject;
	}

	private static List<Expression> createNewContent(boolean sort, Object... content) {
		final List<Expression> newContent = new ArrayList<>(content.length);
		final Set<String> knownKeys = new HashSet<>();

		String lastKey = null;
		Expression lastExpression = null;
		int i = 0;
		while (i < content.length) {

			Object next;
			if (i + 1 >= content.length) {
				next = null;
			} else {
				next = contentAt(content, i + 1);
			}
			Object current = contentAt(content, i);

			if (current instanceof String stringValue) {
				if (next instanceof Expression expression) {
					lastKey = stringValue;
					lastExpression = expression;
					i += 2;
				} else {
					lastExpression = PropertyLookup.forName((String) current);
					i += 1;
				}
			} else if (current instanceof Expression expression) {
				lastExpression = expression;
				i += 1;
			}
			if (lastExpression instanceof Asterisk) {
				lastExpression = PropertyLookup.wildcard();
			}

			if (lastKey != null) {
				Assertions.isTrue(!knownKeys.contains(lastKey), "Duplicate key '" + lastKey + "'");
				newContent.add(KeyValueMapEntry.create(lastKey, lastExpression));
				knownKeys.add(lastKey);
			} else if (lastExpression instanceof SymbolicName || lastExpression instanceof PropertyLookup) {
				newContent.add(lastExpression);
			} else if (lastExpression instanceof Property property) {
				List<PropertyLookup> names = property.getNames();
				if (names.size() > 1) {
					throw new IllegalArgumentException("Cannot project nested properties!");
				}
				newContent.addAll(names);
			} else if (lastExpression instanceof AliasedExpression aliasedExpression) {
				newContent.add(KeyValueMapEntry.create(aliasedExpression.getAlias(), aliasedExpression));
			} else if (lastExpression instanceof KeyValueMapEntry) {
				newContent.add(lastExpression);
			} else if (lastExpression == null) {
				throw new IllegalArgumentException("Could not determine an expression from the given content!");
			} else {
				throw new IllegalArgumentException(lastExpression + " of type " + lastExpression.getClass()
					+ " cannot be used with an implicit name as map entry.");
			}

			lastKey = null;
			lastExpression = null;
		}

		if (sort) {
			newContent.sort((o1, o2) -> {
				if (o1 instanceof KeyValueMapEntry kvm1 && o2 instanceof KeyValueMapEntry kvm2) {
					return kvm1.getKey().compareTo(kvm2.getKey());
				} else if (o1 instanceof PropertyLookup pl1 && o2 instanceof PropertyLookup pl2) {
					if (pl1 == PropertyLookup.wildcard()) {
						return -1;
					} else if (pl2 == PropertyLookup.wildcard()) {
						return 1;
					}
					return pl1.getPropertyKeyName().getValue().compareTo(pl2.getPropertyKeyName().getValue());
				} else if (o1 instanceof PropertyLookup) {
					return 1;
				}
				return -1;
			});
		}

		return newContent;
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}
}
