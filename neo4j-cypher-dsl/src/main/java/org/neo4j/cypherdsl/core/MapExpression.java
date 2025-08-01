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

import static org.apiguardian.api.API.Status.STABLE;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.utils.Assertions;

/**
 * A dedicated map expression.<p>
 * Most of the comparison methods on this expression will not result in a sensible query fragment.
 * A {@link MapExpression} is be useful as a concrete parameter to functions or as properties on {@link Node nodes}
 * or {@link Relationship relationships}.
 *
 * @author Michael J. Simons
 * @soundtrack Rammstein - RAMMSTEIN
 * @since 1.0
 */
@API(status = STABLE, since = "1.0")
public final class MapExpression extends TypedSubtree<Expression> implements Expression {

	static MapExpression create(Map<String, Object> map) {

		Object[] args = new Object[map.size() * 2];
		int i = 0;
		for (Map.Entry<String, Object> entry : map.entrySet()) {
			Object value = entry.getValue();
			args[i++] = entry.getKey();
			args[i++] = value instanceof Expression ? value : Cypher.literalOf(value);
		}
		return create(false, args);
	}

	static MapExpression create(boolean sort, Object... input) {

		Assertions.isTrue(input.length % 2 == 0, "Need an even number of input parameters");
		List<Expression> newContent = new ArrayList<>(input.length / 2);
		Set<String> knownKeys = new HashSet<>();

		for (int i = 0; i < input.length; i += 2) {
			Object keyCandidate = input[i];
			String key;
			if (keyCandidate instanceof String v) {
				key = v;
			} else if (keyCandidate instanceof Property property) {
				List<PropertyLookup> names = property.getNames();
				if (names.size() != 1) {
					throw new IllegalArgumentException("Nested properties are not supported in a map expression");
				}
				key = names.get(0).getPropertyKeyName().getValue();
			} else {
				throw new IllegalStateException("Key needs to be of type String or Property.");
			}
			Assertions.isInstanceOf(Expression.class, input[i + 1], "Value needs to be of type Expression.");
			Assertions.isTrue(!knownKeys.contains(input[i]), "Duplicate key '" + input[i] + "'");

			final KeyValueMapEntry entry = KeyValueMapEntry.create(key, (Expression) input[i + 1]);
			newContent.add(entry);
			knownKeys.add(entry.getKey());
		}

		if (sort) {
			newContent.sort(Comparator.comparing(o -> ((KeyValueMapEntry) o).getKey()));
		}
		return new MapExpression(newContent);
	}

	static MapExpression withEntries(List<Expression> entries) {
		return new MapExpression(entries);
	}

	private MapExpression(List<Expression> children) {
		super(children);
	}

	MapExpression addEntries(List<Expression> entries) {
		List<Expression> newContent = new ArrayList<>(super.children.size() + entries.size());
		newContent.addAll(super.children);
		newContent.addAll(entries);
		return new MapExpression(newContent);
	}

	@Override
	protected Visitable prepareVisit(Expression child) {
		return Expressions.nameOrExpression(child);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}
}
