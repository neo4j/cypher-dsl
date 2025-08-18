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
package org.neo4j.cypherdsl.core.internal;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Items yielded by a stand alone or in query call.
 *
 * @author Michael J. Simons
 * @since 2020.0.1
 */
@API(status = INTERNAL, since = "2020.0.1")
public final class YieldItems extends TypedSubtree<Expression> {

	private YieldItems(Expression... children) {
		super(children);
	}

	/**
	 * Creates a new {@literal YIELD} expressions.
	 * @param c the elements to yield
	 * @return the new expression
	 */
	public static YieldItems yieldAllOf(Expression... c) {

		if (c == null || c.length == 0) {
			throw new IllegalArgumentException("Cannot yield an empty list of items.");
		}

		return new YieldItems(c);
	}

}
