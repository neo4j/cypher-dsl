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

import static org.apiguardian.api.API.Status.*;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.TypedSubtree;

/**
 * Items yielded by a stand alone or in query call.
 *
 * @param <T>    The children's type
 * @param <SELF> The concrete type of this class.
 * @author Michael J. Simons
 * @soundtrack Brian May &amp; Kerry Ellis - Golden Days
 * @since 2020.1.0.0
 */
@API(status = INTERNAL, since = "2020.1.0")
public final class YieldItems<T extends Expression, SELF extends YieldItems<T, SELF>> extends TypedSubtree<T, SELF> {

	static <C extends Expression, SELF extends YieldItems<C, SELF>> YieldItems<C, SELF> yieldAllOf(C... c) {

		if (c == null || c.length == 0) {
			throw new IllegalArgumentException("Cannot yield an empty list of items.");
		}

		return new YieldItems<C, SELF>(c);
	}

	private YieldItems(T... children) {
		super(children);
	}
}
