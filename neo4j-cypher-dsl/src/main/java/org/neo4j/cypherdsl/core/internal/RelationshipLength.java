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
package org.neo4j.cypherdsl.core.internal;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Expresses the length of a relationship.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
public final class RelationshipLength implements Visitable {

	private final Integer minimum;

	private final Integer maximum;

	private final boolean unbounded;

	private RelationshipLength(Integer minimum, Integer maximum) {
		this.minimum = minimum;
		this.maximum = maximum;
		this.unbounded = minimum == null && maximum == null;
	}

	/**
	 * Creates an unbounded {@link RelationshipLength}.
	 * @return the new length definition
	 */
	public static RelationshipLength unbounded() {
		return new RelationshipLength(null, null);
	}

	/**
	 * Creates a {@link RelationshipLength} with given minimum and maximum values.
	 * @param minimum minimum length
	 * @param maximum maximum length
	 * @return the new length definition
	 */
	public static RelationshipLength of(Integer minimum, Integer maximum) {
		return new RelationshipLength(minimum, maximum);
	}

	/**
	 * {@return minimum number of hops to match}
	 */
	@API(status = INTERNAL)
	public Integer getMinimum() {
		return this.minimum;
	}

	/**
	 * {@return maximum number of hops to match}
	 */
	@API(status = INTERNAL)
	public Integer getMaximum() {
		return this.maximum;
	}

	/**
	 * {@return true if neither minimum nor maximum number of hops are set}
	 */
	@API(status = INTERNAL)
	public boolean isUnbounded() {
		return this.unbounded;
	}

}
