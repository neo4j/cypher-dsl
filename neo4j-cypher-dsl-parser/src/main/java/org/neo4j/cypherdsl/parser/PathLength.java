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
package org.neo4j.cypherdsl.parser;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * Value object for the lenght of a path.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = INTERNAL, since = "2021.3.0")
final class PathLength {

	private final Integer minimum;

	private final Integer maximum;

	private final boolean unbounded;

	private PathLength(Integer minimum, Integer maximum) {
		this.minimum = minimum;
		this.maximum = maximum;
		this.unbounded = minimum == null && maximum == null;
	}

	static PathLength of(String minimum, String maximum) {
		Integer min = ((minimum == null) || minimum.isBlank()) ? null : Integer.valueOf(minimum.trim());
		Integer max = ((maximum == null) || maximum.isBlank()) ? null : Integer.valueOf(maximum.trim());
		return new PathLength(min, max);
	}

	Integer getMinimum() {
		return this.minimum;
	}

	Integer getMaximum() {
		return this.maximum;
	}

	boolean isUnbounded() {
		return this.unbounded;
	}

}
