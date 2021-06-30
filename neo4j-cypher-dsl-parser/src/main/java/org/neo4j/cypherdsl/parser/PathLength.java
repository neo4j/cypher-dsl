/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.parser;

import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;

/**
 * Value object for the lenght of a path.
 *
 * @author Michael J. Simons
 * @soundtrack Pink Floyd - Atom Heart Mother
 * @since 2021.3.0
 */
@API(status = INTERNAL, since = "2021.3.0")
final class PathLength {

	static PathLength of(String minimum, String maximum) {
		Integer min = minimum == null || minimum.isBlank() ? null : Integer.valueOf(minimum.trim());
		Integer max = maximum == null || maximum.isBlank() ? null : Integer.valueOf(maximum.trim());
		return new PathLength(min, max);
	}

	private final Integer minimum;

	private final Integer maximum;

	private final boolean unbounded;

	private PathLength(Integer minimum, Integer maximum) {
		this.minimum = minimum;
		this.maximum = maximum;
		this.unbounded = minimum == null && maximum == null;
	}

	public Integer getMinimum() {
		return minimum;
	}

	public Integer getMaximum() {
		return maximum;
	}

	public boolean isUnbounded() {
		return unbounded;
	}
}
