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

import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Relationship.Direction;
import org.neo4j.cypherdsl.core.SymbolicName;

/**
 * A value object for the details of a path.
 *
 * @author Michael J. Simons
 * @soundtrack Pink Floyd - The Division Bell
 * @since 2021.3.0
 */
final class PathAtom implements PatternAtom {

	static PathAtom of(SymbolicName name, PathLength length, boolean left, boolean right,
		String[] relTypes, MapExpression properties) {

		if (left && right) {
			throw new IllegalArgumentException("Only left-to-rhs, rhs-to-left or unidirectional path elements are supported.");
		}

		Direction direction;
		if (left) {
			direction = Direction.RTL;
		} else if (right) {
			direction = Direction.LTR;
		} else {
			direction = Direction.UNI;
		}

		return new PathAtom(name, length, direction, relTypes, properties);
	}

	private final SymbolicName name;

	private final PathLength length;

	private final Direction direction;

	private final String[] types;

	private final MapExpression properties;

	private PathAtom(SymbolicName name, PathLength length, Direction direction, String[] types,
		MapExpression properties) {
		this.name = name;
		this.length = length;
		this.direction = direction;
		this.types = types;
		this.properties = properties;
	}

	public PathLength getLength() {
		return length;
	}

	public Direction getDirection() {
		return direction;
	}

	public String[] getTypes() {
		return types;
	}

	public MapExpression getProperties() {
		return this.properties;
	}

	public SymbolicName getName() {
		return name;
	}
}
