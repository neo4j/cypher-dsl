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

import java.util.List;

import org.jetbrains.annotations.Nullable;
import org.neo4j.cypherdsl.core.Hint;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Where;

/**
 * Shape of a match clause.
 *
 * @author Michael J. Simons
 * @param optional        flag if this is an optional match
 * @param patternElements the pattern elements inside the match
 * @param optionalWhere   an optional where clause
 * @param optionalHints   optional hints
 * @since 2023.0.2
 */
public record MatchDefinition(
	boolean optional, List<PatternElement> patternElements,
	@Nullable Where optionalWhere,
	@Nullable List<Hint> optionalHints) {
}
