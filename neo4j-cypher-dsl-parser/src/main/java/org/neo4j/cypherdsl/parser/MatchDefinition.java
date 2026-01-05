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

import java.util.List;

import org.neo4j.cypherdsl.core.Hint;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.Where;

/**
 * Shape of a match clause.
 *
 * @author Michael J. Simons
 * @param optional flag if this is an optional match
 * @param patternElements the pattern elements inside the match
 * @param optionalWhere an optional where clause
 * @param optionalHints optional hints
 * @since 2023.0.2
 */
public record MatchDefinition(boolean optional, List<PatternElement> patternElements, Where optionalWhere,
		List<Hint> optionalHints) {
}
