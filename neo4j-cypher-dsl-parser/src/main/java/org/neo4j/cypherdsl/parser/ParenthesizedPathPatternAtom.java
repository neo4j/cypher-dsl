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
package org.neo4j.cypherdsl.parser;

import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.PatternElement;
import org.neo4j.cypherdsl.core.QuantifiedPathPattern;
import org.neo4j.cypherdsl.core.RelationshipPattern;

/**
 * Helper to deal with quantified path patterns.
 *
 * @param patternElement the pattern element in parentheses
 * @param quantifier the quantifier
 * @param predicate any predicate
 * @author Michael J. Simons
 */
record ParenthesizedPathPatternAtom(RelationshipPattern patternElement, QuantifiedPathPattern.Quantifier quantifier,
		Expression predicate) implements PatternAtom {

	PatternElement asPatternElement() {

		return this.patternElement.quantify(this.quantifier).where(this.predicate);
	}
}
