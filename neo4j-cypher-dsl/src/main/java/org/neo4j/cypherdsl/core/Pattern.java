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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.TypedSubtree;

import static org.apiguardian.api.API.Status.INTERNAL;

/**
 * A pattern is something that can be matched. It consists of one or more pattern
 * elements. Those can be nodes or chains of nodes and relationships.
 * <p>
 * See <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M15/railroad/Pattern.html">Pattern</a>.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
@API(status = INTERNAL, since = "1.0")
final class Pattern extends TypedSubtree<PatternElement> {

	private Pattern(List<PatternElement> patternElements) {
		super(patternElements);
	}

	static Pattern of(PatternElement requiredPattern, PatternElement... patternElement) {
		List<PatternElement> elements;
		if (patternElement == null || patternElement.length == 0) {
			elements = List.of(requiredPattern);
		}
		else {
			elements = new ArrayList<>();
			elements.add(requiredPattern);
			elements.addAll(Arrays.asList(patternElement));
		}
		return Pattern.of(elements);
	}

	static Pattern of(Collection<? extends PatternElement> elements) {
		return new Pattern(elements.stream().map(PatternElement.class::cast).toList());
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

}
