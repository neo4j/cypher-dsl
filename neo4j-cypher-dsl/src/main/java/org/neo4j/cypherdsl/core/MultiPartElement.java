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
package org.neo4j.cypherdsl.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * Connects several visitables into one element.
 *
 * @author Michael J. Simons
 * @since 1.0
 */
class MultiPartElement implements Visitable {

	private final List<Visitable> precedingClauses;

	private final With with;

	MultiPartElement(List<Visitable> precedingClauses, With with) {

		if (precedingClauses == null || precedingClauses.isEmpty()) {
			this.precedingClauses = Collections.emptyList();
		}
		else {
			this.precedingClauses = new ArrayList<>(precedingClauses);
		}

		this.with = with;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.precedingClauses.forEach(c -> c.accept(visitor));
		this.with.accept(visitor);
		visitor.leave(this);
	}

	@Override
	public String toString() {
		return RendererBridge.render(this);
	}

	boolean isImporting() {
		return this.precedingClauses.isEmpty() && !this.with.getItems().isEmpty()
				&& this.with.getItems().stream().allMatch(IdentifiableElement.class::isInstance);
	}

	IdentifiableElement[] getImports() {
		return this.with.getItems().stream().map(IdentifiableElement.class::cast).toArray(IdentifiableElement[]::new);
	}

}
