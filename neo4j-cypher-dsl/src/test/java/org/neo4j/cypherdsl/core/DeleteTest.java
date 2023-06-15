/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * @author Michael J. Simons
 */
class DeleteTest {

	@Test
	void buildingDeleteClauseWithMultipleElements() {

		Delete delete = Delete.delete(Cypher.name("n"), Cypher.name("b"));
		StringBuilder sb = new StringBuilder();
		delete.accept(new Visitor() {

			boolean first = true;

			@Override
			public void enter(Visitable segment) {
				if (segment instanceof Delete) {
					sb.append("Delete{cypher=DELETE");
				} else if (segment instanceof SymbolicName) {
					SymbolicName n = (SymbolicName) segment;
					sb.append(" ").append(n.getValue());
					if (first) {
						sb.append(",");
						first = false;
					}
				}
			}

			@Override
			public void leave(Visitable segment) {
				if (segment instanceof Delete) {
					sb.append("}");
				}
			}
		});
		assertThat(sb).hasToString("Delete{cypher=DELETE n, b}");
	}
}
