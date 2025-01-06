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

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

/**
 * @author Michael J. Simons
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class RelationshipChainTest {

	private final Node s = Cypher.node("Start").named("s");
	private final Node e = Cypher.node("End").named("e");
	private final RelationshipChain chain1 = s.relationshipTo(Cypher.anyNode()).relationshipTo(e);

	@Test
	void namedShouldReturnNew() {

		RelationshipChain chain2 = chain1.named("x");
		assertThat(chain2).isNotSameAs(chain1);

		String cypher1 = Cypher.match(chain1).returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher1).isEqualTo("MATCH (s:`Start`)-->()-->(e:`End`) RETURN *");
		String cypher2 = Cypher.match(chain2).returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher2).isEqualTo("MATCH (s:`Start`)-->()-[x]->(e:`End`) RETURN *");
	}

	@Test
	void unboundedShouldReturnNew() {

		RelationshipChain chain2 = chain1.unbounded();
		assertThat(chain2).isNotSameAs(chain1);

		String cypher1 = Cypher.match(chain1).returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher1).isEqualTo("MATCH (s:`Start`)-->()-->(e:`End`) RETURN *");
		String cypher2 = Cypher.match(chain2).returning(Cypher.asterisk()).build().getCypher();
		assertThat(cypher2).isEqualTo("MATCH (s:`Start`)-->()-[*]->(e:`End`) RETURN *");
	}
}
