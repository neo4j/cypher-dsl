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
package org.neo4j.cypherdsl.examples.sdn6;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.examples.sdn6.misc.Example_;

/**
 * @author Michael J. Simons
 * @soundtrack In Flames - I, The Mask
 */
class UsageTest {

	@Test // GH-335
	void defaultUsageOfSelfReferentialNode() {
		Example_ node = Example_.EXAMPLE;
		assertThat(node).isNotNull();
	}

	@Test // GH-335
	void selfReferentialNodesShouldLeadToUsableCode() {
		Example_ node = Example_.EXAMPLE.named("example");
		assertThat(node).isNotNull();
	}

	@Test // GH-335
	void ltolx() {

		var left = Example_.EXAMPLE.named("n");
		var parent = left.withParent(Example_.EXAMPLE.named("m"));
		assertThat(parent.getLeft().getRequiredSymbolicName().getValue()).isEqualTo("n");
	}

	@Test // GH-335
	void rtorx() {

		var parent = Example_.EXAMPLE.withParent(Example_.EXAMPLE.named("m"));
		assertThat(parent.getRight().getRequiredSymbolicName().getValue()).isEqualTo("m");
	}

	@Test // GH-335
	void renamingLRShouldWork() {

		var left = Example_.EXAMPLE.named("n");
		var right = Example_.EXAMPLE.named("m");
		var rel = left.withParent(right).named("r");
		var cypher = Cypher.match(rel)
			.where(right.ID.isEqualTo(Cypher.literalOf(1L)))
			.returning(left, right)
			.build().getCypher();
		assertThat(cypher).isEqualTo("MATCH (n:`Example`)-[r:`BELONGS_TO`]->(m:`Example`) WHERE m.id = 1 RETURN n, m");
	}

	@Test // GH-335
	void renamingShouldWork() {

		var node = Example_.EXAMPLE.named("n");
		var rel = node.withParent(Example_.EXAMPLE).named("r");
		var cypher = Cypher.match(rel)
			.where(node.ID.isEqualTo(Cypher.literalOf(1L)))
			.returning(node)
			.build().getCypher();
		assertThat(cypher).matches(
			"MATCH \\(n:`Example`\\)-\\[r:`BELONGS_TO`]->\\(.+:`Example`\\) WHERE n\\.id = 1 RETURN n");
	}
}
