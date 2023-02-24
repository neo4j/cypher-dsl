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
package org.neo4j.cypherdsl.examples.parser;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Set;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.StatementCatalog;
import org.neo4j.cypherdsl.parser.CypherParser;

/**
 * @author Michael J. Simons
 */
class StatementCatalogBuildingVisitorViaParserTest {

	@Test
	void simpleShowCase() {

		// tag::catalog-example[]
		var input = """
			MATCH (m:`Movie` {title: 'The Matrix'})<-[a:`ACTED_IN`]-(p:`Person`)
			WHERE p.born >= $born
			RETURN p
			""";
		var statement = CypherParser.parse(input);

		var catalog = statement.getCatalog();

		assertThat(catalog.getNodeLabels())
			.extracting(StatementCatalog.Token::value)
			.containsExactlyInAnyOrder("Person", "Movie");
		assertThat(catalog.getProperties())
			.containsExactlyInAnyOrder(
				StatementCatalog.property(Set.of(StatementCatalog.label("Movie")), "title"),
				StatementCatalog.property(Set.of(StatementCatalog.label("Person")), "born")
			);
		// end::catalog-example[]

		var cypher = statement.getCypher();
		assertThat(cypher).isEqualTo("MATCH (m:`Movie` {title: 'The Matrix'})<-[a:`ACTED_IN`]-(p:`Person`) WHERE p.born >= $born RETURN p");
	}
}
