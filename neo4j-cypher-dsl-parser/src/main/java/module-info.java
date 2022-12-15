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
/**
 * @author Michael J. Simons
 * @since 2023.0.0
 */
@SuppressWarnings( {"requires-automatic"}) // Neo4j Cypher Parser
module org.neo4j.cypherdsl.parser {

	requires static org.jetbrains.annotations;

	requires transitive org.apiguardian.api;
	requires transitive org.neo4j.cypherdsl.core;

	requires org.neo4j.cypher.internal.parser.javacc;
	requires org.neo4j.cypher.internal.ast.factory;

	exports org.neo4j.cypherdsl.parser;
}
