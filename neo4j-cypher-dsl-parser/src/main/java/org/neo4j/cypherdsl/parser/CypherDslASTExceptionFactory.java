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

import static org.apiguardian.api.API.Status.INTERNAL;

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypher.internal.ast.factory.ASTExceptionFactory;

/**
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = INTERNAL, since = "2021.3.0")
enum CypherDslASTExceptionFactory implements ASTExceptionFactory {
	INSTANCE;

	@Override
	public Exception syntaxException(String got, List<String> expected, Exception source, int offset, int line, int column) {
		return new RuntimeException(source.getMessage());
	}

	@Override
	public Exception syntaxException(Exception source, int offset, int line, int column) {
		return new RuntimeException(source.getMessage());
	}
}
