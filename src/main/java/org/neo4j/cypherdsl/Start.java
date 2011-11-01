/**
 * Copyright (c) 2002-2011 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
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
package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.query.StartExpression;

/**
 * Implements the START clause. Use the static methods in StartExpression to create expressions,
 * or use the convenience methods here for common cases.
 */
public interface Start
{
    StartNext start(StartExpression startExpression);

    StartNext nodes( String name, int... id );

    StartNext nodes( String name, String... parameters );

    StartNext nodesLookup( String name, String indexName, String key, String value );

    StartNext nodesQuery( String name, String indexName, String query );

    StartNext relationships( String name, int... id );

    StartNext relationships( String name, String... parameters );

    StartNext relationshipsLookup( String name, String indexName, String key, String value );
}
