/*
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

import org.neo4j.cypherdsl.query.ReturnExpression;

/**
 * TODO
 */
public interface Return
    extends Execute
{
    ReturnNext returnExpr(ReturnExpression returnExpression);

    ReturnNext returnNodes(String... name);
    ReturnNext returnNodes(boolean distinct, String... name);
    ReturnNext returnRelationships(String... name);
    ReturnNext returnPaths(String... name);
    ReturnNext returnProperties(String... names);
    ReturnNext returnProperties(boolean optional, String... names);

    // Aggregate functions
    ReturnNext count();
    ReturnNext count(String name);
    ReturnNext sum(String name);
    ReturnNext avg(String name);
    ReturnNext max(String name);
    ReturnNext min(String name);
    ReturnNext collect(String name);
}
