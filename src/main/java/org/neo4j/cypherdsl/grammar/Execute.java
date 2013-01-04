/**
 * Copyright (c) 2002-2013 "Neo Technology,"
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
package org.neo4j.cypherdsl.grammar;

import java.util.Map;

import org.neo4j.cypherdsl.AsString;
import org.neo4j.cypherdsl.query.Query;

/**
 * Once the query has been constructed, the methods here can be used to either stringify it or extract the
 * Query model for further processing. Can also be used to specify parameters to be used for execution.
 * <p/>
 * Note that setting parameters will create a new set of parameters for each execution, so that you can reuse
 * this Execute instance for many executions.
 */
public interface Execute
        extends AsString
{
    Query toQuery();

    /**
     * Create a ExecuteWithParameters that has the given parameter set. The result
     * can be used to further specify parameters before execution.
     *
     * @param name
     * @param value
     * @return
     */
    ExecuteWithParameters parameter( String name, Object value );

    /**
     * Create a ExecuteWithParameters that has the given parameters set. The result
     * can be used to further specify parameters before execution. Note that the given
     * map overwrites any existing parameters that have already been set, if they have the same names.
     *
     * @param name
     * @param value
     * @return
     */
    ExecuteWithParameters parameters( Map<String, Object> parameters );
}
