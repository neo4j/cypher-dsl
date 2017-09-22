/**
 * Licensed to Neo Technology under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Neo Technology licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
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
