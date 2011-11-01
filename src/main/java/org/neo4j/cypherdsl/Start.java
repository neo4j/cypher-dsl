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
