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
package org.neo4j.cypherdsl.query.neo4j;

import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.CypherQuery.literals;
import static org.neo4j.cypherdsl.query.Query.checkNull;

import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.expression.StartExpression;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.graphdb.Node;

/**
 * START expressions that use Neo4j Node objects directly, thus avoiding use of
 * long identifiers.
 */
public abstract class StartExpressionNeo
{
    public static StartExpression.StartNodes nodeById( String name, Node... nodes )
    {
        return nodeById( identifier( name ), nodes );
    }

    public static StartExpression.StartNodes nodeById( Identifier name, Node... nodes )
    {
        checkNull( name, "Name" );

        for ( Node node : nodes )
        {
            Query.checkNull( node, "Node" );
        }

        long[] ids = new long[nodes.length];
        for ( int i = 0; i < nodes.length; i++ )
        {
            Node node = nodes[i];
            ids[i] = node.getId();
        }

        return new StartExpression.StartNodes( name, literals( ids ) );
    }

}
