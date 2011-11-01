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

import java.io.IOException;
import java.util.Map;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.neo4j.cypher.commands.Query;
import org.neo4j.cypher.javacompat.CypherParser;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.test.GraphDescription;
import org.neo4j.test.GraphHolder;
import org.neo4j.test.ImpermanentGraphDatabase;
import org.neo4j.test.TestData;

import static org.neo4j.cypherdsl.query.MatchExpression.Direction.*;
import static org.neo4j.cypherdsl.query.MatchExpression.*;

/**
 * Set up a query using the CypherQuery builder, and then use it to execute a query to a test database.
 */
public class CypherExecutionTest
    implements GraphHolder
{
    public @Rule
    TestData<Map<String, Node>> data = TestData.producedThrough( GraphDescription.createGraphFor(
        this, true ) );

    private ImpermanentGraphDatabase graphdb;
    private ExecutionEngine engine;
    private CypherParser parser;

    @Test
    @GraphDescription.Graph( value = {
        "John friend Sara", "John friend Joe",
        "Sara friend Maria", "Joe friend Steve"
    }, autoIndexNodes = true )
    public void testCypherExecution()
        throws Exception
    {
        data.get();

        String query = CypherQuery.newQuery()
            .nodesLookup( "john", "node_auto_index", "name", "John" )
            .match( path( "john", OUTGOING, "" ).relationship( "friend" ).path( OUTGOING, null, "friend", "fof" ) )
            .returnProperty( "john.name", "fof.name" )
            .toString();

        Query parsedQuery = parser.parse( query );
        System.out.println( engine.execute( parsedQuery ).toString() );
    }

    @Before
    public void setup()
        throws IOException
    {
        graphdb = new ImpermanentGraphDatabase();
        graphdb.cleanContent( false );

        parser = new CypherParser();
        engine = new ExecutionEngine( graphdb );
    }

    @Override
    public GraphDatabaseService graphdb()
    {
        return graphdb;
    }
}
