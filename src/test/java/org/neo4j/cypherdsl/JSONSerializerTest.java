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

import static org.neo4j.cypherdsl.CypherQuery.as;
import static org.neo4j.cypherdsl.CypherQuery.count;
import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.CypherQuery.lookup;
import static org.neo4j.cypherdsl.CypherQuery.node;
import static org.neo4j.cypherdsl.CypherQuery.path;
import static org.neo4j.cypherdsl.CypherQuery.shortestPath;
import static org.neo4j.cypherdsl.CypherQuery.start;

import java.io.IOException;
import java.util.Map;

import org.junit.*;
import org.junit.Rule;
import org.junit.Test;
import org.neo4j.cypherdsl.result.JSONSerializer;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.neo4j.test.*;

/**
 * Test of JSON serialization of results.
 */
public class JSONSerializerTest
        implements GraphHolder
{
    public
    @Rule
    TestData<Map<String, Node>> data = TestData.producedThrough( GraphDescription.createGraphFor( this, true ) );

    private GraphDatabaseService graphdb;

    @Test
    @GraphDescription.Graph(value = {
            "John friend Sara", "John friend Joe",
            "Sara friend Maria", "Joe friend Steve"
    }, autoIndexNodes = true)
    public void testJSONSerialization()
    {
        data.get();

        JSONSerializer serializer = new JSONSerializer();
        String query = start( lookup( "john", "node_auto_index", "name", "John" ) ).
                match( node( "john" ).out( "friend" ).node().out( "friend" ).node( "fof" ) ).
                returns( as( identifier( "john" ).property( "name" ), "name" ), as( identifier( "fof" ).property(
                        "name" ), "friend" ), identifier( "john" ), as( count(), "count" ) )
                .toString();
        try (Transaction tx = graphdb.beginTx()) {
            String json = serializer.toJSON( graphdb.execute( query ) ).toString();
            System.out.println( json );
            tx.success();
        }
    }

    @Test
    @GraphDescription.Graph(value = {
            "John friend Sara", "John friend Joe",
            "Sara friend Maria", "Joe friend Steve"
    }, autoIndexNodes = true)
    public void testIterableJSONSerialization()
    {
        data.get();

        JSONSerializer serializer = new JSONSerializer();
        String query = start( lookup( "john", "node_auto_index", "name", "John" ), lookup( "maria",
                "node_auto_index", "name", "Maria" ) )
                .match( path( "p", shortestPath( node( "john" ).out().hops( null, 3 ).node( "maria" ) ) ) )
                .returns( identifier( "p" ) )
                .toString();
        System.out.println( query );
        try (Transaction tx = graphdb.beginTx()) {
            String json = serializer.toJSON( graphdb.execute( query ) ).toString();
            System.out.println( json );
            tx.success();
        }
    }

    @Before
    public void setup()
            throws IOException
    {
        graphdb = new TestGraphDatabaseFactory().newImpermanentDatabase();
    }

    @After
    public void tearDown() 
    {
	    data.get().clear();
	    graphdb.shutdown();
    }

    @Override
    public GraphDatabaseService graphdb()
    {
        return graphdb;
    }
}
