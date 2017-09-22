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
package org.neo4j.cypherdsl.querydsl;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.neo4j.cypherdsl.grammar.Execute;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Result;
import org.neo4j.test.*;
import org.neo4j.test.GraphDescription;
import org.neo4j.test.GraphHolder;
import org.neo4j.test.TestData;
import org.neo4j.test.TestGraphDatabaseFactory;

import java.io.IOException;
import java.util.Map;

import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.neo4j.StartExpressionNeo.nodeById;

/**
 * Set up a query using the CypherQuery builder, and then use it to execute a query to a test database and project
 * the results.
 */
public class ProjectionTest
        implements GraphHolder
{
    public
    @Rule
    TestData<Map<String, Node>> data = TestData.producedThrough( GraphDescription.createGraphFor(
            this, true ) );

    private GraphDatabaseService graphdb;

    @Test
    @GraphDescription.Graph(value = {
            "John friend Sara", "John friend Joe",
            "Sara friend Maria", "Joe friend Steve"
    }, autoIndexNodes = true)
    public void testCypherExecution()
            throws Exception
    {
        data.get();

        String query = start( lookup( "john", "node_auto_index", "name", "John" ) )
                .match( node( "john" ).out( "friend" ).node().out( "friend" ).node( "fof" ) )
                .returns( identifier( "john" ).property( "name" ), identifier( "fof" ).property( "name" ),
                        identifier( "john" ) )
                .toString();

        System.out.println( query );
        Result result = graphdb.execute( query );
        Node john = null;
        while (result.hasNext()) {
            Map<String, Object> row = result.next();
            john = ((Node) row.get( "john" ));
        }

        {
            Execute q = start( nodeById( "john", john ) )
                    .match( node( "john" ).out( "friend" )
                            .node()
                            .out( "friend" )
                            .node( "fof" ) )
                    .returns( identifier( "john" ).property( "name" ), identifier( "fof" )
                            .property( "name" ) );

            System.out.println( q );
            System.out.println( graphdb.execute( q.toString() ).toString() );
        }

        {
            Projection<Friend> projection = new Projection<Friend>( Friend.class );
            Iterable<Friend> friends = projection.iterable( graphdb.execute( start( nodeById( "john", john ) )
                    .match( node( "john" ).out( "friend" )
                            .node()
                            .out( "friend" )
                            .node( "fof" ) )
                    .returns( as( identifier( "john" ).property( "name" ), "name" ),
                            as( identifier( "fof" ).property( "name" ), "friend" ) ).toString() ) );
            System.out.println( friends );
        }
    }

    @Before
    public void setup()
            throws IOException
    {
        graphdb = new TestGraphDatabaseFactory().newImpermanentDatabase();
    }

    @Override
    public GraphDatabaseService graphdb()
    {
        return graphdb;
    }

    public static class Friend
    {
        public String name;
        public String friend;

        @Override
        public String toString()
        {
            return name + " is friend with " + friend;
        }
    }
}
