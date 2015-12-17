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
package org.neo4j.cypherdsl.querydsl;

import static org.neo4j.cypherdsl.CypherQuery.as;
import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.CypherQuery.lookup;
import static org.neo4j.cypherdsl.CypherQuery.node;
import static org.neo4j.cypherdsl.CypherQuery.start;
import static org.neo4j.cypherdsl.query.neo4j.StartExpressionNeo.nodeById;

import java.io.IOException;
import java.util.Map;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.neo4j.cypherdsl.grammar.Execute;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Result;
import org.neo4j.test.*;

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
        while (result.hasNext())
        {
            Map<String, Object> row = result.next();
            john = ((Node) row.get( "john" ));
        }
        System.out.println( result.toString() );

        {
            Execute q = start( nodeById( "john", john ) )
                    .match( node( "john" ).out( "friend" )
                            .node()
                            .out( "friend" )
                            .node( "fof" ) )
                    .returns( identifier( "john" ).property( "name" ), identifier( "fof" )
                            .property( "name" ) );

            System.out.println( q );
            System.out.println( graphdb.execute( q.toString() ).resultAsString() );
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
