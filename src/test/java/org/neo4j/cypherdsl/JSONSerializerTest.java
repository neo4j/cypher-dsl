/**
 * Copyright (c) 2002-2012 "Neo Technology,"
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

import java.io.IOException;
import java.util.Map;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.cypher.javacompat.ExecutionResult;
import org.neo4j.cypherdsl.result.JSONSerializer;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.neo4j.test.GraphDescription;
import org.neo4j.test.GraphHolder;
import org.neo4j.test.ImpermanentGraphDatabase;
import org.neo4j.test.TestData;

import static org.neo4j.cypherdsl.CypherQuery.*;

/**
 * Test of JSON serialization of results.
 */
public class JSONSerializerTest
    implements GraphHolder
{
    public @Rule
    TestData<Map<String, Node>> data = TestData.producedThrough( GraphDescription.createGraphFor(this, true ) );

    private ImpermanentGraphDatabase graphdb;
    private ExecutionEngine engine;

    @Test
    @GraphDescription.Graph( value = {
        "John friend Sara", "John friend Joe",
        "Sara friend Maria", "Joe friend Steve"
    }, autoIndexNodes = true )
    public void testJSONSerialization()
    {
        data.get();

        JSONSerializer serializer = new JSONSerializer();
        String query = start(lookup("john", "node_auto_index", "name", "John")).
                        match( node( "john" ).out( "friend" ).node().out( "friend" ).node( "fof" ) ).
                        returns( as( identifier( "john" ).property( "name" ) , "name" ), as( identifier( "fof" ).property( "name" ) ,"friend" ), identifier( "john" ), as( count(), "count" ) )
                        .toString();
        
        // set properties on a node so we can requery it later and see it can be serialized into json
        ExecutionResult result = engine.execute(query);
        
        Transaction tx = graphdb.beginTx();
        for (Map<String, Object> map : result) {
			Node n = (Node) map.get("john");
			n.setProperty("doubleValue", 3.14d);
			n.setProperty("floatValue", 3.14f);
			n.setProperty("byteValue", Byte.MAX_VALUE);
			n.setProperty("boolValue", true);
			n.setProperty("stringArray", new String[] {"one", "two", "three"});
			n.setProperty("primitiveArray", new int[] {1, 2, 3});
		}
        tx.success();
        
        // requery and serialize into json
        String json = serializer.toJSON( engine.execute(query) ).toString();
        System.out.println( json );
    }

    @Test
    @GraphDescription.Graph( value = {
        "John friend Sara", "John friend Joe",
        "Sara friend Maria", "Joe friend Steve"
    }, autoIndexNodes = true )
    public void testIterableJSONSerialization()
    {
        data.get();

        JSONSerializer serializer = new JSONSerializer();
        String query = start( lookup( "john", "node_auto_index", "name", "John" ), lookup( "maria", "node_auto_index", "name", "Maria" ) )
            .match( path( "p", shortestPath( node( "john" ).out().hops( null,3 ).node( "maria" ) )))
            .returns( identifier( "p" ) )
            .toString();
        System.out.println( query );
        String json = serializer.toJSON(engine.execute(query)).toString();
        System.out.println( json );
    }

    @Before
    public void setup()
        throws IOException
    {
        graphdb = new ImpermanentGraphDatabase();
        graphdb.cleanContent( false );

        engine = new ExecutionEngine( graphdb );
    }

    @Override
    public GraphDatabaseService graphdb()
    {
        return graphdb;
    }
}
