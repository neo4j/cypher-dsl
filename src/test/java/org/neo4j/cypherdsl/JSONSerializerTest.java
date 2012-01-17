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
import org.neo4j.cypherdsl.result.JSONSerializer;
import org.neo4j.cypherdsl.result.NameResolver;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.test.GraphDescription;
import org.neo4j.test.GraphHolder;
import org.neo4j.test.ImpermanentGraphDatabase;
import org.neo4j.test.TestData;

import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.MatchExpression.*;
import static org.neo4j.cypherdsl.query.ReturnExpression.*;
import static org.neo4j.cypherdsl.query.StartExpression.*;

/**
 * TODO
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
        String query = start(lookup("john", "node_auto_index", "name", "John"))
                .match(path().from("john").out("friend")
                        .link().out("friend").to("fof"))
                .returns(properties("john.name", "fof.name"), nodes("john"), count())
                .toString();
        String json = serializer.toJSON( engine.execute(query) ).toString();
        System.out.println( json );

        // Now replace names to make it prettier
        json = serializer.toJSON( new NameResolver().
                replace("john.name", "name").
                replace("fof.name", "friend").
                replace("count(*)", "count").
                replace("john", "node").map(engine.execute( query ))  ).toString();
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
            .match( shortestPath( "p" ).from( "john" ).out().hops( null,3 ).to( "maria" ) )
            .returns( paths( "p" ) )
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
