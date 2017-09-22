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

import org.junit.Test;

import static org.neo4j.cypherdsl.CypherQuery.*;

/**
 * Tests for all parts of the Cypher DSL.
 */
public class CypherQueryTest2 extends AbstractCypherTest
{
    @Test
    public void testDSL()
    {
        assertQueryEquals( CYPHER + "START john=node(0) RETURN john", new CypherQuery()
        {{
                starts( nodesById( "john", 0 ) ).returns( identifier( "john" ) );
            }}.toString() );

        assertQueryEquals( CYPHER + "START john=node(0) MATCH r=(john)-[:KNOWS*1..3]->(x) RETURN x", new CypherQuery()
        {{
                starts( nodesById( "john", 0 ) ).
                        match( path( "r", node( "john" )
                                .out( "KNOWS" )
                                .hops( 1, 3 )
                                .node( "x" ) ) ).
                        returns( identifier( "x" ) );
            }}.toString() );

        assertQueryEquals( CYPHER + "START n=node(3,1) WHERE (n.age<30 and n.name=\"Tobias\") or not(n" +
                ".name=\"Tobias\") RETURN n", new CypherQuery()
        {{
                starts( nodesById( "n", 3, 1 ) ).
                        where( identifier( "n" ).property( "age" ).lt( 30 ).and( identifier( "n" ).string( "name" )
                                .eq( "Tobias" ) )
                                .or( not( identifier( "n" ).string( "name" ).eq( "Tobias" ) ) ) ).
                        returns( identifier( "n" ) );
            }}.toString() );
    }

    @Test
    public void testAndOrPrecedence()
    {
        assertQueryEquals( CYPHER + "START n=node(1) WHERE n.value=0 and (n.title=\"ololo\" or n.value=1) RETURN n",
                new CypherQuery()
                {{
                        starts( nodesById( "n", 1 ) ).
                                where( identifier( "n" ).number( "value" ).eq( 0 )
                                        .and( identifier( "n" ).string( "title" ).eq( "ololo" ).or( identifier( "n" )
                                                .number( "value" ).eq( 1 ) ) ) ).
                                returns( identifier( "n" ) );
                    }}.toString() );
    }

    @Test
    public void testLabel()
    {
        assertQueryEquals( CYPHER + "CREATE (n:Person:Swedish)",
                new CypherQuery()
                {{
                        creates( node("n").labels( label("Person"), label("Swedish") ) );
                    }}.toString() );
    }

    @Test
    public void testLabelWithSpaces()
    {
        assertQueryEquals( CYPHER + "MATCH (movie:`Movie With Spaces`) RETURN movie",
                match( node( "movie" ).label( "Movie With Spaces" ) ).
                        returns( identifier( "movie" ) ).
                        toString() );
    }

    @Test
    public void testLabelWithSpecialCharacters()
    {
        assertQueryEquals( CYPHER + "MATCH (movie:`Movie\"#'?!`) RETURN movie",
                match( node( "movie" ).label( "Movie\"#'?!" ) ).
                        returns( identifier( "movie" ) ).
                        toString() );
    }

    @Test
    public void testVariableLengthRelationshipWithProperties()
    {
        assertQueryEquals(CYPHER + "MATCH ()-[r:RATED*1.. {rating:5}]->() RETURN r",
                match(node().out("RATED").hops(1, null).values(value("rating", 5)).as("r").node()).
                        returns(identifier("r")).toString());
    }

    @Test
    public void testRelationshipWithOnlyProperties()
    {
        assertQueryEquals(CYPHER + "MATCH ()-[ {rating:5}]->(n) RETURN n",
                match(node().out().values(value("rating", 5)).node("n")).
                        returns(identifier("n")).toString());
    }

    @Test
    public void testLabelsFunction()
    {
        assertQueryEquals(CYPHER + "MATCH (n) RETURN labels(n)",
                match(node("n")).returns(labels(identifier("n"))).toString());
    }
}
