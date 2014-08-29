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

}
