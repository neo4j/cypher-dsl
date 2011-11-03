/**
 * Copyright (c) 2002-2011 "Neo Technology,"
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

import static org.junit.Assert.*;

/**
 * Tests for all parts of the Cypher DSL.
 */
public class CypherQueryTest2
{
    @Test
    public void testDSL()
    {
        assertEquals( "START john=node(0) RETURN john", new CypherQuery()
        {{
            starts( node( "john", 0 ) ).returns( nodes( "john" ) );
        }}.toString() );

        assertEquals( "START john=node(0) MATCH r=(john)-[?:KNOWS*1..3]->(x) RETURN x", new CypherQuery()
        {{
            starts( node( "john", 0 ) ).
            match( path( "r" ).from( "john" )
                       .optional()
                       .out( "KNOWS" )
                       .hops( 1, 3 )
                       .to( "x" ) ).returns( nodes( "x" ) );
        }}.toString() );

        assertEquals( "START n=node(3,1) WHERE (n.age<30 and n.name=\"Tobias\") or not(n.name=\"Tobias\") RETURN n", new CypherQuery()
        {{
            starts( node( "n", 3, 1 ) ).
            where( number( "n.age" ).lt( 30 ).and( string( "n.name").eq( "Tobias" ) )
                       .or( not( string( "n.name" ).eq( "Tobias" ) ) ) ).
            returns( nodes( "n" ) );
        }}.toString() );
    }
}
