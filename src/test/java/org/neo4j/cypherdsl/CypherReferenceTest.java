/*
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
import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.MatchExpression.*;
import static org.neo4j.cypherdsl.MatchExpression.Direction.*;
import static org.neo4j.cypherdsl.WhereExpression.*;

/**
 * Construct Cypher queries corresponding to the Cypher Reference manual
 */
public class CypherReferenceTest
{
    @Test
    public void test15_3_1()
    {
        assertEquals( "START n=node(1) RETURN n",
                      newQuery().nodes( "n", 1 ).returnNodes( "n" ).toString() );
    }

    @Test
    public void test15_3_2()
    {
        assertEquals( "START r=relationship(0) RETURN r",
                      newQuery().
                          relationships( "r", 0 )
                          .returnRelationships( "r" )
                          .toString() );
    }

    @Test
    public void test15_3_3()
    {
        assertEquals( "START n=node(1,2,3) RETURN n",
                      newQuery().
                          nodes( "n", 1, 2, 3 ).
                          returnNodes( "n" ).
                          toString() );
    }

    @Test
    public void test15_3_4()
    {
        assertEquals( "START n=node:nodes(name=\"A\") RETURN n",
                      newQuery().
                          nodesLookup( "n", "nodes", "name", "A" ).
                          returnNodes( "n" ).
                          toString() );
    }

    @Test
    public void test15_3_5()
    {
        assertEquals( "START r=relationship:rels(property=\"some_value\") RETURN r",
                      newQuery().
                          relationshipsLookup( "r", "rels", "property", "some_value" ).
                          returnNodes( "r" ).
                          toString() );
    }

    @Test
    public void test15_3_6()
    {
        assertEquals( "START n=node:nodes(\"name:A\") RETURN n",
                      newQuery().
                          nodesQuery( "n", "nodes", "name:A" ).
                          returnNodes( "n" ).
                          toString() );
    }

    @Test
    public void test15_3_7()
    {
        assertEquals( "START a=node(1),b=node(2) RETURN a,b",
                      newQuery().
                          nodes( "a", 1 ).
                          nodes( "b", 2 ).
                          returnNodes( "a", "b" ).
                          toString() );
    }

    @Test
    public void test15_4_1()
    {
        assertEquals( "START n=node(3) MATCH (n)--(x) RETURN x",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", "x" ) ).
                          returnNodes( "x" ).
                          toString() );
    }

    @Test
    public void test15_4_2()
    {
        assertEquals( "START n=node(3) MATCH (n)-->(x) RETURN x",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", "x" ).direction( OUTGOING ) ).
                          returnNodes( "x" ).
                          toString() );
    }

    @Test
    public void test15_4_3()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r]->(x) RETURN r",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", "x" ).direction( OUTGOING ).name( "r" ) ).
                          returnNodes( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_4()
    {
        assertEquals( "START n=node(3) MATCH (n)-[:BLOCKS]->(x) RETURN x",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", "x" ).direction( OUTGOING ).relationship( "BLOCKS" ) ).
                          returnNodes( "x" ).
                          toString() );
    }

    @Test
    public void test15_4_5()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r:BLOCKS]->(x) RETURN r",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", "x" ).direction( OUTGOING ).name( "r" ).relationship( "BLOCKS" ) ).
                          returnNodes( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_6()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r:`TYPE WITH SPACE IN IT`]->(x) RETURN r",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", "x" ).direction( OUTGOING )
                                     .name( "r" )
                                     .relationship( "`TYPE WITH SPACE IN IT`" ) ).
                          returnNodes( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_7()
    {
        assertEquals( "START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c) RETURN a,b,c",
                      newQuery().
                          nodes( "a", 3 ).
                          match( path( "a", "b" ).direction( OUTGOING )
                                     .relationship( "KNOWS" )
                                     .path( "c" )
                                     .direction( OUTGOING )
                                     .relationship( "KNOWS" ) ).
                          returnNodes( "a", "b", "c" ).
                          toString() );
    }

    @Test
    public void test15_4_8()
    {
        assertEquals( "START a=node(3),x=node(2,4) MATCH (a)-[:KNOWS*1..3]->(x) RETURN a,x",
                      newQuery().
                          nodes( "a", 3 ).nodes( "x", 2, 4 ).
                          match( path( "a", "x" ).direction( OUTGOING ).relationship( "KNOWS" ).hops( 1, 3 ) ).
                          returnNodes( "a", "x" ).
                          toString() );
    }

    @Test
    public void test15_4_9()
    {
        assertEquals( "START a=node(3) MATCH p1=(a)-[:KNOWS*0..1]->(b),p2=(b)-[:KNOWS*0..1]->(c) RETURN a,b,c,length(p1),length(p2)",
                      newQuery().
                          nodes( "a", 3 ).
                          match( MatchExpression.named( "p1", path( "a", "b" ).direction( OUTGOING )
                              .relationship( "KNOWS" )
                              .hops( 0, 1 ) ) ).
                          match( MatchExpression.named( "p2", path( "b", "c" ).direction( OUTGOING )
                              .relationship( "KNOWS" )
                              .hops( 0, 1 ) ) ).
                          returnNodes( "a", "b", "c" ).returnLength( "p1" ).returnLength( "p2" ).
                          toString() );
    }

    @Test
    public void test15_4_10()
    {
        assertEquals( "START a=node(2) MATCH (a)-[?]->(b) RETURN a,x",
                      newQuery().
                          nodes( "a", 2 ).
                          match( path( "a", "b" ).direction( OUTGOING ).optional()).
                          returnNodes( "a", "x").
                          toString() );
    }

    @Test
    public void test15_4_11()
    {
        assertEquals( "START a=node(3) MATCH (a)-[r?:LOVES]->() RETURN a,r",
                      newQuery().
                          nodes( "a", 3 ).
                          match( path( "a", "" ).direction( OUTGOING ).name( "r" ).relationship( "LOVES" ).optional()).
                          returnNodes( "a").returnRelationships( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_12()
    {
        assertEquals( "START a=node(2) MATCH (a)-[?]->(x) RETURN x,x.name",
                      newQuery().
                          nodes( "a", 2 ).
                          match( path( "a", "x" ).direction( OUTGOING ).optional()).
                          returnNodes( "x").returnProperties( "x.name" ).
                          toString() );
    }

    @Test
    public void test15_4_13()
    {
        assertEquals( "START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c),(a)-[:BLOCKS]-(d)-[:KNOWS]-(c) RETURN a,b,c,d",
                      newQuery().
                          nodes( "a", 3 ).
                          match( path( "a", "b" ).relationship( "KNOWS" ).direction( OUTGOING ).path( "c" ).relationship( "KNOWS" ).direction( OUTGOING )).
                          match( path( "a", "d" ).relationship( "BLOCKS" ).path( "c" ).relationship( "KNOWS" )).
                          returnNodes( "a","b","c","d").
                          toString() );
    }

    @Test
    public void test15_4_14()
    {
        assertEquals( "START d=node(1),e=node(2) MATCH p=shortestPath((d)-[*..15]->(e)) RETURN p",
                      newQuery().
                          nodes( "d", 1 ).nodes( "e", 2 ).
                          match( named( "p", shortestPath( "d", "e" ).hops( null, 15 )
                              .direction( OUTGOING ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_4_15()
    {
        assertEquals( "START a=node(3) MATCH p=(a)-->(b) RETURN p",
                      newQuery().
                          nodes( "a", 3 ).
                          match( named( "p", path( "a","b" )
                              .direction( OUTGOING ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_4_16()
    {
        assertEquals( "START r=relationship(0) MATCH (a)-[r]-(b) RETURN a,b",
                      newQuery().
                          relationships( "r", 0 ).
                          match( path( "a","b" ).name( "r" )).
                          returnNodes( "a","b" ).
                          toString() );
    }

    @Test
    public void test15_5_1()
    {
        assertEquals( "START n=node(3,1) WHERE (n.age<30 and n.name=\"Tobias\") or not(n.name=\"Tobias\") RETURN n",
                      newQuery().
                          nodes( "n",3,1).
                          where( lt( "n.age", 30 ).and( eq( "n.name", "Tobias" )).or( not( eq( "n.name", "Tobias" ) ) ) ).
                          returnNodes( "n").
                          toString() );
    }

    @Test
    public void test15_5_2()
    {
        assertEquals( "START n=node(3,1) WHERE n.age<30 RETURN n",
                      newQuery().
                          nodes( "n", 3, 1 ).
                          where( lt( "n.age", 30 ) ).
                          returnNodes( "n" ).
                          toString() );
    }

    @Test
    public void test15_5_3()
    {
        assertEquals( "START n=node(3,1) WHERE n.name=~/Tob.*/ RETURN n",
                      newQuery().
                          nodes( "n", 3, 1 ).
                          where( WhereExpression.regexp( "n.name", "Tob.*" )).
                          returnNodes( "n" ).
                          toString() );
    }

    @Test
    public void test15_5_4()
    {
        assertEquals( "START n=node(3,1) WHERE n.belt?=\"white\" RETURN n",
                      newQuery().
                          nodes( "n", 3, 1 ).
                          where( eq( optional( "n.belt" ), "white" ) ).
                          returnNodes( "n" ).
                          toString() );
    }

    @Test
    public void test15_5_5()
    {
        assertEquals( "START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is null RETURN b",
                      newQuery().
                          nodes( "a", 1 ).nodes( "b",3,2 ).
                          match( path( "a","b" ).direction( INCOMING ).name( "r" ).optional() ).
                          where( isNull( "r" )).
                          returnNodes( "b" ).
                          toString() );
    }

    @Test
    public void test15_5_5_2()
    {
        assertEquals( "START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is not null RETURN b",
                      newQuery().
                          nodes( "a", 1 ).nodes( "b", 3, 2 ).
                          match( path( "a", "b" ).direction( INCOMING ).name( "r" ).optional() ).
                          where( isNotNull( "r" ) ).
                          returnNodes( "b" ).
                          toString() );
    }

    @Test
    public void test15_6_1()
    {
        assertEquals( "START n=node(2) RETURN n",
                      newQuery().
                          nodes( "n", 2 ).
                          returnNodes( "n" ).
                          toString() );
    }

    @Test
    public void test15_6_2()
    {
        assertEquals( "START n=node(1) MATCH (n)-[r:KNOWS]->(c) RETURN r",
                      newQuery().
                          nodes( "n", 1 ).
                          match( MatchExpression.path( "n","c" ).name( "r" ).relationship( "KNOWS" ).direction( OUTGOING ) ).
                          returnRelationships( "r" ).
                          toString() );
    }

    @Test
    public void test15_6_3()
    {
        assertEquals( "START n=node(1) RETURN n.name",
                      newQuery().
                          nodes( "n", 1 ).
                          returnProperties( "n.name" ).
                          toString() );
    }

    @Test
    public void test15_6_4()
    {
        assertEquals( "START `This isn't a common identifier`=node(1) RETURN `This isn't a common identifier`.`<<!!__??>>`",
                      newQuery().
                          nodes( "`This isn't a common identifier`", 1 ).
                          returnProperties( "`This isn't a common identifier`.`<<!!__??>>`" ).
                          toString() );
    }

    @Test
    public void test15_6_5()
    {
        assertEquals( "START n=node(1,2) RETURN n.age?",
                      newQuery().
                          nodes( "n", 1,2 ).
                          returnProperties( true, "n.age" ).
                          toString() );
    }

    @Test
    public void test15_6_6()
    {
        assertEquals( "START a=node(1) MATCH (a)-->(b) RETURN distinct b",
                      newQuery().
                          nodes( "n", 1,2 ).
                          match( path( "a","b" ).direction( OUTGOING ) ).
                          returnNodes( true, "b" ).
                          toString() );
    }
}
