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

import org.junit.Test;
import org.neo4j.cypherdsl.query.Identifier;
import org.neo4j.cypherdsl.query.Order;
import org.neo4j.cypherdsl.query.Query;

import static org.junit.Assert.*;
import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.Order.*;

/**
 * Construct Cypher queries corresponding to the Cypher Reference manual
 */
public class CypherReferenceTest
{
    public static final String CYPHER="CYPHER 1.8 ";

    @Test
    public void test16_9_1()
    {
        assertEquals( CYPHER + "START n=node(1) RETURN n",
                      start( nodesById( "n", 1 ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test16_9_2()
    {
        assertEquals( CYPHER + "START r=relationship(0) RETURN r",
                      start( relationshipsById( "r", 0 ) ).returns( identifier( "r" ) ).toString() );
    }

    @Test
    public void test16_9_3()
    {
        assertEquals( CYPHER+"START n=node(1,2,3) RETURN n",
                      start( nodesById( "n", 1, 2, 3 ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test16_9_4()
    {
        assertEquals( CYPHER+"START n=node(*) RETURN n",
                      start( allNodes( "n" ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test16_9_5()
    {
        assertEquals( CYPHER+"START n=node:nodes(name=\"A\") RETURN n",
                      start( lookup( "n", "nodes", "name", "A" ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test16_9_6()
    {
        assertEquals( CYPHER+"START r=relationship:rels(name=\"Andres\") RETURN r",
                      start( relationshipLookup( "r", "rels", "name", "Andres" ) ).
                          returns( identifier( "r" ) ).toString() );
    }

    @Test
    public void test16_9_7()
    {
        assertEquals( CYPHER+"START n=node:nodes(\"name:A\") RETURN n",
                      start( query( "n", "nodes", "name:A" ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test16_9_8()
    {
        assertEquals( CYPHER + "START a=node(1),b=node(2) RETURN a,b",
                      start( nodesById( "a", 1 ), nodesById( "b", 2 ) ).returns( identifier( "a" ), identifier( "b" ) )
                          .toString() );
    }

    @Test
    public void test16_10_2()
    {
        assertEquals( CYPHER + "START n=node(3) MATCH (n)--(x) RETURN x",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).both().node( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test16_10_3()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-->(x) RETURN x",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out().node( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test16_10_4()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r]->() RETURN r",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out().as( "r" ).node() ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test16_10_5()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[:BLOCKS]->(x) RETURN x",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out( "BLOCKS" ).node( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test16_10_6()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[:BLOCKS|KNOWS]->(x) RETURN x",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out( "BLOCKS","KNOWS" ).node( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test16_10_7()
    {
        assertEquals( CYPHER + "START n=node(3) MATCH (n)-[r:BLOCKS]->() RETURN r",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out( "BLOCKS" ).as( "r" ).node() ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test16_10_8()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r:`TYPE WITH SPACE IN IT`]->() RETURN r",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out( "TYPE WITH SPACE IN IT" ).as( "r" ).node() ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test16_10_9()
    {
        assertEquals( CYPHER + "START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c) RETURN a,b,c",
                      start( nodesById( "a", 3 ) ).
                          match( node( "a" ).out( "KNOWS" ).node( "b" ).out( "KNOWS" ).node( "c" ) ).
                          returns( identifiers( "a", "b", "c" ) ).
                          toString() );
    }

    @Test
    public void test16_10_10()
    {
        assertEquals( CYPHER+"START a=node(3),x=node(2,4) MATCH (a)-[:KNOWS*1..3]->(x) RETURN a,x",
                      start( nodesById( "a", 3 ), nodesById( "x", 2, 4 ) ).
                          match( node( "a" ).out( "KNOWS" ).hops( 1, 3 ).node( "x" ) ).
                          returns( identifiers( "a", "x" ) ).
                          toString() );
    }

    @Test
    public void test16_10_11()
    {
        assertEquals( CYPHER+"START a=node(3),x=node(2,4) MATCH (a)-[r:KNOWS*1..3]->(x) RETURN r",
                      start( nodesById( "a", 3 ), nodesById( "x", 2, 4 ) ).
                          match( node( "a" ).out( "KNOWS" ).hops( 1, 3 ).as( "r" ).node( "x" ) ).
                          returns( identifiers( "r" ) ).
                          toString() );
    }

    @Test
    public void test16_10_12()
    {
        assertEquals( CYPHER + "START a=node(3) MATCH p1=(a)-[:KNOWS*0..1]->(b),p2=(b)-[:BLOCKS*0..1]->(c) RETURN a,b,c,length(p1),length(p2)",
                      start( nodesById( "a", 3 ) ).
                          match( path( "p1", node( "a" ).out( "KNOWS" ).hops( 0, 1 ).node( "b" ) ),
                                 path( "p2", node( "b" ).out( "BLOCKS" ).hops( 0, 1 ).node( "c" ) )).
                          returns( identifier( "a" ), identifier( "b" ), identifier( "c" ), length( identifier( "p1" ) ), length( identifier( "p2" ) ) ).
                          toString() );
    }

    @Test
    public void test16_10_13()
    {
        assertEquals( CYPHER+"START a=node(2) MATCH (a)-[?]->(x) RETURN a,x",
                      start( nodesById( "a", 2 ) ).
                          match( node( "a" ).out().optional().node( "x" ) ).
                          returns( identifiers( "a", "x" ) ).
                          toString() );
    }

    @Test
    public void test16_10_14()
    {
        assertEquals( CYPHER + "START a=node(3) MATCH (a)-[r?:LOVES]->() RETURN a,r",
                      start( nodesById( "a", 3 ) ).
                          match( node( "a" ).out( "LOVES" ).as( "r" ).optional().node() ).
                          returns( identifier( "a" ), identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test16_10_15()
    {
        assertEquals( CYPHER+"START a=node(2) MATCH (a)-[?]->(x) RETURN x,x.name",
                      start( nodesById( "a", 2 ) ).
                          match( node( "a" ).out().optional().node( "x" ) ).
                          returns( identifier( "x" ), identifier( "x" ).string( "name" ) ).
                          toString() );
    }

    @Test
    public void test16_10_16()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c),(a)-[:BLOCKS]-(d)-[:KNOWS]-(c) RETURN a,b,c,d",
                      start( nodesById( "a", 3 ) ).
                          match( node( "a" ).out( "KNOWS" ).node( "b" ).out( "KNOWS" ).node( "c" ),
                                 node( "a" ).both( "BLOCKS" ).node( "d" ).both( "KNOWS" ).node( "c" ) ).
                          returns( identifiers( "a", "b", "c", "d" ) ).
                          toString() );
/*
                      start( node( "a", 3 ) ).
                          match( path().from( "a" ).out( "KNOWS" ).to( "b" ).link().out( "KNOWS" ).to( "c" ),
                                 path().from( "a" ).both( "BLOCKS" ).to( "d" ).link().both( "KNOWS" ).to( "c" ) ).
                          returns( identifiers( "a", "b", "c", "d" ) ).
                          toString() );
*/
    }

    @Test
    public void test16_10_17()
    {
        assertEquals( CYPHER+"START d=node(1),e=node(2) MATCH p=shortestPath((d)-[*..15]->(e)) RETURN p",
                      start( nodesById( "d", 1 ), nodesById( "e", 2 ) ).
                          match( path( "p", shortestPath( node( "d" ).out().hops( null, 15 ).node( "e" ) ) )).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test16_10_18()
    {
        assertEquals( CYPHER+"START d=node(1),e=node(2) MATCH p=allShortestPaths((d)-[*..15]->(e)) RETURN p",
                      start( nodesById( "d", 1 ), nodesById( "e", 2 ) ).
                          match( path( "p", allShortestPaths( node( "d" ).out().hops( null, 15 ).node( "e" ) ))).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test16_10_19()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH p=(a)-->(b) RETURN p",
                      start( nodesById( "a", 3 ) ).
                          match( path( "p", node( "a" ).out().node( "b" ) )).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test16_10_20()
    {
        assertEquals( CYPHER + "START a=relationship(0) MATCH (a)-[r]-(b) RETURN a,b",
                      start( relationshipsById( "a", 0 ) ).
                          match( node( "a" ).both().as( "r" ).node( "b" ) ).
                          returns( identifier( "a" ), identifier( "b" ) ).
                          toString() );
    }

    @Test
    public void test16_10_21()
    {
        assertEquals( CYPHER + "START a=node(3),b=node(2) MATCH (a)-[?:KNOWS]-(x)-[?:KNOWS]-(b) RETURN x",
                      start( nodesById( "a", 3 ), nodesById( "b", 2 ) ).
                          match( node( "a" ).
                              both( "KNOWS" ).optional().
                              node( "x" ).
                              both( "KNOWS" ).optional().
                              node( "b" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test16_11_1()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE (n.age<30 and n.name=\"Tobias\") or not(n.name=\"Tobias\") RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( identifier( "n" ).number( "age" ).lt( 30 ).and( identifier( "n" ).string( "name" ).eq( "Tobias" ) )
                                     .or( not( identifier( "n" ).string( "name" ).eq( "Tobias" ) ) ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test16_11_2()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.age<30 RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( identifier( "n" ).number( "age" ).lt( 30 ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test16_11_3()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.name=~/Tob.*/ RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( identifier( "n" ).string( "name" ).regexp( "Tob.*" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test16_11_4()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.name=~/Some\\/thing/ RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( identifier( "n" ).string( "name" ).regexp( "Some/thing" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test16_11_5()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.name=~/(?i)ANDR.*/ RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( identifier( "n" ).string( "name" ).regexp( "ANDR.*", false ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test16_11_6()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r]->() WHERE type(r)=~/K.*/ RETURN r",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out().as( "r" ).node() ).
                          where( type( identifier( "r" ) ).regexp( literal( "K.*" ) ) ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test16_11_7()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE has(n.belt) RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( has( identifier( "n" ).property( "belt" ))).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test16_11_8()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.belt?=\"white\" RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( identifier( "n").property( "belt" ).trueIfMissing().eq( "white" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test16_11_9()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.belt!=\"white\" RETURN n",
                      start( nodesById( "n", 3, 1 ) ).
                          where( identifier("n").property( "belt").falseIfMissing().eq( "white" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }


    @Test
    public void test16_11_10()
    {
        assertEquals( CYPHER+"START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is null RETURN b",
                      start( nodesById( "a", 1 ), nodesById( "b", 3, 2 ) ).
                          match( node( "a" ).in().as( "r" ).optional().node( "b" ) ).
                          where( isNull( identifier("r" )) ).
                          returns( identifier( "b" ) ).
                          toString() );
    }

    @Test
    public void test16_11_10_2()
    {
        assertEquals( CYPHER+"START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is not null RETURN b",
                      start( nodesById( "a", 1 ), nodesById( "b", 3, 2 ) ).
                          match( node( "a" ).in().as( "r" ).optional().node( "b" ) ).
                          where( isNotNull( identifier("r" )) ).
                          returns( identifier( "b" ) ).
                          toString() );
    }

    @Test
    public void test16_11_11()
    {
        assertEquals( CYPHER+"START a=node(1),b=node(3,2) WHERE (a)<--(b) RETURN b",
                      start( nodesById( "a", 1 ), nodesById( "b", 3, 2 ) ).
                          where( node("a").in().node("b") ).
                          returns( identifier( "b" ) ).
                          toString() );
    }

    @Test
    public void test16_11_12()
    {
        assertEquals( CYPHER+"START a=node(3,1,2) WHERE a.name IN [\"Peter\",\"Tobias\"] RETURN a",
                      start( nodesById( "a", 3, 1, 2 ) ).where( identifier( "a").string( "name").in( collection( "Peter", "Tobias" ) )).returns( identifier( "a" ) ).toString() );
    }

    @Test
    public void test16_12_1()
    {
        assertEquals( CYPHER+"START n=node(2) RETURN n",
                      start( nodesById( "n", 2 ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test16_12_2()
    {
        assertEquals( CYPHER+"START n=node(1) MATCH (n)-[r:KNOWS]->(c) RETURN r",
                      start( nodesById( "n", 1 ) ).
                          match( node( "n" ).out( "KNOWS" ).as( "r" ).node( "c" ) ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test16_12_3()
    {
        assertEquals( CYPHER+"START n=node(1) RETURN n.name",
                      start( nodesById( "n", 1 ) ).returns( identifier( "n" ).property( "name" ) ).toString() );
    }

    @Test
    public void test16_12_4()
    {
        assertEquals( CYPHER+"START a=node(1) MATCH p=(a)-[r]->(b) RETURN *",
                      start( nodesById( "a", 1 ) ).match( path( "p", node("a").out().as("r").node( "b"))).returns( all() ).toString() );
    }

    @Test
    public void test16_12_5()
    {
        assertEquals( CYPHER+"START `This isn't a common identifier`=node(1) RETURN `This isn't a common identifier`.happy",
                      start( nodesById( identifier( "This isn't a common identifier" ), 1 ) ).
                          returns( identifier( "This isn't a common identifier").property( "happy" ) ).
                          toString() );
    }

    @Test
    public void test16_12_6()
    {
        assertEquals( CYPHER + "START a=node(1) RETURN a.age AS SomethingTotallyDifferent",
                      start( nodesById( "a", 1 ) ).
                          returns( exp( identifier( "a" ).property( "age" ) ).as( "SomethingTotallyDifferent" ) ).
                          toString() );
    }

    @Test
    public void test16_12_7()
    {
        assertEquals( CYPHER+"START n=node(1,2) RETURN n.age?",
                      start( nodesById( "n", 1, 2 ) ).
                          returns( identifier( "n" ).property( "age" ).optional() ).
                          toString() );
    }

    @Test
    public void test16_12_8()
    {
        assertEquals( CYPHER+"START a=node(1) MATCH (a)-->(b) RETURN DISTINCT b",
                      start( nodesById( "a", 1 ) ).
                          match( node( "a" ).out().node( "b" ) ).
                          returns( exp( identifier( "b" ) ).distinct() ).
                          toString() );
    }

    @Test
    public void test16_13_3()
    {
        assertEquals( CYPHER+"START n=node(2) MATCH (n)-->(x) RETURN n,count(*)",
                      start( nodesById( "n", 2 ) ).
                          match( node( "n" ).out( ).node( "x" ) ).
                          returns( identifier( "n" ), count() ).
                          toString() );
    }

    @Test
    public void test16_13_4()
    {
        assertEquals( CYPHER+"START n=node(2) MATCH (n)-[r]->() RETURN type(r),count(*)",
                      start( nodesById( "n", 2 ) ).
                          match( node( "n" ).out().as( "r" ).node() ).
                          returns( type( identifier("r") ), count() ).
                          toString() );
    }

    @Test
    public void test16_13_5()
    {
        assertEquals( CYPHER+"START n=node(2) MATCH (n)-->(x) RETURN count(x)",
                      start( nodesById( "n", 2 ) ).
                          match( node( "n" ).out().node( "x" ) ).
                          returns( count( identifier( "x" ) ) ).
                          toString() );
    }

    @Test
    public void test16_13_6()
    {
        assertEquals( CYPHER+"START n=node(2,3,4,1) RETURN count(n.property?)",
                      start( nodesById( "n", 2, 3, 4, 1 ) ).
                          returns( count( identifier( "n" ).property( "property" ).optional() ) ).
                          toString() );
    }

    @Test
    public void test16_13_7()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN sum(n.property)",
                      start( nodesById( "n", 2, 3, 4 ) ).returns( sum( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test16_13_8()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN avg(n.property)",
                      start( nodesById( "n", 2, 3, 4 ) ).returns( avg( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test16_13_9()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN max(n.property)",
                      start( nodesById( "n", 2, 3, 4 ) ).returns( max( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test16_13_10()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN min(n.property)",
                      start( nodesById( "n", 2, 3, 4 ) ).returns( min( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test16_13_11()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN collect(n.property)",
                      start( nodesById( "n", 2, 3, 4 ) ).returns( collect( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test16_13_12()
    {
        assertEquals( CYPHER+"START a=node(2) MATCH (a)-->(b) RETURN count(DISTINCT b.eyes)",
                      start( nodesById( "a", 2 ) ).
                          match( node( "a" ).out().node( "b" ) ).
                          returns( count( exp( identifier( "b" ).property( "eyes" ) ).distinct() ) ).
                          toString() );
    }

    @Test
    public void test16_14_1()
    {
        assertEquals( CYPHER+"START n=node(3,1,2) RETURN n ORDER BY n.name",
                      start( nodesById( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "name" ) ).
                          toString() );
    }

    @Test
    public void test16_14_2()
    {
        assertEquals( CYPHER+"START n=node(3,1,2) RETURN n ORDER BY n.age,n.name",
                      start( nodesById( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "age" ), identifier( "n" ).property( "name" ) ).
                          toString() );
    }

    @Test
    public void test16_14_3()
    {
        assertEquals(CYPHER+ "START n=node(3,1,2) RETURN n ORDER BY n.name DESCENDING",
                      start( nodesById( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( order( identifier( "n" ).property( "name" ), DESCENDING ) ).
                          toString() );
    }

    @Test
    public void test16_14_4()
    {
        assertEquals( CYPHER+"START n=node(3,1,2) RETURN n.length?,n ORDER BY n.length?",
                      start( nodesById( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ).property( "length" ).optional(), identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "length" ).optional() ).
                          toString() );
    }

    @Test
    public void test16_15_1()
    {
        assertEquals( CYPHER+"START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 3",
                      start( nodesById( "n", 3, 4, 5, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "name" ) ).
                          skip( 3 ).
                          toString() );
    }

    @Test
    public void test16_15_2()
    {
        assertEquals( CYPHER+"START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 1 LIMIT 2",
                      start( nodesById( "n", 3, 4, 5, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n").property( "name" ) ).
                          skip( 1 ).
                          limit( 2 ).
                          toString() );
    }

    @Test
    public void test16_16_1()
    {
        assertEquals( CYPHER+"START n=node(3,4,5,1,2) RETURN n LIMIT 3",
                      start( nodesById( "n", 3, 4, 5, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          limit( 3 ).
                          toString() );
    }

    @Test
    public void test16_17_1()
    {
        assertEquals( CYPHER+"START david=node(1) MATCH (david)-[otherPerson]->() WITH otherPerson,count(*) AS foaf WHERE foaf>1 RETURN otherPerson",
                      start( nodesById( "david", 1 ) ).
                      match( node( "david" ).out( ).as( "otherPerson" ).node() ).
                      with(identifier( "otherPerson" ), count().as( "foaf" )).
                      where( identifier( "foaf" ).gt( literal( 1 ) ) ).
                      returns( identifier( "otherPerson" ) ).
                      toString());
    }


    @Test
    public void test16_18_1()
    {
        assertEquals( CYPHER + "CREATE (n)",
                      create( node( "n" ) ).toString() );
    }

    @Test
    public void test16_18_2()
    {
        assertEquals( CYPHER+"CREATE (n {name:\"Andres\",title:\"Developer\"})",
                      create(node( identifier( "n" )).values( value( "name", "Andres" ), value( "title", "Developer" ) )).toString());
    }

    @Test
    public void test16_18_3()
    {
        assertEquals( CYPHER+"CREATE (a {name:\"Andres\"}) RETURN a",
                      create(node( "a").values( value( "name", "Andres" ) )).returns( identifier( "a" ) ).toString());
    }

    @Test
    public void test16_18_4()
    {
        assertEquals( CYPHER+"START a=node(1),b=node(2) CREATE (a)-[r:REL]->(b) RETURN r",
                      start( nodesById( "a", 1 ), nodesById( "b", 2 ) ).create(node( "a" ).out( "REL" ).as( identifier( "r" ) ).node( identifier( "b" ) ) ).returns( identifier( "r" ) ).toString());
    }

    @Test
    public void test16_18_5()
    {
        assertEquals( CYPHER + "START a=node(1),b=node(2) CREATE (a)-[r:REL {name:a.name+\"<->\"+b.name}]->(b) RETURN r",
                      start( nodesById( "a", 1 ), nodesById( "b", 2 ) ).create( node( "a" )
                                                                          .out( identifier("REL")).values( value( "name", identifier( "a" )
                                                                              .string( "name" )
                                                                              .concat( "<->" )
                                                                              .concat( identifier( "b" ).string( "name" ) ) ) )
                                                                          .as( "r" )
                                                                          .node( identifier( "b" ) ) )
                          .returns( identifier( "r" ) ).toString() );
    }

    @Test
    public void test16_18_6()
    {
        assertEquals( CYPHER+"CREATE (andres {name:\"Andres\"})-[:WORKS_AT]->(neo)<-[:WORKS_AT]-(michael {name:\"Michael\"}) RETURN andres,michael",
                      create(node( "andres").values( value( "name", "Andres" ) ).out( "WORKS_AT" ).node( identifier( "neo" ) ).in( "WORKS_AT" ).node( "michael" ).values( value( "name", "Michael" ) )).returns( identifier( "andres" ), identifier( "michael" ) ).toString());
    }

    @Test
    public void test16_18_7()
    {
        assertEquals( CYPHER+"CREATE (node {props})",
                      create(node( "node").values( param( "props" ) )).toString());
    }

    @Test
    public void test16_19_1()
    {
        assertEquals( CYPHER+"START n=node(4) DELETE n",
                      start( nodesById( "n", 4 ) ).delete( identifier( "n" ) ).toString());
    }

    @Test
    public void test16_19_2()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r]-() DELETE n,r",
                      start( nodesById( "n", 3 ) ).match( node( "n" ).both( ).as( "r" ).node() ).delete( identifier( "n" ), identifier( "r" ) ).toString() );
    }

    @Test
    public void test16_19_3()
    {
        assertEquals( CYPHER+"START andres=node(3) DELETE andres.age RETURN andres",
                      start( nodesById( "andres", 3 ) ).delete( identifier( "andres" ).property( "age" ) ).returns( identifier( "andres" ) ).toString());
    }

    @Test
    public void test16_20_1()
    {
        assertEquals( CYPHER+"START n=node(2) SET n.surname=\"Taylor\" RETURN n",
                      start( nodesById( "n", 2 ) ).set(property( identifier( "n" ).property( "surname" ), literal( "Taylor" ))).returns( identifier( "n" ) ).toString());
    }

    @Test
    public void test16_21_1()
    {
        assertEquals( CYPHER+"START left=node(1),right=node(3,4) RELATE (left)-[r:KNOWS]->(right) RETURN r",
                      start( nodesById( "left", 1 ), nodesById( "right", 3, 4 ) ).relate(node( "left" ).out( "KNOWS" ).as( "r" ).node( "right" )).returns( identifier( "r" ) ).toString());
    }

    @Test
    public void test16_21_2()
    {
        assertEquals( CYPHER+"START root=node(2) RELATE (root)-[:LOVES]-(someone) RETURN someone",
                      start( nodesById( "root", 2 ) ).relate( node( "root" ).both( "LOVES" ).node( "someone" ) ).returns( identifier( "someone" ) ).toString());
    }

    @Test
    public void test16_21_3()
    {
        assertEquals( CYPHER+"START root=node(2) RELATE (root)-[:X]-(leaf {name:\"D\"}) RETURN leaf",
                      start( nodesById( "root", 2 ) ).relate( node( "root" ).both( "X" ).node( identifier( "leaf" )).values( value( "name", "D" ) ) ).returns( identifier( "leaf" ) ).toString());
    }

    @Test
    public void test16_21_4()
    {
        assertEquals( CYPHER+"START root=node(2) RELATE (root)-[r:X {since:\"forever\"}]-() RETURN r",
                      start( nodesById( "root", 2 ) ).relate( node( "root" ).both( identifier("X")).values( value("since", "forever" ) ).as( "r" ).node() ).returns( identifier( "r" ) ).toString());
    }

    @Test
    public void test16_21_5()
    {
        assertEquals( CYPHER+"START root=node(2) RELATE (root)-[:FOO]->(x),(root)-[:BAR]->(x) RETURN x",
                      start( nodesById( "root", 2 ) ).relate( node( "root" ).out( "FOO" ).node( "x" ), node( "root" ).out( "BAR" ).node( "x" ) ).returns( identifier( "x" ) ) .toString());
    }

    @Test
    public void test16_22_1()
    {
        assertEquals( CYPHER+"START begin=node(2),end=node(1) MATCH p=(begin)-->(end) FOREACH(n in nodes(p): SET n.marked=true)",
                      start( nodesById( "begin", 2 ), nodesById( "end", 1 ) ).
                      match( path( "p", node( "begin" ).out( ).node("end" ))).
                      forEach( in( identifier( "n" ), nodes( identifier( "p" ) ) ).
                          set( property( identifier( "n" ).property( "marked" ), literal( true ) ) ) ).toString());
    }


    @Test
    public void test16_23_1_1()
    {
        assertEquals( CYPHER+"START a=node(3),b=node(1) MATCH p=(a)-[*1..3]->(b) WHERE all(x IN nodes(p) WHERE x.age>30) RETURN p",
                      start( nodesById( "a", 3 ), nodesById( "b", 1 ) ).
                          match( path( "p", node( "a" ).out().hops( 1, 3 ).node( "b" ) )).
                          where( all( "x", nodes( identifier( "p" ) ), identifier( "x" ).number( "age" ).gt( 30 ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test16_23_1_2()
    {
        assertEquals( CYPHER+"START a=node(2) WHERE any(x IN a.array WHERE x=\"one\") RETURN p",
                      start( nodesById( "a", 2 ) ).
                          where( any( "x", identifier( "a" ).property( "array" ), identifier( "x" ).eq( "one" ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test16_23_1_3()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH p=(n)-[*1..3]->(b) WHERE none(x IN nodes(p) WHERE x.age=25) RETURN p",
                      start( nodesById( "n", 3 ) ).
                          match( path( "p", node( "n" ).out( ).hops( 1,3 ).node( "b" ))).
                          where( none( "x", nodes( identifier( "p" ) ), identifier( "x" ).number( "age" ).eq( 25 ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test16_23_1_4()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH p=(n)-->(b) WHERE single(var IN nodes(p) WHERE var.eyes=\"blue\") RETURN p",
                      start( nodesById( "n", 3 ) ).
                          match( path( "p", node( "n" ).out().node( "b" ) )).
                          where( single( "var", nodes( identifier( "p" ) ), identifier( "var" ).string( "eyes" )
                              .eq( "blue" ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test16_23_2_1()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH p=(a)-->(b)-->(c) RETURN length(p)",
                      start( nodesById( "a", 3 ) ).
                          match( path( "p", node( "a" ).out().node( "b" ).out().node( "c" ) )).
                              returns( length( identifier( "p" ) ) ).
                              toString() );
    }

    @Test
    public void test16_23_2_2()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r]->() RETURN type(r)",
                      start( nodesById( "n", 3 ) ).
                          match( node( "n" ).out().as( "r" ).node() ).
                          returns( type( identifier( "r" ) ) ).
                          toString() );
    }

    @Test
    public void test16_23_2_3()
    {
        assertEquals( CYPHER+"START a=node(3,4,5) RETURN id(a)",
                      start( nodesById( "a", 3, 4, 5 ) ).returns( id( identifier( "a" ) ) ).toString() );
    }

    @Test
    public void test16_23_2_4()
    {
        assertEquals( CYPHER+"START a=node(3) RETURN coalesce(a.hairColour?,a.eyes?)",
                      start( nodesById( "a", 3 ) ).returns( coalesce( identifier( "a" ).property( "hairColour" ).optional(), identifier( "a" ).property( "eyes" ).optional() ) ).toString() );
    }

    @Test
    public void test16_23_2_5()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,head(a.array)",
                      start( nodesById( "a", 2 ) ).returns( identifier( "a" ).property( "array" ), head( identifier( "a" ).property( "array" ) ) ).toString() );
    }

    @Test
    public void test16_23_2_6()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,last(a.array)",
                      start( nodesById( "a", 2 ) ).returns( identifier( "a" ).property( "array" ), last( identifier( "a" ).property( "array" ) ) ).toString() );
    }

    @Test
    public void test16_23_3_1()
    {
        assertEquals( CYPHER+"START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN nodes(p)",
                      start( nodesById( "a", 3 ), nodesById( "c", 2 ) ).
                          match( path( "p", node( "a" ).out().node( "b" ).out().node( "c" ) )).
                              returns( nodes( identifier( "p" ) ) ).
                              toString() );
    }

    @Test
    public void test16_23_3_2()
    {
        assertEquals( CYPHER+"START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN relationships(p)",
                      start( nodesById( "a", 3 ), nodesById( "c", 2 ) ).
                          match( path( "p", node( "a" ).out().node( "b" ).out().node( "c" ) )).
                              returns( relationships( identifier( "p" ) ) ).
                              toString() );
    }

    @Test
    public void test16_23_3_3()
    {
        assertEquals( CYPHER+"START a=node(3),b=node(4),c=node(1) MATCH p=(a)-->(b)-->(c) RETURN extract(n IN nodes(p):n.age)",
                      start( nodesById( "a", 3 ), nodesById( "b", 4 ), nodesById( "c", 1 ) ).
                          match( path( "p", node( "a" ).out().node( "b" ).out().node( "c" ) )).
                              returns( extract( "n", nodes( identifier( "p" ) ), identifier( "n" ).number( "age" ) ) ).
                              toString() );
    }

    @Test
    public void test16_23_3_4()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,filter(x IN a.array:length(x)=3)",
                      start( nodesById( "a", 2 ) ).
                          returns( identifier( "a" ).property( "array" ), filter( "x", identifier( "a" ).property( "array" ), length( identifier( "x" ) )
                              .eq( 3 ) ) ).
                          toString() );
    }

    @Test
    public void test16_23_3_5()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,tail(a.array)",
                      start( nodesById( "a", 2 ) ).
                          returns( identifier( "a" ).property( "array" ), tail( identifier( "a" ).property( "array" ) ) ).
                          toString() );
    }

    @Test
    public void test16_23_3_6()
    {
        assertEquals( CYPHER+"START a=node(1) RETURN range(0,10),range(2,18,3)",
                      start( nodesById( "a", 1 ) ).
                          returns( range( 0, 10), range( 2,18,3) ).
                          toString() );
    }

    @Test
    public void test16_23_4_1()
    {
        assertEquals( CYPHER+"START a=node(3),c=node(2) RETURN a.age,c.age,abs(a.age-c.age)",
                      start( nodesById( "a", 3 ), nodesById( "c", 2 ) ).
                          returns( identifier( "a" ).property( "age" ), identifier( "c" ).property( "age" ), abs( identifier( "a" )
                                                                                                                      .number( "age" )
                                                                                                                      .subtract( identifier( "c" )
                                                                                                                                     .property( "age" ) ) ) ).
                          toString() );
    }

    @Test
    public void test16_23_4_2()
    {
        assertEquals( CYPHER+"START a=node(1) RETURN round(3.141592)",
                      start( nodesById( "a", 1 ) ).
                          returns( round( 3.141592) ).
                          toString() );
    }

    @Test
    public void test16_23_4_3()
    {
        assertEquals( CYPHER+"START a=node(1) RETURN sqrt(256)",
                      start( nodesById( "a", 1 ) ).
                          returns( sqrt( 256 )).
                          toString() );
    }


    @Test
    public void test16_23_4_4()
    {
        assertEquals( CYPHER+"START a=node(1) RETURN sign(-17),sign(0.1)",
                      start( nodesById( "a", 1 ) ).
                          returns( sign( -17), sign( 0.1) ).
                          toString() );
    }

    // Cookbook
    @Test
    public void test5_1_1()
    {
        // This test shows how to do partial queries. When the Query from toQuery() is passed into a new CypherQuery
        // it is cloned, so any modifications do not affect the original query

        Query query = start( lookup( "n", "node_auto_index", "name", "User1" ) ).
            match( node( "n" ).out( "hasRoleInGroup" ).node( "hyperEdge" ).out( "hasGroup" ).node( "group" ),
                   node( "hyperEdge" ).out( "hasRole" ).node( "role" ) ).toQuery();

        assertEquals( CYPHER + "START n=node:node_auto_index(name=\"User1\") MATCH (n)-[:hasRoleInGroup]->(hyperEdge)-[:hasGroup]->(group),(hyperEdge)-[:hasRole]->(role) WHERE group.name=\"Group2\" RETURN role.name",
                      CypherQuery.newQuery( query ).starts().
                          where( identifier( "group" ).string( "name" ).eq( "Group2" ) ).
                          returns( identifier( "role" ).string( "name" ) ).
                          toString() );

        assertEquals( CYPHER + "START n=node:node_auto_index(name=\"User1\") MATCH (n)-[:hasRoleInGroup]->(hyperEdge)-[:hasGroup]->(group),(hyperEdge)-[:hasRole]->(role) RETURN role.name,group.name ORDER BY role.name ASCENDING",
                      CypherQuery.newQuery( query ).starts().
                          returns( identifier( "role" ).property( "name" ), identifier( "group" ).property( "name" ) ).
                          orderBy( order( identifier( "role" ).string( "name" ), ASCENDING ) ).
                          toString() );
    }

    @Test
    public void test5_1_2()
    {
        Identifier u1 = identifier("u1");
        Identifier u2 = identifier("u2");
        Identifier hyperEdge1 = identifier("hyperEdge1");
        Identifier group = identifier("group");
        Identifier role = identifier("role");
        Identifier hyperEdge2 = identifier("hyperEdge2");
        assertEquals( CYPHER + "START u1=node:node_auto_index(name=\"User1\"),u2=node:node_auto_index(name=\"User2\") " +
                      "MATCH (u1)-[:hasRoleInGroup]->(hyperEdge1)-[:hasGroup]->(group)," +
                      "(hyperEdge1)-[:hasRole]->(role)," +
                      "(u2)-[:hasRoleInGroup]->(hyperEdge2)-[:hasGroup]->(group)," +
                      "(hyperEdge2)-[:hasRole]->(role) " +
                      "RETURN group.name,count(role) " +
                      "ORDER BY group.name ASCENDING",
                      start( lookup( u1, identifier( "node_auto_index" ), identifier( "name" ), literal( "User1" ) ), lookup( u2, identifier( "node_auto_index" ), identifier( "name" ), literal( "User2" ) ) ).
                              match( node( u1 )
                                         .out( "hasRoleInGroup" )
                                         .node( hyperEdge1 )
                                         .out( "hasGroup" )
                                         .node( group ),
                                     node( hyperEdge1 ).out( "hasRole" ).node( role ),
                                     node( u2 )
                                         .out( "hasRoleInGroup" )
                                         .node( hyperEdge2 )
                                         .out( "hasGroup" )
                                         .node( group ),
                                     node( hyperEdge2 ).out( "hasRole" ).node( role ) ).
                              returns( group.property( "name" ), count( role ) ).
                              orderBy( order( group.property( "name" ), ASCENDING ) )
                          .toString() );
    }

    @Test
    public void test5_1_3()
    {
        assertEquals( CYPHER+ "START u1=node:node_auto_index(name=\"User1\"),u2=node:node_auto_index(name=\"User2\") " +
                      "MATCH (u1)-[:hasRoleInGroup]->(hyperEdge1)-[:hasGroup]->(group)," +
                      "(hyperEdge1)-[:hasRole]->(role)," +
                      "(u2)-[:hasRoleInGroup]->(hyperEdge2)-[:hasGroup]->(group)," +
                      "(hyperEdge2)-[:hasRole]->(role) " +
                      "RETURN group.name,count(role) " +
                      "ORDER BY group.name ASCENDING",
                      start( lookup( "u1", "node_auto_index", "name", "User1" ), lookup( "u2", "node_auto_index", "name", "User2" ) ).
                      match( node( "u1" ).out( "hasRoleInGroup" )
                                 .node( "hyperEdge1" )
                                 .out( "hasGroup" )
                                 .node( "group" ),
                             node( "hyperEdge1" ).out( "hasRole" ).node( "role" ),
                             node( "u2" ).out( "hasRoleInGroup" )
                                 .node( "hyperEdge2" )
                                 .out( "hasGroup" )
                                 .node( "group" ),
                             node( "hyperEdge2" ).out( "hasRole" ).node( "role" ) ).
                      returns( identifier( "group" ).property( "name" ), count( identifier( "role" ) ) ).
                      orderBy( order( identifier( "group" ).property( "name" ), Order.ASCENDING ) ).toString()
                            );
    }

    @Test
    public void test5_2_1()
    {
        assertEquals( CYPHER+"START joe=node:node_auto_index(name=\"Joe\") MATCH (joe)-[:knows]->(friend)-[:knows]->(friend_of_friend),(joe)-[r?:knows]->(friend_of_friend) WHERE r is null RETURN friend_of_friend.name,count(*) ORDER BY count(*) DESCENDING,friend_of_friend.name",
                      start( lookup( "joe", "node_auto_index", "name", "Joe" ) ).
                          match( node( "joe" ).out( "knows" ).node( "friend" )
                                     .out( "knows" ).node( "friend_of_friend" ),
                                 node( "joe" ).out( "knows" ).as( "r" ).optional().node( "friend_of_friend" ) ).
                          where( isNull( identifier("r" )) ).
                          returns( identifier( "friend_of_friend" ).property( "name" ), count() ).
                          orderBy( order( count(), DESCENDING ), identifier( "friend_of_friend" ).property( "name" ) ).
                          toString() );
    }

    @Test
    public void test5_3_1()
    {
        assertEquals( CYPHER+"START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                      start( lookup( "place", "node_auto_index", "name", "CoffeShop1" ) ).
                          match( node( "place" ).in( "favorite" ).node( "person" ).out( "favorite" ).node( "stuff" ) ).
                          returns( identifier( "stuff" ).property( "name" ), count() ).
                          orderBy( order( count(), DESCENDING ), identifier( "stuff" ).property( "name" ) ).
                          toString() );

        assertEquals( CYPHER + "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)-[:tagged]->(tag)<-[:tagged]-(otherPlace) RETURN otherPlace.name,collect(tag.name) ORDER BY otherPlace.name DESCENDING",
                      start( lookup( "place", "node_auto_index", "name", "CoffeShop1" ) ).
                          match( node( "place" ).out( "tagged" ).node( "tag" ).in( "tagged" ).node( "otherPlace" ) ).
                          returns( identifier( "otherPlace" ).property( "name" ), collect( identifier( "tag" ).property( "name" ) ) ).
                          orderBy( order( identifier( "otherPlace" ).property( "name" ), DESCENDING ) ).
                          toString() );
    }

    @Test
    public void test5_4_1()
    {
        assertEquals( CYPHER+"START me=node:node_auto_index(name=\"Joe\") MATCH (me)-[:favorite]->(stuff)<-[:favorite]-(person) WHERE not((me)-[:friend]-(person)) RETURN person.name,count(stuff) ORDER BY count(stuff) DESCENDING",
                      start( lookup( "me", "node_auto_index", "name", "Joe" ) ).
                          match( node( "me" ).out( "favorite" ).node( "stuff" ).in( "favorite" ).node( "person" )).
                          where( not( node( "me").both( "friend" ).node( "person"))).
                          returns( identifier( "person" ).property( "name" ), count( identifier( "stuff" ) ) ).
                          orderBy( order( count( identifier( "stuff" ) ), DESCENDING ) ).
                          toString() );
    }
}
