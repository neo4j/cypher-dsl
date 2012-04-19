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
import org.neo4j.cypherdsl.query.Query;

import static org.junit.Assert.*;
import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.Order.*;

/**
 * Construct Cypher queries corresponding to the Cypher Reference manual
 */
public class CypherReferenceTest
{
    public static final String CYPHER="CYPHER 1.7 ";

    @Test
    public void test15_6_1()
    {
        assertEquals( CYPHER + "START n=node(1) RETURN n",
                      start( node( "n", 1 ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test15_6_2()
    {
        assertEquals( CYPHER+"START r=relationship(0) RETURN r",
                      start( relationship( "r", 0 ) ).returns( identifier( "r" ) ).toString() );
    }

    @Test
    public void test15_6_3()
    {
        assertEquals( CYPHER+"START n=node(1,2,3) RETURN n",
                      start( node( "n", 1, 2, 3 ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test15_6_4()
    {
        assertEquals( CYPHER+"START n=node(*) RETURN n",
                      start( allNodes( "n" ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test15_6_5()
    {
        assertEquals( CYPHER+"START n=node:nodes(name=\"A\") RETURN n",
                      start( lookup( "n", "nodes", "name", "A" ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test15_6_6()
    {
        assertEquals( CYPHER+"START r=relationship:rels(property=\"some_value\") RETURN r",
                      start( relationshipLookup( "r", "rels", "property", "some_value" ) ).
                          returns( identifier( "r" ) ).toString() );
    }

    @Test
    public void test15_6_7()
    {
        assertEquals( CYPHER+"START n=node:nodes(\"name:A\") RETURN n",
                      start( query( "n", "nodes", "name:A" ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test15_6_8()
    {
        assertEquals( CYPHER + "START a=node(1),b=node(2) RETURN a,b",
                      start( node( "a", 1 ), node( "b", 2 ) ).returns( identifier( "a" ), identifier( "b" ) )
                          .toString() );
    }

    @Test
    public void test15_7_2()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)--(x) RETURN x",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).to( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_7_3()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-->(x) RETURN x",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out().to( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_7_4()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r]->() RETURN r",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out().as( "r" ) ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_7_5()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[:BLOCKS]->(x) RETURN x",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out( "BLOCKS" ).to( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_7_6()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[:BLOCKS|KNOWS]->(x) RETURN x",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out( "BLOCKS","KNOWS" ).to( "x" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_7_7()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r:BLOCKS]->() RETURN r",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).as( "r" ).out( "BLOCKS" ) ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_7_8()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r:`TYPE WITH SPACE IN IT`]->() RETURN r",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).as( "r" ).out( "TYPE WITH SPACE IN IT" ) ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_7_9()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c) RETURN a,b,c",
                      start( node( "a", 3 ) ).
                          match( path().from( "a" ).out( "KNOWS" ).to( "b" ).
                              link().out( "KNOWS" ).to( "c" ) ).
                          returns( identifiers( "a", "b", "c" ) ).
                          toString() );
    }

    @Test
    public void test15_7_10()
    {
        assertEquals( CYPHER+"START a=node(3),x=node(2,4) MATCH (a)-[:KNOWS*1..3]->(x) RETURN a,x",
                      start( node( "a", 3 ), node( "x", 2, 4 ) ).
                          match( path().from( "a" ).out( "KNOWS" ).hops( 1, 3 ).to( "x" ) ).
                          returns( identifiers( "a", "x" ) ).
                          toString() );
    }

    @Test
    public void test15_7_11()
    {
        assertEquals( CYPHER+"START a=node(3),x=node(2,4) MATCH (a)-[r:KNOWS*1..3]->(x) RETURN r",
                      start( node( "a", 3 ), node( "x", 2, 4 ) ).
                          match( path().from( "a" ).as( "r" ).out( "KNOWS" ).hops( 1, 3 ).to( "x" ) ).
                          returns( identifiers( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_7_12()
    {
        assertEquals( CYPHER + "START a=node(3) MATCH p1=(a)-[:KNOWS*0..1]->(b),p2=(b)-[:BLOCKS*0..1]->(c) RETURN a,b,c,length(p1),length(p2)",
                      start( node( "a", 3 ) ).
                          match( path( "p1" ).from( "a" ).out( "KNOWS" ).hops( 0, 1 ).to( "b" ),
                                 path( "p2" ).from( "b" ).out( "BLOCKS" ).hops( 0, 1 ).to( "c" ) ).
                          returns( identifier( "a" ), identifier( "b" ), identifier( "c" ), length( identifier( "p1" ) ), length( identifier( "p2" ) ) ).
                          toString() );
    }

    @Test
    public void test15_7_13()
    {
        assertEquals( CYPHER+"START a=node(2) MATCH (a)-[?]->(x) RETURN a,x",
                      start( node( "a", 2 ) ).
                          match( path().from( "a" ).optional().out().to( "x" ) ).
                          returns( identifiers( "a", "x" ) ).
                          toString() );
    }

    @Test
    public void test15_7_14()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH (a)-[r?:LOVES]->() RETURN a,r",
                      start( node( "a", 3 ) ).
                          match( path().from( "a" ).as( "r" ).optional().out( "LOVES" ) ).
                          returns( identifier( "a" ), identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_7_15()
    {
        assertEquals( CYPHER+"START a=node(2) MATCH (a)-[?]->(x) RETURN x,x.name",
                      start( node( "a", 2 ) ).
                          match( path().from( "a" ).optional().out().to( "x" ) ).
                          returns( identifier( "x" ), identifier( "x" ).string( "name" ) ).
                          toString() );
    }

    @Test
    public void test15_7_16()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c),(a)-[:BLOCKS]-(d)-[:KNOWS]-(c) RETURN a,b,c,d",
                      start( node( "a", 3 ) ).
                          match( path().from( "a" ).out( "KNOWS" ).to( "b" )
                                     .link().out( "KNOWS" ).to( "c" ),
                                 path().from( "a" ).both( "BLOCKS" ).to( "d" ).link().both( "KNOWS" ).to( "c" ) ).
                          returns( identifiers( "a", "b", "c", "d" ) ).
                          toString() );
    }

    @Test
    public void test15_7_17()
    {
        assertEquals( CYPHER+"START d=node(1),e=node(2) MATCH p=shortestPath((d)-[*..15]->(e)) RETURN p",
                      start( node( "d", 1 ), node( "e", 2 ) ).
                          match( shortestPath( "p" ).from( "d" ).out().hops( null, 15 ).to( "e" ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_7_18()
    {
        assertEquals( CYPHER+"START d=node(1),e=node(2) MATCH p=allShortestPaths((d)-[*..15]->(e)) RETURN p",
                      start( node( "d", 1 ), node( "e", 2 ) ).
                          match( allShortestPaths( "p" ).from( "d" ).out().hops( null, 15 ).to( "e" ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_7_19()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH p=(a)-->(b) RETURN p",
                      start( node( "a", 3 ) ).
                          match( path( "p" ).from( "a" ).out().to( "b" ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_7_20()
    {
        assertEquals( CYPHER+"START a=node(3),b=node(2) MATCH (a)-[?:KNOWS]-(x)-[?:KNOWS]-(b) RETURN x",
                      start( node( "a", 3 ), node( "b", 2 ) ).
                          match( path().from( "a" )
                                     .both( "KNOWS" )
                                     .optional()
                                     .to( "x" )
                                     .link()
                                     .both( "KNOWS" )
                                     .optional()
                                     .to( "b" ) ).
                          returns( identifier( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_8_1()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE (n.age<30 and n.name=\"Tobias\") or not(n.name=\"Tobias\") RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier( "n" ).number( "age" ).lt( 30 ).and( identifier( "n" ).string( "name" ).eq( "Tobias" ) )
                                     .or( not( identifier( "n" ).string( "name" ).eq( "Tobias" ) ) ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_8_2()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.age<30 RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier( "n" ).number( "age" ).lt( 30 ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_8_3()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.name=~/Tob.*/ RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier( "n" ).string( "name" ).regexp( "Tob.*" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_8_4()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.name=~/Some\\/thing/ RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier( "n" ).string( "name" ).regexp( "Some/thing" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_8_5()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.name=~/(?i)ANDR.*/ RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier( "n" ).string( "name" ).regexp( "ANDR.*", false ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_8_6()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r]->() WHERE type(r)=~/K.*/ RETURN r",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out().as( "r" ) ).
                          where( regexp( type( identifier( "r" ) ), literal( "K.*" ) ) ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_8_7()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE has(n.belt) RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier( "n" ).property( "belt" ).has()).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_8_8()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.belt?=\"white\" RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier( "n").string( "belt" ).trueIfMissing().eq( "white" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_8_9()
    {
        assertEquals( CYPHER+"START n=node(3,1) WHERE n.belt!=\"white\" RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( identifier("n").string( "belt").falseIfMissing().eq( "white" ) ).
                          returns( identifier( "n" ) ).
                          toString() );
    }


    @Test
    public void test15_8_10()
    {
        assertEquals( CYPHER+"START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is null RETURN b",
                      start( node( "a", 1 ), node( "b", 3, 2 ) ).
                          match( path().from( "a" ).as( "r" ).optional().in().to( "b" ) ).
                          where( identifier("r" ).isNull() ).
                          returns( identifier( "b" ) ).
                          toString() );
    }

    @Test
    public void test15_8_10_2()
    {
        assertEquals( CYPHER+"START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is not null RETURN b",
                      start( node( "a", 1 ), node( "b", 3, 2 ) ).
                          match( path().from( "a" ).as( "r" ).optional().in().to( "b" ) ).
                          where( identifier("r" ).isNotNull() ).
                          returns( identifier( "b" ) ).
                          toString() );
    }

    @Test
    public void test15_8_11()
    {
        assertEquals( CYPHER+"START a=node(1),b=node(3,2) WHERE (a)<--(b) RETURN b",
                      start( node( "a", 1 ), node( "b", 3, 2 ) ).
                          where( relationship().from("a").in().to("b") ).
                          returns( identifier( "b" ) ).
                          toString() );
    }

    @Test
    public void test15_8_12()
    {
        assertEquals( CYPHER+"START a=node(3,1,2) WHERE a.name IN [\"Peter\",\"Tobias\"] RETURN a",
                      start( node( "a", 3,1,2 ) ).where( in( identifier( "a").string( "name"), literals( "Peter", "Tobias" ) )).returns( identifier( "a" ) ).toString() );
    }

    @Test
    public void test15_9_1()
    {
        assertEquals( CYPHER+"START n=node(2) RETURN n",
                      start( node( "n", 2 ) ).returns( identifier( "n" ) ).toString() );
    }

    @Test
    public void test15_9_2()
    {
        assertEquals( CYPHER+"START n=node(1) MATCH (n)-[r:KNOWS]->(c) RETURN r",
                      start( node( "n", 1 ) ).
                          match( path().from( "n" ).as( "r" ).out( "KNOWS" ).to( "c" ) ).
                          returns( identifier( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_9_3()
    {
        assertEquals( CYPHER+"START n=node(1) RETURN n.name",
                      start( node( "n", 1 ) ).returns( identifier( "n" ).property( "name" ) ).toString() );
    }

    @Test
    public void test15_9_4()
    {
        assertEquals( CYPHER+"START `This isn't a common identifier`=node(1) RETURN `This isn't a common identifier`.`<<!!__??>>`",
                      start( node( identifier("This isn't a common identifier"), 1 ) ).
                          returns( identifier( "This isn't a common identifier").property( "<<!!__??>>" ) ).
                          toString() );
    }

    @Test
    public void test15_9_5()
    {
        assertEquals( CYPHER + "START a=node(1) RETURN a.age AS SomethingTotallyDifferent",
                      start( node( "a", 1 ) ).
                          returns( exp( identifier( "a" ).property( "age" ) ).as( "SomethingTotallyDifferent" ) ).
                          toString() );
    }

    @Test
    public void test15_9_6()
    {
        assertEquals( CYPHER+"START n=node(1,2) RETURN n.age?",
                      start( node( "n", 1, 2 ) ).
                          returns( identifier( "n" ).number( "age" ).optional() ).
                          toString() );
    }

    @Test
    public void test15_9_7()
    {
        assertEquals( CYPHER+"START a=node(1) MATCH (a)-->(b) RETURN DISTINCT b",
                      start( node( "a", 1 ) ).
                          match( path().from( "a" ).out().to( "b" ) ).
                          returns( exp( identifier( "b" ) ).distinct() ).
                          toString() );
    }

    @Test
    public void test15_10_3()
    {
        assertEquals( CYPHER+"START n=node(2) MATCH (n)-->(x) RETURN n,count(*)",
                      start( node( "n", 2 ) ).
                          match( path().from( "n" ).out( ).to( "x" ) ).
                          returns( identifier( "n" ), count() ).
                          toString() );
    }

    @Test
    public void test15_10_4()
    {
        assertEquals( CYPHER+"START n=node(2) MATCH (n)-[r]->() RETURN type(r),count(*)",
                      start( node( "n", 2 ) ).
                          match( path().from( "n" ).as( "r" ).out() ).
                          returns( type( identifier("r") ), count() ).
                          toString() );
    }

    @Test
    public void test15_10_5()
    {
        assertEquals( CYPHER+"START n=node(2) MATCH (n)-->(x) RETURN count(x)",
                      start( node( "n", 2 ) ).
                          match( path().from( "n" ).out().to( "x" ) ).
                          returns( count( identifier( "x" ) ) ).
                          toString() );
    }

    @Test
    public void test15_10_6()
    {
        assertEquals( CYPHER+"START n=node(2,3,4,1) RETURN count(n.property?)",
                      start( node( "n", 2, 3, 4, 1 ) ).
                          returns( count( identifier( "n" ).property( "property" ).optional() ) ).
                          toString() );
    }

    @Test
    public void test15_10_7()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN sum(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( sum( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test15_10_8()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN avg(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( avg( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test15_10_9()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN max(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( max( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test15_10_10()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN min(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( min( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test15_10_11()
    {
        assertEquals( CYPHER+"START n=node(2,3,4) RETURN collect(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( collect( identifier("n").property( "property" ) ) ).toString() );
    }

    @Test
    public void test15_10_12()
    {
        assertEquals( CYPHER+"START a=node(2) MATCH (a)-->(b) RETURN count(DISTINCT b.eyes)",
                      start( node( "a", 2 ) ).
                          match( path().from( "a" ).out().to( "b" ) ).
                          returns( count( exp( identifier( "b" ).property( "eyes" ) ).distinct() ) ).
                          toString() );
    }

    @Test
    public void test15_11_1()
    {
        assertEquals( CYPHER+"START n=node(3,1,2) RETURN n ORDER BY n.name",
                      start( node( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "name" ) ).
                          toString() );
    }

    @Test
    public void test15_11_2()
    {
        assertEquals( CYPHER+"START n=node(3,1,2) RETURN n ORDER BY n.age,n.name",
                      start( node( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "age" ), identifier( "n" ).property( "name" ) ).
                          toString() );
    }

    @Test
    public void test15_11_3()
    {
        assertEquals(CYPHER+ "START n=node(3,1,2) RETURN n ORDER BY n.name DESCENDING",
                      start( node( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( order( identifier( "n" ).property( "name" ), DESCENDING ) ).
                          toString() );
    }

    @Test
    public void test15_11_4()
    {
        assertEquals( CYPHER+"START n=node(3,1,2) RETURN n.length?,n ORDER BY n.length?",
                      start( node( "n", 3, 1, 2 ) ).
                          returns( identifier( "n" ).property( "length" ).optional(), identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "length" ).optional() ).
                          toString() );
    }

    @Test
    public void test15_12_1()
    {
        assertEquals( CYPHER+"START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 3",
                      start( node( "n", 3, 4, 5, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n" ).property( "name" ) ).
                          skip( 3 ).
                          toString() );
    }

    @Test
    public void test15_12_2()
    {
        assertEquals( CYPHER+"START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 1 LIMIT 2",
                      start( node( "n", 3, 4, 5, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          orderBy( identifier( "n").property( "name" ) ).
                          skip( 1 ).
                          limit( 2 ).
                          toString() );
    }

    @Test
    public void test15_13_1()
    {
        assertEquals( CYPHER+"START n=node(3,4,5,1,2) RETURN n LIMIT 3",
                      start( node( "n", 3, 4, 5, 1, 2 ) ).
                          returns( identifier( "n" ) ).
                          limit( 3 ).
                          toString() );
    }

    @Test
    public void test15_14_1_1()
    {
        assertEquals( CYPHER+"START a=node(3),b=node(1) MATCH p=(a)-[*1..3]->(b) WHERE all(x IN nodes(p) WHERE x.age>30) RETURN p",
                      start( node( "a", 3 ), node( "b", 1 ) ).
                          match( path( "p" ).from( "a" ).out().hops( 1, 3 ).to( "b" ) ).
                          where( all( "x", nodes( identifier( "p" ) ), identifier( "x" ).number( "age" ).gt( 30 ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_14_1_2()
    {
        assertEquals( CYPHER+"START a=node(2) WHERE any(x IN a.array WHERE x=\"one\") RETURN p",
                      start( node( "a", 2 ) ).
                          where( any( "x", identifier( "a" ).property( "array" ), string( "x" ).eq( "one" ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_14_1_3()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH p=(n)-[*1..3]->(b) WHERE none(x IN nodes(p) WHERE x.age=25) RETURN p",
                      start( node( "n", 3 ) ).
                          match( path( "p" ).from( "n" ).out( ).hops( 1,3 ).to( "b" )).
                          where( none( "x", nodes( identifier( "p" ) ), identifier( "x" ).number( "age" ).eq( 25 ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_14_1_4()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH p=(n)-->(b) WHERE single(var IN nodes(p) WHERE var.eyes=\"blue\") RETURN p",
                      start( node( "n", 3 ) ).
                          match( path( "p" ).from( "n" ).out().to( "b" ) ).
                          where( single( "var", nodes( identifier( "p" ) ), identifier( "var" ).string( "eyes" )
                              .eq( "blue" ) ) ).
                          returns( identifier( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_14_2_1()
    {
        assertEquals( CYPHER+"START a=node(3) MATCH p=(a)-->(b)-->(c) RETURN length(p)",
                      start( node( "a", 3 ) ).
                          match( path( "p" ).from( "a" ).out().to( "b" ).link().out().to( "c" ) ).
                          returns( length( identifier( "p" ) ) ).
                          toString() );
    }

    @Test
    public void test15_14_2_2()
    {
        assertEquals( CYPHER+"START n=node(3) MATCH (n)-[r]->() RETURN type(r)",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).as( "r" ).out() ).
                          returns( type( identifier( "r" ) ) ).
                          toString() );
    }

    @Test
    public void test15_14_2_3()
    {
        assertEquals( CYPHER+"START a=node(3,4,5) RETURN id(a)",
                      start( node( "a", 3, 4, 5 ) ).returns( id( identifier( "a" ) ) ).toString() );
    }

    @Test
    public void test15_14_2_4()
    {
        assertEquals( CYPHER+"START a=node(3) RETURN coalesce(a.hairColour?,a.eyes?)",
                      start( node( "a", 3) ).returns( coalesce( identifier( "a" ).string( "hairColour" ).optional(), identifier( "a" ).string( "eyes" ).optional() ) ).toString() );
    }

    @Test
    public void test15_14_2_5()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,head(a.array)",
                      start( node( "a", 2) ).returns( identifier( "a" ).property( "array" ), head( identifier( "a" ).property( "array" ) ) ).toString() );
    }

    @Test
    public void test15_14_2_6()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,last(a.array)",
                      start( node( "a", 2) ).returns( identifier( "a" ).property( "array" ), last( identifier( "a" ).property( "array" ) ) ).toString() );
    }

    @Test
    public void test15_14_3_1()
    {
        assertEquals( CYPHER+"START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN nodes(p)",
                      start( node( "a", 3 ), node( "c", 2 ) ).
                          match( path( "p" ).from( "a" ).out().to( "b" ).link().out().to( "c" ) ).
                          returns( nodes( identifier( "p" ) ) ).
                          toString() );
    }

    @Test
    public void test15_14_3_2()
    {
        assertEquals( CYPHER+"START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN relationships(p)",
                      start( node( "a", 3 ), node( "c", 2 ) ).
                          match( path( "p" ).from( "a" ).out().to( "b" ).link().out().to( "c" ) ).
                          returns( relationships( identifier( "p" ) ) ).
                          toString() );
    }

    @Test
    public void test15_14_3_3()
    {
        assertEquals( CYPHER+"START a=node(3),b=node(4),c=node(1) MATCH p=(a)-->(b)-->(c) RETURN extract(n IN nodes(p):n.age)",
                      start( node( "a", 3 ), node( "b",4), node( "c", 1 ) ).
                          match( path( "p" ).from( "a" ).out().to( "b" ).link().out().to( "c" ) ).
                          returns( extract( "n", nodes( identifier( "p" ) ), identifier( "n" ).number( "age" ) ) ).
                          toString() );
    }

    @Test
    public void test15_14_3_4()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,filter(x IN a.array:length(x)=3)",
                      start( node( "a", 2 ) ).
                          returns( identifier( "a" ).property( "array" ), filter( "x", identifier( "a" ).property( "array" ), length( identifier( "x" ) )
                              .eq( 3 ) ) ).
                          toString() );
    }

    @Test
    public void test15_14_3_5()
    {
        assertEquals( CYPHER+"START a=node(2) RETURN a.array,tail(a.array)",
                      start( node( "a", 2 ) ).
                          returns( identifier( "a" ).property( "array" ), tail( identifier( "a" ).property( "array" ) ) ).
                          toString() );
    }

    @Test
    public void test15_14_4_1()
    {
        assertEquals( CYPHER+"START a=node(3),c=node(2) RETURN a.age,c.age,abs(a.age-c.age)",
                      start( node( "a", 3 ),node( "c",2 ) ).
                          returns( identifier( "a" ).property( "age" ), identifier( "c" ).property( "age" ), abs( identifier( "a" )
                                                                                                                      .number( "age" )
                                                                                                                      .subtract( identifier( "c" )
                                                                                                                                     .property( "age" ) ) ) ).
                          toString() );
    }

    // Cookbook
    @Test
    public void test5_1_1()
    {
        // This test shows how to do partial queries. When the Query from toQuery() is passed into a new CypherQuery
        // it is cloned, so any modifications do not affect the original query

        Query query = start( lookup( "n", "node_auto_index", "name", "User1" ) ).
            match( path().from( "n" ).out( "hasRoleInGroup" ).to( "hyperEdge" ).link().out( "hasGroup" ).to( "group" ),
                   path().from( "hyperEdge" ).out( "hasRole" ).to( "role" ) ).toQuery();

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
                              match( path().from( u1 )
                                         .out( "hasRoleInGroup" )
                                         .to( hyperEdge1 )
                                         .link()
                                         .out( "hasGroup" )
                                         .to( group ),
                                     path().from( hyperEdge1 ).out( "hasRole" ).to( role ),
                                     path().from( u2 )
                                         .out( "hasRoleInGroup" )
                                         .to( hyperEdge2 )
                                         .link()
                                         .out( "hasGroup" )
                                         .to( group ),
                                     path().from( hyperEdge2 ).out( "hasRole" ).to( role ) ).
                              returns( group.property( "name" ), count( role ) ).
                              orderBy( order( group.property( "name" ), ASCENDING ) )
                          .toString() );
    }

    @Test
    public void test5_2_1()
    {
        assertEquals( CYPHER+"START joe=node:node_auto_index(name=\"Joe\") MATCH (joe)-[:knows]->(friend)-[:knows]->(friend_of_friend),(joe)-[r?:knows]->(friend_of_friend) WHERE r is null RETURN friend_of_friend.name,count(*) ORDER BY count(*) DESCENDING,friend_of_friend.name",
                      start( lookup( "joe", "node_auto_index", "name", "Joe" ) ).
                          match( path().from( "joe" ).out( "knows" ).to( "friend" )
                                     .link().out( "knows" ).to( "friend_of_friend" ),
                                 path().from( "joe" ).as( "r" ).out( "knows" ).optional().to( "friend_of_friend" ) ).
                          where( identifier("r" ).isNull() ).
                          returns( identifier( "friend_of_friend" ).property( "name" ), count() ).
                          orderBy( order( count(), DESCENDING ), identifier( "friend_of_friend" ).property( "name" ) ).
                          toString() );
    }

    @Test
    public void test16_12_3()
    {
        assertEquals( CYPHER+"START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                      start( lookup( "place", "node_auto_index", "name", "CoffeShop1" ) ).
                          match( path().from( "place" ).in( "favorite" ).to( "person" )
                                     .link().out( "favorite" ).to( "stuff" ) ).
                          returns( identifier( "stuff" ).property( "name" ), count() ).
                          orderBy( order( count(), DESCENDING ), identifier( "stuff" ).property( "name" ) ).
                          toString() );

        assertEquals( CYPHER + "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)-[:tagged]->(tag)<-[:tagged]-(otherPlace) RETURN otherPlace.name,collect(tag.name) ORDER BY otherPlace.name DESCENDING",
                      start( lookup( "place", "node_auto_index", "name", "CoffeShop1" ) ).
                          match( path().from( "place" ).out( "tagged" ).to( "tag" )
                                     .link().in( "tagged" ).to( "otherPlace" ) ).
                          returns( identifier( "otherPlace" ).property( "name" ), collect( identifier( "tag" ).property( "name" ) ) )
                          .
                              orderBy( order( identifier( "otherPlace" ).property( "name" ), DESCENDING ) )
                          .
                              toString() );
    }

    @Test
    public void test5_3_1()
    {
        assertEquals( CYPHER+"START me=node:node_auto_index(name=\"Joe\") MATCH (me)-[:favorite]->(stuff)<-[:favorite]-(person),(me)-[r?:friend]-(person) WHERE r is null RETURN person.name,count(stuff) ORDER BY count(stuff) DESCENDING",
                      start( lookup( "me", "node_auto_index", "name", "Joe" ) ).
                          match( path().from( "me" ).out( "favorite" ).to( "stuff" )
                                     .link().in( "favorite" ).to( "person" ),
                                 path().from( "me" ).as( "r" ).optional().both( "friend" ).to( "person" ) ).
                          where( identifier( "r" ).isNull() ).
                          returns( identifier( "person" ).property( "name" ), count( identifier( "stuff" ) ) ).
                          orderBy( order( count( identifier( "stuff" ) ), DESCENDING ) ).
                          toString() );
    }
}
