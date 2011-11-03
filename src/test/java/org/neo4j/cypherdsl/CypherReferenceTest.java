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
import org.neo4j.cypherdsl.query.MatchExpression;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.cypherdsl.query.ReturnExpression;

import static org.junit.Assert.*;
import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.MatchExpression.*;
import static org.neo4j.cypherdsl.query.MatchExpression.Direction.*;
import static org.neo4j.cypherdsl.query.OrderByExpression.Order.*;
import static org.neo4j.cypherdsl.query.OrderByExpression.*;
import static org.neo4j.cypherdsl.query.ReturnExpression.*;
import static org.neo4j.cypherdsl.query.StartExpression.*;
import static org.neo4j.cypherdsl.query.WhereExpression.*;

/**
 * Construct Cypher queries corresponding to the Cypher Reference manual
 */
public class CypherReferenceTest
{
    @Test
    public void test15_3_1()
    {
        assertEquals( "START n=node(1) RETURN n",
                      start( node( "n", 1 ) ).returns( nodes( "n" ) ).toString() );
    }

    @Test
    public void test15_3_2()
    {
        assertEquals( "START r=relationship(0) RETURN r",
                      start( relationship( "r", 0 ) ).returns( relationships( "r" ) ).toString() );
    }

    @Test
    public void test15_3_3()
    {
        assertEquals( "START n=node(1,2,3) RETURN n",
                      start( node( "n", 1, 2, 3 ) ).returns( nodes( "n" ) ).toString() );
    }

    @Test
    public void test15_3_4()
    {
        assertEquals( "START n=node:nodes(name=\"A\") RETURN n",
                      start( lookup( "n", "nodes", "name", "A" ) ).returns( nodes( "n" ) ).toString() );
    }

    @Test
    public void test15_3_5()
    {
        assertEquals( "START r=relationship:rels(property=\"some_value\") RETURN r",
                      start( relationshipLookup( "r", "rels", "property", "some_value" ) ).
                          returns( nodes( "r" ) ).toString() );
    }

    @Test
    public void test15_3_6()
    {
        assertEquals( "START n=node:nodes(\"name:A\") RETURN n",
                      start( query( "n", "nodes", "name:A" ) ).returns( nodes( "n" ) ).toString() );
    }

    @Test
    public void test15_3_7()
    {
        assertEquals( "START a=node(1),b=node(2) RETURN a,b",
                      start( node( "a", 1 ), node( "b", 2 ) ).returns( nodes( "a", "b" ) ).toString() );
    }

    @Test
    public void test15_4_1()
    {
        assertEquals( "START n=node(3) MATCH (n)--(x) RETURN x",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).to( "x" ) ).
                          returns( nodes( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_4_2()
    {
        assertEquals( "START n=node(3) MATCH (n)-->(x) RETURN x",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out().to( "x" ) ).
                          returns( nodes( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_4_3()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r]->(x) RETURN r",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out().as( "r" ).to( "x" ) ).
                          returns( nodes( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_4_4()
    {
        assertEquals( "START n=node(3) MATCH (n)-[:BLOCKS]->(x) RETURN x",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).out( "BLOCKS" ).to( "x" ) ).
                          returns( nodes( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_4_5()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r:BLOCKS]->(x) RETURN r",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).as( "r" ).out( "BLOCKS" ).to( "x" ) ).
                          returns( nodes( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_4_6()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r:`TYPE WITH SPACE IN IT`]->(x) RETURN r",
                      start( node( "n", 3 ) ).
                          match( path().from( "n" ).as( "r" ).out( "`TYPE WITH SPACE IN IT`" ).to( "x" ) ).
                          returns( nodes( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_4_7()
    {
        assertEquals( "START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c) RETURN a,b,c",
                      start( node( "a", 3 ) ).
                          match( path().from( "a" ).out( "KNOWS" ).to( "b" ).
                              link().out( "KNOWS" ).to( "c" ) ).
                          returns( nodes( "a", "b", "c" ) ).
                          toString() );
    }

    @Test
    public void test15_4_8()
    {
        assertEquals( "START a=node(3),x=node(2,4) MATCH (a)-[:KNOWS*1..3]->(x) RETURN a,x",
                      start( node( "a", 3 ), node( "x", 2, 4 ) ).
                          match( path().from( "a" ).out( "KNOWS" ).hops( 1, 3 ).to( "x" ) ).
                          returns( nodes( "a", "x" ) ).
                          toString() );
    }

    @Test
    public void test15_4_9()
    {
        assertEquals( "START a=node(3) MATCH p1=(a)-[:KNOWS*0..1]->(b),p2=(b)-[:KNOWS*0..1]->(c) RETURN a,b,c,length(p1),length(p2)",
                      start( node( "a", 3 ) ).
                          match( path( "p1" ).from( "a" ).out( "KNOWS" ).hops( 0, 1 ).to( "b" ),
                                 path( "p2" ).from( "b" ).out( "KNOWS" ).hops( 0, 1 ).to( "c" ) ).
                          returns( nodes( "a", "b", "c" ), length( "p1" ), length( "p2" ) ).
                          toString() );
    }

    @Test
    public void test15_4_10()
    {
        assertEquals( "START a=node(2) MATCH (a)-[?]->(b) RETURN a,x",
                      start( node( "a", 2 ) ).
                          match( path().from( "a" ).optional().out().to( "b" ) ).
                          returns( nodes( "a", "x" ) ).
                          toString() );
    }

    @Test
    public void test15_4_11()
    {
        assertEquals( "START a=node(3) MATCH (a)-[r?:LOVES]->() RETURN a,r",
                      start( node( "a", 3 ) ).
                          match( path().from( "a" ).as( "r" ).optional().out( "LOVES" ) ).
                          returns( nodes( "a" ), relationships( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_4_12()
    {
        assertEquals( "START a=node(2) MATCH (a)-[?]->(x) RETURN x,x.name",
                      start( node( "a", 2 ) ).
                          match( path().from( "a" ).optional().out().to( "x" ) ).
                          returns( nodes( "x" ), properties( "x.name" ) ).
                          toString() );
    }

    @Test
    public void test15_4_13()
    {
        assertEquals( "START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c),(a)-[:BLOCKS]-(d)-[:KNOWS]-(c) RETURN a,b,c,d",
                      start( node( "a", 3 ) ).
                          match( path().from( "a" ).out( "KNOWS" ).to( "b" )
                                     .link().out( "KNOWS" ).to( "c" ),
                                 path().from( "a" ).both( "BLOCKS" ).to( "d" ).link().rel( "KNOWS" ).to( "c" ) ).
                          returns( nodes( "a", "b", "c", "d" ) ).
                          toString() );
    }

    @Test
    public void test15_4_14()
    {
        assertEquals( "START d=node(1),e=node(2) MATCH p=shortestPath((d)-[*..15]->(e)) RETURN p",
                      start( node( "d", 1 ), node( "e", 2 ) ).
                          match( shortestPath( "p" ).from( "d" ).out().hops( null, 15 ).to( "e" ) ).
                          returns( paths( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_4_15()
    {
        assertEquals( "START a=node(3) MATCH p=(a)-->(b) RETURN p",
                      start( node( "a", 3 ) ).
                          match( path( "p" ).from( "a" ).out().to( "b" ) ).
                          returns( paths( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_4_16()
    {
        assertEquals( "START r=relationship(0) MATCH (a)-[r]-(b) RETURN a,b",
                      start( relationship( "r", 0 ) ).
                          match( path().from( "a" ).as( "r" ).to( "b" ) ).
                          returns( nodes( "a", "b" ) ).
                          toString() );
    }

    @Test
    public void test15_5_1()
    {
        assertEquals( "START n=node(3,1) WHERE (n.age<30 and n.name=\"Tobias\") or not(n.name=\"Tobias\") RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( lt( "n.age", 30 ).and( eq( "n.name", "Tobias" ) )
                                     .or( not( eq( "n.name", "Tobias" ) ) ) ).
                          returns( nodes( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_5_2()
    {
        assertEquals( "START n=node(3,1) WHERE n.age<30 RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( lt( "n.age", 30 ) ).
                          returns( nodes( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_5_3()
    {
        assertEquals( "START n=node(3,1) WHERE n.name=~/Tob.*/ RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( regexp( "n.name", "Tob.*" ) ).
                          returns( nodes( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_5_4()
    {
        assertEquals( "START n=node(3,1) WHERE n.belt?=\"white\" RETURN n",
                      start( node( "n", 3, 1 ) ).
                          where( eq( "n.belt", "white" ).optional() ).
                          returns( nodes( "n" ) ).
                          toString() );
    }

    @Test
    public void test15_5_5()
    {
        assertEquals( "START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is null RETURN b",
                      start( node( "a", 1 ), node( "b", 3, 2 ) ).
                          match( path().from( "a" ).as( "r" ).optional().in().to( "b" ) ).
                          where( isNull( "r" ) ).
                          returns( nodes( "b" ) ).
                          toString() );
    }

    @Test
    public void test15_5_5_2()
    {
        assertEquals( "START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is not null RETURN b",
                      start( node( "a", 1 ), node( "b", 3, 2 ) ).
                          match( path().from( "a" ).as( "r" ).optional().in().to( "b" ).optional() ).
                          where( isNotNull( "r" ) ).
                          returns( nodes( "b" ) ).
                          toString() );
    }

    @Test
    public void test15_6_1()
    {
        assertEquals( "START n=node(2) RETURN n",
                      start( node( "n", 2 ) ).returns( nodes( "n" ) ).toString() );
    }

    @Test
    public void test15_6_2()
    {
        assertEquals( "START n=node(1) MATCH (n)-[r:KNOWS]->(c) RETURN r",
                      start( node( "n", 1 ) ).
                          match( path().from( "n" ).as( "r" ).out( "KNOWS" ).to( "c" ) ).
                          returns( relationships( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_6_3()
    {
        assertEquals( "START n=node(1) RETURN n.name",
                      start( node( "n", 1 ) ).returns( properties( "n.name" ) ).toString() );
    }

    @Test
    public void test15_6_4()
    {
        assertEquals( "START `This isn't a common identifier`=node(1) RETURN `This isn't a common identifier`.`<<!!__??>>`",
                      start( node( "`This isn't a common identifier`", 1 ) ).
                          returns( properties( "`This isn't a common identifier`.`<<!!__??>>`" ) ).
                          toString() );
    }

    @Test
    public void test15_6_5()
    {
        assertEquals( "START n=node(1,2) RETURN n.age?",
                      start( node( "n", 1, 2 ) ).
                          returns( properties( "n.age" ).optional() ).
                          toString() );
    }

    @Test
    public void test15_6_6()
    {
        assertEquals( "START a=node(1) MATCH (a)-->(b) RETURN distinct b",
                      start( node( "a", 1 ) ).
                          match( path().from( "a").out( ).to( "b" ) ).
                          returns( nodes( "b" ).distinct() ).
                          toString() );
    }

    @Test
    public void test15_7_2()
    {
        assertEquals( "START n=node(2) MATCH (n)-->(x) RETURN n,count(*)",
                      start( node( "n", 2 ) ).
                          match( path().from( "n" ).out( ).to( "x" ) ).
                          returns( nodes( "n" ), count() ).
                          toString() );
    }

    @Test
    public void test15_7_3()
    {
        assertEquals( "START n=node(2) MATCH (n)-[r]->() RETURN type(r),count(*)",
                      start( node( "n", 2 ) ).
                          match( path().from("n").as( "r" ).out( )).
                          returns( ReturnExpression.type( "r" ), count() ).
                          toString() );
    }

    @Test
    public void test15_7_4()
    {
        assertEquals( "START n=node(2) MATCH (n)-->(x) RETURN count(x)",
                      start( node( "n", 2 ) ).
                          match( path().from( "n").out( ).to( "x" ) ).
                          returns( count( "x" ) ).
                          toString() );
    }

    @Test
    public void test15_7_5()
    {
        assertEquals( "START n=node(2,3,4,1) RETURN count(n.property?)",
                      start( node( "n", 2, 3, 4, 1 ) ).
                          returns( count( "n.property" ).optional() ).
                          toString() );
    }

    @Test
    public void test15_7_6()
    {
        assertEquals( "START n=node(2,3,4) RETURN sum(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( sum( "n.property" ) ).toString() );
    }

    @Test
    public void test15_7_7()
    {
        assertEquals( "START n=node(2,3,4) RETURN avg(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( avg( "n.property" ) ).toString() );
    }

    @Test
    public void test15_7_8()
    {
        assertEquals( "START n=node(2,3,4) RETURN max(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( max( "n.property" ) ).toString() );
    }

    @Test
    public void test15_7_9()
    {
        assertEquals( "START n=node(2,3,4) RETURN min(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( min( "n.property" ) ).toString() );
    }

    @Test
    public void test15_7_10()
    {
        assertEquals( "START n=node(2,3,4) RETURN collect(n.property)",
                      start( node( "n", 2, 3, 4 ) ).returns( collect( "n.property" ) ).toString() );
    }

    @Test
    public void test15_7_11()
    {
        assertEquals( "START a=node(2) MATCH (a)-->(b) RETURN count(distinct b.eyes)",
                      start( node( "a", 2 ) ).
                          match( path().from( "a" ).out( ).to( "b" )).
                          returns( count( "b.eyes" ).distinct() ).
                          toString() );
    }

    @Test
    public void test15_8_1()
    {
        assertEquals( "START n=node(3,1,2) RETURN n ORDER BY n.name",
                      start( node( "n", 3, 1, 2 ) ).
                          returns( nodes( "n" ) ).
                          orderBy( property( "n.name" ) ).
                          toString() );
    }

    @Test
    public void test15_8_2()
    {
        assertEquals( "START n=node(3,1,2) RETURN n ORDER BY n.age,n.name",
                      start( node( "n", 3, 1, 2 ) ).
                          returns( nodes( "n" ) ).
                          orderBy( property( "n.age" ), property( "n.name" ) ).
                          toString() );
    }

    @Test
    public void test15_8_3()
    {
        assertEquals( "START n=node(3,1,2) RETURN n.length?,n ORDER BY n.length?",
                      start( node( "n", 3, 1, 2 ) ).
                          returns( properties( "n.length" ).optional(), nodes( "n" ) ).
                          orderBy( property( "n.length" ).optional() ).
                          toString() );
    }

    @Test
    public void test15_9_1()
    {
        assertEquals( "START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 3",
                      start( node( "n", 3, 4, 5, 1, 2 ) ).
                          returns( nodes( "n" ) ).
                          orderBy( property( "n.name" ) ).
                          skip( 3 ).
                          toString() );
    }

    @Test
    public void test15_9_2()
    {
        assertEquals( "START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 1 LIMIT 2",
                      start( node( "n", 3, 4, 5, 1, 2 ) ).
                          returns( nodes( "n" ) ).
                          orderBy( property( "n.name" ) ).
                          skip( 1 ).
                          limit( 2 ).
                          toString() );
    }

    @Test
    public void test15_10_1()
    {
        assertEquals( "START n=node(3,4,5,1,2) RETURN n LIMIT 3",
                      start( node( "n", 3, 4, 5, 1, 2 ) ).
                          returns( nodes( "n" ) ).
                          limit( 3 ).
                          toString() );
    }

    @Test
    public void test15_11_2()
    {
        assertEquals( "START a=node(3),b=node(1) MATCH p=(a)-[*1..3]->(b) WHERE all(x in nodes(p):x.age>30) RETURN p",
                      start( node( "a", 3 ), node( "b", 1 ) ).
                          match( path( "p" ).from( "a" ).out( ).hops( 1,3 ).to( "b" )).
                          where( all( "x", "nodes(p)", gt( "x.age", 30 ) ) ).
                          returns( paths( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_3()
    {
        assertEquals( "START a=node(3) MATCH p=(a)-[*1..3]->(b) WHERE any(x in nodes(p):x.eyes=\"blue\") RETURN p",
                      start( node( "a", 3 ) ).
                          match( path( "p" ).from( "a" ).out( ).hops( 1, 3 ).to( "b" )).
                          where( any( "x", "nodes(p)", eq( "x.eyes", "blue" ) ) ).
                          returns( paths( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_4()
    {
        assertEquals( "START n=node(3) MATCH p=(n)-[*1..3]->(b) WHERE none(x in nodes(p):x.age=25) RETURN p",
                      start( node( "n", 3 ) ).
                          match( path( "p" ).from( "n" ).out( ).hops( 1,3 ).to( "b" )).
                          where( none( "x", "nodes(p)", eq( "x.age", 25 ) ) ).
                          returns( paths( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_5()
    {
        assertEquals( "START n=node(3) MATCH p=(n)-->(b) WHERE single(var in nodes(p):var.eyes=\"blue\") RETURN p",
                      start( node( "n", 3 ) ).
                          match( path( "p" ).from( "n" ).out( ).to( "b" )).
                          where( single( "var", "nodes(p)", eq( "var.eyes", "blue" ) ) ).
                          returns( paths( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_7()
    {
        assertEquals( "START a=node(3) MATCH p=(a)-->(b)-->(c) RETURN length(p)",
                      start( node( "a", 3 ) ).
                          match( path( "p" ).from( "a" ).out( ).to( "b" ).link().out( ).to( "c" )).
                          returns( length( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_8()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r]->() RETURN type(r)",
                      start( node( "n", 3 ) ).
                          match( path( ).from( "n" ).as( "r" ).out( )).
                          returns( ReturnExpression.type( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_11_9()
    {
        assertEquals( "START a=node(3,4,5) RETURN id(a)",
                      start( node( "a", 3, 4, 5 ) ).returns( id( "a" ) ).toString() );
    }

    @Test
    public void test15_11_11()
    {
        assertEquals( "START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN nodes(p)",
                      start( node( "a", 3 ), node( "c", 2 ) ).
                          match( path( "p" ).from( "a" ).out( ).to( "b" ).link().out(  ).to( "c" )).
                          returns( nodesOf( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_12()
    {
        assertEquals( "START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN relationships(p)",
                      start( node( "a", 3 ), node( "c", 2 ) ).
                          match( path( "p" ).from( "a" ).out( ).to( "b" ).link().out( ).to( "c" )).
                          returns( relationshipsOf( "p" ) ).
                          toString() );
    }

    // Cookbook
    @Test
    public void test15_12_1()
    {
        // This test shows how to do partial queries. When the Query from toQuery() is passed into a new CypherQuery
        // it is cloned, so any modifications do not affect the original query

        Query query = start( lookup( "n", "node_auto_index", "name", "User1" ) ).
            match( path().from( "n" ).out( "hasRoleInGroup" ).to( "hyperEdge" ).link().out("hasGroup" ).to( "group" ),
                   path().from("hyperEdge").out( "hasRole" ).to( "role" )).toQuery();

        assertEquals( "START n=node:node_auto_index(name=\"User1\") MATCH (n)-[:hasRoleInGroup]->(hyperEdge)-[:hasGroup]->(group),(hyperEdge)-[:hasRole]->(role) WHERE group.name=\"Group2\" RETURN role.name",
                      CypherQuery.newQuery( query ).starts().
                          where( eq( "group.name", "Group2" ) ).
                          returns( properties( "role.name" ) ).
                          toString() );

        assertEquals( "START n=node:node_auto_index(name=\"User1\") MATCH (n)-[:hasRoleInGroup]->(hyperEdge)-[:hasGroup]->(group),(hyperEdge)-[:hasRole]->(role) RETURN role.name,group.name ORDER BY role.name ASCENDING",
                      CypherQuery.newQuery( query ).starts().
                          returns( properties( "role.name", "group.name" ) ).
                          orderBy( property( "role.name", ASCENDING ) ).
                          toString() );
    }

    @Test
    public void test15_12_2()
    {
        assertEquals( "START joe=node:node_auto_index(name=\"Joe\") MATCH (joe)-[:knows]->(friend)-[:knows]->(friend_of_friend),(joe)-[r?:knows]->(friend_of_friend) WHERE r is null RETURN friend_of_friend.name,count(*) ORDER BY count(*) DESCENDING,friend_of_friend.name",
                      start( lookup( "joe", "node_auto_index", "name", "Joe" ) ).
                          match( path().from( "joe" ).out( "knows" ).to( "friend" )
                                     .link().out( "knows" ).to( "friend_of_friend" ),
                                 path().from( "joe").as( "r" ).out( "knows" ).optional().to( "friend_of_friend" )).
                          where( isNull( "r" ) ).
                          returns( properties( "friend_of_friend.name" ), count() ).
                          orderBy( property( "count(*)", DESCENDING ), property( "friend_of_friend.name" ) ).
                          toString() );
    }

    @Test
    public void test15_12_3()
    {
        assertEquals( "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                      start( lookup( "place", "node_auto_index", "name", "CoffeShop1" ) ).
                          match( path().from( "place" ).in( "favorite" ).to( "person" )
                                     .link().out( "favorite" ).to( "stuff" )).
                          returns( properties( "stuff.name" ), count() ).
                          orderBy( property( "count(*)", DESCENDING ), property( "stuff.name" ) ).
                          toString() );

        assertEquals( "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)-[:tagged]->(tag)<-[:tagged]-(otherPlace) RETURN otherPlace.name,collect(tag.name) ORDER BY otherPlace.name DESCENDING",
                      start( lookup( "place", "node_auto_index", "name", "CoffeShop1" ) ).
                          match( path().from( "place" ).out( "tagged" ).to( "tag" )
                                     .link().in( "tagged" ).to( "otherPlace" )).
                          returns( properties( "otherPlace.name" ), collect( "tag.name" ) ).
                          orderBy( property( "otherPlace.name", DESCENDING ) ).
                          toString() );
    }

    @Test
    public void test15_12_4()
    {
        assertEquals( "START me=node:node_auto_index(name=\"Joe\") MATCH (me)-[:favorite]->(stuff)<-[:favorite]-(person),(me)-[r?:friend]-(person) WHERE r is null RETURN person.name,count(stuff) ORDER BY count(stuff) DESCENDING",
                      start( lookup( "me", "node_auto_index", "name", "Joe" ) ).
                          match( path().from( "me" ).out( "favorite" ).to( "stuff" )
                                     .link().in( "favorite" ).to( "person" ),
                                 path().from( "me" ).as( "r" ).optional().both( "friend" ).to( "person" ) ).
                          where( isNull( "r" ) ).
                          returns( properties( "person.name" ), count( "stuff" ) ).
                          orderBy( property( "count(stuff)", DESCENDING ) ).
                          toString() );
    }
}
