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
import org.neo4j.cypherdsl.query.Query;

import static org.junit.Assert.*;
import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.CypherReferenceTest.CYPHER;
import static org.neo4j.cypherdsl.Order.*;

/**
 * Construct Cypher queries corresponding to the Cypher Cookbook in the manual
 */
public class CypherCookbookTest
{
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
    public void test5_3_2()
    {
        assertEquals( CYPHER+"START place=node:node_auto_index(name=\"CoffeeShop1\") MATCH (place)-[:tagged]->(tag)<-[:tagged]-(otherPlace) RETURN otherPlace.name,collect(tag.name) ORDER BY otherPlace.name DESCENDING",
                      start( lookup( "place", "node_auto_index", "name", "CoffeeShop1" ) ).
                      match( node("place" ).out( "tagged" ).node( "tag" ).in( "tagged" ).node( "otherPlace" ) ).
                      returns( identifier( "otherPlace" ).property( "name" ), collect( identifier( "tag" ).property( "name" ) ) ).
                      orderBy( order( identifier( "otherPlace" ).property( "name" ), DESCENDING ) ).toString());
    }

    @Test
    public void test5_4_1()
    {
        assertEquals( CYPHER + "START me=node:node_auto_index(name=\"Joe\") MATCH (me)-[:favorite]->(stuff)<-[:favorite]-(person) WHERE not((me)-[:friend]-(person)) RETURN person.name,count(stuff) ORDER BY count(stuff) DESCENDING",
                      start( lookup( "me", "node_auto_index", "name", "Joe" ) ).
                          match( node( "me" ).out( "favorite" ).node( "stuff" ).in( "favorite" ).node( "person" ) ).
                          where( not( node( "me" ).both( "friend" ).node( "person" ) ) ).
                          returns( identifier( "person" ).property( "name" ), count( identifier( "stuff" ) ) ).
                          orderBy( order( count( identifier( "stuff" ) ), DESCENDING ) ).
                          toString() );
    }

    @Test
    public void test5_5_1()
    {
        assertEquals( CYPHER+"START me=node(5),other=node(4,3) " +
                      "MATCH pGroups=(me)-[?:member_of_group]->(mg)<-[?:member_of_group]-(other),pMutualFriends=(me)-[?:knows]->(mf)<-[?:knows]-(other) " +
                      "RETURN other.name AS name," +
                      "count(DISTINCT pGroups) AS mutualGroups," +
                      "count(DISTINCT pMutualFriends) AS mutualFriends ORDER BY mutualFriends DESCENDING",
                      start( nodesById( "me", 5 ), nodesById( "other",4,3 ) ).
                      match( path( "pGroups", node( "me" ).out( "member_of_group" ).optional().node( "mg" ).in( "member_of_group" ).optional().node( "other" ) ),
                             path( "pMutualFriends", node( "me" ).out( "knows" ).optional().node( "mf" ).in( "knows" ).optional().node( "other" ) )).
                      returns( as( identifier( "other" ).property( "name" ) , "name" ),
                               as( count( distinct( identifier( "pGroups" )) ), "mutualGroups" ),
                               as( count( distinct( identifier( "pMutualFriends" )) ) , "mutualFriends" )).
                      orderBy( order( identifier( "mutualFriends" ), DESCENDING ) ).toString());
    }

    @Test
    public void test5_6_1()
    {
        assertEquals( CYPHER+"START me=node(9) " +
                      "MATCH (me)-[:favorite]->(myFavorites)-[:tagged]->(tag)<-[:tagged]-(theirFavorites)<-[:favorite]-(people) " +
                      "WHERE not(me=people) " +
                      "RETURN people.name AS name,count(*) AS similar_favs " +
                      "ORDER BY similar_favs DESCENDING",
                      start( nodesById( "me", 9 ) ).
                      match( node( "me" ).out( "favorite" ).node( "myFavorites" ).out( "tagged" ).node( "tag" ).in( "tagged" ).node( "theirFavorites" ).in( "favorite" ).node( "people" ) ).
                      where( not( identifier( "me" ).eq( identifier( "people" ) ) ) ).
                      returns( as( identifier( "people" ).property( "name" ) , "name" ), as( count(), "similar_favs" ) ).
                      orderBy( order( identifier( "similar_favs" ), DESCENDING ) ).toString());
    }

    @Test
    public void test5_7_1()
    {
        assertEquals( CYPHER+"START me=node:node_auto_index(name=\"Joe\") " +
                      "MATCH (me)-[r1]->(other)-[r2]->(me) " +
                      "WHERE type(r1)=type(r2) and type(r1)=~/FOLLOWS|LOVES/ " +
                      "RETURN other.name,type(r1)",
                      start( lookup( "me", "node_auto_index", "name", "Joe" ) ).
                      match( node( "me" ).out().as( "r1").node( "other" ).out().as( "r2").node( "me" ) ).
                      where( type( identifier( "r1" ) ).eq( type( identifier( "r2" ) ) ).and( type( identifier( "r1" ) ).regexp( "FOLLOWS|LOVES" ) ) ).
                      returns( identifier( "other" ).property( "name" ), type( identifier( "r1" ) ) ).toString());
    }

    @Test
    public void test5_8_1()
    {
        assertEquals( CYPHER+"START origin=node(1) " +
                      "MATCH (origin)-[r1:KNOWS|WORKSAT]-(c)-[r2:KNOWS|WORKSAT]-(candidate) " +
                      "WHERE type(r1)=type(r2) and not((origin)-[:KNOWS]-(candidate)) " +
                      "RETURN origin.name AS origin,candidate.name AS candidate,sum(round(r2.weight+" +
                      "coalesce(r2.activity?,0)*2)) AS boost " +
                      "ORDER BY boost DESCENDING "+
                      "LIMIT 10",
                      start( nodesById( "origin", 1 ) ).
                      match( node("origin" ).both("KNOWS","WORKSAT").as( "r1" ).node( "c" ).both("KNOWS","WORKSAT").as( "r2" ).node( "candidate" ) ).
                      where( type( identifier( "r1" ) ).eq( type( identifier( "r2" ) ) ).and( not( node( "origin" ).both("KNOWS").node( "candidate" ) ) ) ).
                      returns( as( identifier( "origin" ).property( "name" ) , "origin" ),
                               as( identifier( "candidate" ).property( "name" ) , "candidate" ),
                               as( sum( round( identifier( "r2" ).property( "weight" ).add( coalesce( identifier( "r2" ).property( "activity" ).optional(), literal( 0 ) ).times( 2 ) ) ) ), "boost" )).
                      orderBy( order( identifier( "boost" ), DESCENDING )).
                      limit( 10 ).toString());
    }

    @Test
    public void test5_9_1()
    {
        assertEquals( CYPHER+"START a=node(1) " +
                      "MATCH (a)--(b) "+
                      "WITH a,count(DISTINCT b) AS n "+
                      "MATCH (a)--()-[r]-()--(a) " +
                      "RETURN n,count(DISTINCT r) AS r",
                      start( nodesById( "a", 1 ) ).
                      match( node( "a" ).both().node( "b" ) ).
                      with( identifier( "a" ), as( count( distinct( identifier( "b" ) ) ) , "n" ) ).
                      match( node( "a" ).both().node(  ).both().as("r").node(  ).both().node( "a" ) ).
                      returns( identifier( "n" ), as( count( distinct( identifier( "r" ) ) ) , "r" )).toString() );
    }

    @Test
    public void test5_10_1()
    {
        assertEquals( CYPHER+"CREATE (center) " +
                      "FOREACH(x in range(1,10): CREATE (leaf),(center)-[:X]->(leaf)) "+
                      "RETURN id(center) AS id",
                      create( node( "center" ) ).
                      forEach( in( identifier( "x" ), range( 1,10 ) ).create( node( "leaf" ), node( "center" ).out( "X" ).node( "leaf" ) ) ).
                      returns( as( id( identifier( "center" ) ), "id" ) ).toString());
    }

    @Test
    public void test5_10_2()
    {
        assertEquals( CYPHER+"CREATE (center) " +
                      "FOREACH(x in range(1,10): CREATE (leaf {count:x}),(center)-[:X]->(leaf)) " +
                      "WITH center " +
                      "MATCH (large_leaf)<--(center)-->(small_leaf) " +
                      "WHERE large_leaf.count=small_leaf.count+1 " +
                      "CREATE (small_leaf)-[:X]->(large_leaf) " +
                      "WITH center,min(small_leaf.count) AS min,max(large_leaf.count) AS max " +
                      "MATCH (first_leaf)<--(center)-->(last_leaf) " +
                      "WHERE first_leaf.count=min and last_leaf.count=max " +
                      "CREATE (last_leaf)-[:X]->(first_leaf) " +
                      "RETURN id(center) AS id",
                      create( node( "center" ) ).
                      forEach( in( "x", range( 1,10 ) ).create( node( "leaf" ).values( value( "count", identifier( "x" ) ) ), node( "center" ).out("X").node( "leaf" ) ) ).
                      with( identifier( "center" ) ).
                      match( node( "large_leaf" ).in().node( "center" ).out().node(  "small_leaf"  ) ).
                      where( identifier( "large_leaf" ).property( "count" ).eq( identifier( "small_leaf" ).property( "count" ).add( 1 ) ) ).
                      create(node("small_leaf").out( "X" ).node( "large_leaf" )).
                      with(identifier( "center" ), as( min( identifier( "small_leaf" ).property( "count" ) ), "min" ), as( max( identifier( "large_leaf" ).property( "count" ) ) , "max" ) ).
                      match( node( "first_leaf" ).in(  ).node( "center" ).out(  ).node( "last_leaf" ) ).
                      where( identifier( "first_leaf" ).property( "count" ).eq( identifier( "min" )).and( identifier( "last_leaf" ).property( "count" ).eq( identifier( "max" ) ) ) ).
                      create( node( "last_leaf" ).out( "X" ).node( "first_leaf" ) ).
                      returns( as( id( identifier( "center" ) ), "id" ) ).toString());
    }

    @Test
    public void test5_10_3()
    {
        assertEquals( CYPHER+"CREATE (center) " +
                      "FOREACH(x in range(1,10): CREATE (leaf {count:x}),(center)-[:X]->(leaf)) " +
                      "WITH center " +
                      "MATCH (leaf1)<--(center)-->(leaf2) " +
                      "WHERE id(leaf1)<id(leaf2) " +
                      "CREATE (leaf1)-[:X]->(leaf2) " +
                      "WITH center " +
                      "MATCH (center)-[r]->() " +
                      "DELETE center,r",
                      create( node( "center" ) ).
                      forEach( in( "x", range( 1,10 ) ).create( node( "leaf" ).values( value( "count", identifier( "x" ) ) ), node( "center" ).out( "X" ).node( "leaf" ) ) ).
                      with( identifier( "center" ) ).
                      match( node( "leaf1" ).in(  ).node( "center" ).out(  ).node( "leaf2" ) ).
                      where( id( "leaf1" ).lt( id( "leaf2" ) ) ).
                      create( node( "leaf1" ).out( "X" ).node( "leaf2" ) ).
                      with( identifier( "center" ) ).
                      match( node( "center" ).out().as( "r").node(  ) ).
                      delete( identifier( "center" ), identifier( "r" ) ).toString());
    }

    @Test
    public void test5_10_4()
    {
        assertEquals( CYPHER+"CREATE (center) " +
                      "FOREACH(x in range(1,10): CREATE (leaf1),(leaf2),(center)-[:X]->(leaf1),(center)-[:X]->(leaf2),(leaf1)-[:X]->(leaf2)) " +
                      "RETURN id(center) AS id",
                      create( node( "center" ) ).
                      forEach( in( "x", range( 1,10 ) ).create( node( "leaf1" ), node( "leaf2" ), node( "center" ).out( "X" ).node( "leaf1" ), node( "center" ).out( "X" ).node( "leaf2" ), node( "leaf1" ).out( "X" ).node( "leaf2" ) ) ).
                      returns( as( id( identifier( "center" ) ) , "id" ) ).toString());
    }

    @Test
    public void test5_12_1()
    {
        assertEquals( CYPHER+"CREATE (root)-[:LINK]->(root) RETURN root",
                      create( node( "root" ).out( "LINK" ).node( "root" ) ).returns( identifier( "root" ) ).toString());
    }

    @Test
    public void test5_12_2()
    {
        assertEquals( CYPHER+"START root=node(4) " +
                      "MATCH (root)-[:LINK*0..]->(before),(after)-[:LINK*0..]->(root),(before)-[old:LINK]->(after) " +
                      "WHERE before.value?<25 and 25<after.value? " +
                      "CREATE (before)-[:LINK]->({value:25})-[:LINK]->(after) " +
                      "DELETE old",
                      start( nodesById( "root", 4 ) ).
                      match( node( "root" ).out( "LINK" ).hops( 0,null ).node( "before" ), node( "after" ).out( "LINK" ).hops( 0, null ).node( "root" ), node( "before" ).out( "LINK").as("old").node( "after" ) ).
                      where( identifier( "before" ).property( "value" ).optional().lt( 25 ).and( literal( 25 ).lt( identifier( "after" ).property( "value" ).optional() ) ) ).
                      create( node( "before" ).out( "LINK" ).node( ).values( value( "value", 25 ) ).out( "LINK" ).node( "after" ) ).
                      delete( identifier( "old" ) ).toString());
    }

    @Test
    public void test5_12_3()
    {
        assertEquals( CYPHER+"START root=node(4) " +
                      "MATCH (root)-[:LINK*0..]->(before),(before)-[delBefore:LINK]->(del)-[delAfter:LINK]->(after),(after)-[:LINK*0..]->(root) " +
                      "WHERE del.value=10 " +
                      "CREATE (before)-[:LINK]->(after) " +
                      "DELETE del,delBefore,delAfter",
                      start( nodesById( "root", 4 ) ).
                      match( node( "root" ).out( "LINK" ).hops( 0, null ).node( "before" ),
                             node( "before" ).out( "LINK").as("delBefore").node( "del" ).out( "LINK").as("delAfter").node( "after" ),
                             node( "after" ).out( "LINK" ).hops( 0, null ).node( "root" ) ).
                      where( identifier( "del" ).property( "value" ).eq( 10 ) ).
                      create(node( "before" ).out( "LINK" ).node( "after" )).
                      delete( identifier( "del" ), identifier( "delBefore" ), identifier( "delAfter" ) ).toString());
    }

    @Test
    public void test5_13_1()
    {
        assertEquals( CYPHER+"START root=node:node_auto_index(name=\"Root\") " +
                      "MATCH rootPath=(root)-[:`2010`]->()-[:`12`]->()-[:`31`]->(leaf),(leaf)-[:VALUE]->(event) " +
                      "RETURN event.name " +
                      "ORDER BY event.name ASCENDING",
                      start( lookup( "root", "node_auto_index", "name", "Root" ) ).
                      match( path( "rootPath", node( "root" ).out( "2010" ).node( ).out( "12" ).node( ).out( "31" ).node( "leaf" )), node( "leaf" ).out( "VALUE" ).node( "event" ) ).
                      returns( identifier( "event" ).property( "name" ) ).
                      orderBy( order( identifier( "event" ).property( "name" ), ASCENDING ) ).toString());
    }

    @Test
    public void test5_13_2()
    {
        assertEquals( CYPHER+"START root=node:node_auto_index(name=\"Root\") " +
                      "MATCH startPath=(root)-[:`2010`]->()-[:`12`]->()-[:`31`]->(startLeaf)," +
                      "endPath=(root)-[:`2011`]->()-[:`01`]->()-[:`03`]->(endLeaf)," +
                      "valuePath=(startLeaf)-[:NEXT*0..]->(middle)-[:NEXT*0..]->(endLeaf)," +
                      "values=(middle)-[:VALUE]->(event) " +
                      "RETURN event.name " +
                      "ORDER BY event.name ASCENDING",
                      start( lookup( "root", "node_auto_index", "name", "Root" ) ).
                      match( path( "startPath", node( "root" ).out( "2010" ).node( ).out( "12" ).node( ).out( "31" ).node( "startLeaf" ) ),
                             path( "endPath", node( "root" ).out( "2011" ).node( ).out( "01" ).node(  ).out( "03" ).node( "endLeaf" ) ),
                             path( "valuePath", node( "startLeaf" ).out( "NEXT" ).hops( 0, null ).node( "middle" ).out( "NEXT" ).hops( 0, null ).node( "endLeaf" ) ),
                             path( "values", node( "middle" ).out( "VALUE" ).node( "event" ) )).
                      returns( identifier( "event" ).property( "name" ) ).
                      orderBy( order( identifier( "event" ).property( "name" ), ASCENDING )).toString());
    }

    @Test
    public void test5_13_3()
    {
        assertEquals( CYPHER+"START root=node:node_auto_index(name=\"Root\") " +
                      "MATCH commonPath=(root)-[:`2011`]->()-[:`01`]->(commonRootEnd)," +
                      "startPath=(commonRootEnd)-[:`01`]->(startLeaf)," +
                      "endPath=(commonRootEnd)-[:`03`]->(endLeaf)," +
                      "valuePath=(startLeaf)-[:NEXT*0..]->(middle)-[:NEXT*0..]->(endLeaf)," +
                      "values=(middle)-[:VALUE]->(event) " +
                      "RETURN event.name " +
                      "ORDER BY event.name ASCENDING",
                      start( lookup( "root", "node_auto_index", "name", "Root" ) ).
                      match( path( "commonPath", node( "root" ).out( "2011" ).node( ).out( "01" ).node( "commonRootEnd" ) ),
                             path( "startPath", node( "commonRootEnd" ).out( "01" ).node( "startLeaf" ) ),
                             path( "endPath", node( "commonRootEnd" ).out( "03" ).node( "endLeaf" ) ),
                             path( "valuePath", node( "startLeaf" ).out( "NEXT" ).hops( 0, null ).node( "middle" ).out( "NEXT" ).hops( 0, null ).node( "endLeaf" ) ),
                             path( "values", node( "middle" ).out( "VALUE" ).node( "event" ) )).
                      returns( identifier( "event" ).property( "name" ) ).
                      orderBy( order( identifier( "event" ).property( "name" ), ASCENDING ) ).toString());
    }

    @Test
    public void test5_14_1()
    {
        assertEquals( CYPHER+"START me=node(*) " +
                      "MATCH (me)-[r1:ATE]->(food)<-[r2:ATE]-(you) " +
                      "WHERE has(me.name) and me.name=\"me\" " +
                      "WITH me,count(DISTINCT r1) AS H1,count(DISTINCT r2) AS H2,you " +
                      "MATCH (me)-[r1:ATE]->(food)<-[r2:ATE]-(you) " +
                      "RETURN sum(1-abs(r1.times/H1-r2.times/H2)*r1.times+r2.times/H1+H2) AS similarity",
                      start( allNodes( "me" ) ).
                      match( node( "me" ).out( "ATE").as("r1" ).node( "food" ).in( "ATE" ).as("r2").node( "you" ) ).
                      where( has( identifier( "me" ).property( "name" ) ).and( identifier( "me" ).property( "name" ).eq( "me" ) ) ).
                      with( identifier( "me" ), as( count( distinct( identifier( "r1" ) ) ), "H1" ), as( count( distinct( identifier( "r2" ) ) ), "H2" ),identifier( "you" )  ).
                      match( node( "me" ).out( "ATE").as("r1").node( "food" ).in( "ATE").as("r2").node( "you" ) ).
                      returns( as( sum( literal( 1 ).subtract( abs( identifier( "r1" ).property( "times" ).divideBy( identifier( "H1" ) ).subtract( identifier( "r2" ).property( "times" ).divideBy( identifier( "H2" )))).
                                         times( identifier( "r1" ).property( "times" ).add( identifier( "r2" ).property( "times" ) ) ).divideBy( identifier( "H1" ).add( identifier( "H2" ) ) )) ) , "similarity" ) ).toString());
    }
}
