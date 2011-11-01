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
import org.neo4j.cypherdsl.query.MatchExpression;
import org.neo4j.cypherdsl.query.OrderByExpression;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.cypherdsl.query.ReturnExpression;
import org.neo4j.cypherdsl.query.WhereExpression;

import static org.junit.Assert.*;
import static org.neo4j.cypherdsl.CypherQuery.newQuery;
import static org.neo4j.cypherdsl.OrderBy.Order.*;
import static org.neo4j.cypherdsl.query.MatchExpression.*;
import static org.neo4j.cypherdsl.query.MatchExpression.Direction.*;
import static org.neo4j.cypherdsl.query.ReturnExpression.*;
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
                          match( path( "n", OUTGOING, "x" ) ).
                          returnNodes( "x" ).
                          toString() );
    }

    @Test
    public void test15_4_3()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r]->(x) RETURN r",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", OUTGOING, "r", null, "x" ) ).
                          returnNodes( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_4()
    {
        assertEquals( "START n=node(3) MATCH (n)-[:BLOCKS]->(x) RETURN x",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", OUTGOING, null, "BLOCKS", "x" )).
                          returnNodes( "x" ).
                          toString() );
    }

    @Test
    public void test15_4_5()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r:BLOCKS]->(x) RETURN r",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", OUTGOING, "r", "BLOCKS", "x" )).
                          returnNodes( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_6()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r:`TYPE WITH SPACE IN IT`]->(x) RETURN r",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", OUTGOING, "r", "`TYPE WITH SPACE IN IT`", "x" )).
                          returnNodes( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_7()
    {
        assertEquals( "START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c) RETURN a,b,c",
                      newQuery().
                          nodes( "a", 3 ).
                          match( path( "a", OUTGOING, null, "KNOWS", "b" ).
                              path( OUTGOING, null, "KNOWS", "c" ) ).
                          returnNodes( "a", "b", "c" ).
                          toString() );
    }

    @Test
    public void test15_4_8()
    {
        assertEquals( "START a=node(3),x=node(2,4) MATCH (a)-[:KNOWS*1..3]->(x) RETURN a,x",
                      newQuery().
                          nodes( "a", 3 ).nodes( "x", 2, 4 ).
                          match( path( "a", OUTGOING, null, "KNOWS", "x" ).hops( 1, 3 ) ).
                          returnNodes( "a", "x" ).
                          toString() );
    }

    @Test
    public void test15_4_9()
    {
        assertEquals( "START a=node(3) MATCH p1=(a)-[:KNOWS*0..1]->(b),p2=(b)-[:KNOWS*0..1]->(c) RETURN a,b,c,length(p1),length(p2)",
                      newQuery().
                          nodes( "a", 3 ).
                          match( MatchExpression.named( "p1", path( "a", OUTGOING, null, "KNOWS", "b" ).hops( 0, 1 ) ) ).
                          match( MatchExpression.named( "p2", path( "b", OUTGOING, null, "KNOWS", "c" ).hops( 0, 1 ) ) ).
                          returnNodes( "a", "b", "c" ).returnExpr( length( "p1" ) ).returnExpr( length( "p2" ) ).
                          toString() );
    }

    @Test
    public void test15_4_10()
    {
        assertEquals( "START a=node(2) MATCH (a)-[?]->(b) RETURN a,x",
                      newQuery().
                          nodes( "a", 2 ).
                          match( path( "a", OUTGOING, "b" ).optional() ).
                          returnNodes( "a", "x" ).
                          toString() );
    }

    @Test
    public void test15_4_11()
    {
        assertEquals( "START a=node(3) MATCH (a)-[r?:LOVES]->() RETURN a,r",
                      newQuery().
                          nodes( "a", 3 ).
                          match( path( "a", OUTGOING, "r", "LOVES", "" ).optional() ).
                          returnNodes( "a" ).returnRelationships( "r" ).
                          toString() );
    }

    @Test
    public void test15_4_12()
    {
        assertEquals( "START a=node(2) MATCH (a)-[?]->(x) RETURN x,x.name",
                      newQuery().
                          nodes( "a", 2 ).
                          match( path( "a", OUTGOING, "x" ).optional() ).
                          returnNodes( "x" ).returnProperties( "x.name" ).
                          toString() );
    }

    @Test
    public void test15_4_13()
    {
        assertEquals( "START a=node(3) MATCH (a)-[:KNOWS]->(b)-[:KNOWS]->(c),(a)-[:BLOCKS]-(d)-[:KNOWS]-(c) RETURN a,b,c,d",
                      newQuery().
                          nodes( "a", 3 ).
                          match( path( "a", OUTGOING, null, "KNOWS", "b" )
                                     .path( OUTGOING, null, "KNOWS", "c" )).
                          match( path( "a", ANY, null, "BLOCKS", "d" ).path( "c" ).relationship( "KNOWS" ) ).
                          returnNodes( "a", "b", "c", "d" ).
                          toString() );
    }

    @Test
    public void test15_4_14()
    {
        assertEquals( "START d=node(1),e=node(2) MATCH p=shortestPath((d)-[*..15]->(e)) RETURN p",
                      newQuery().
                          nodes( "d", 1 ).nodes( "e", 2 ).
                          match( named( "p", shortestPath( "d", OUTGOING, "e" ).hops( null, 15 ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_4_15()
    {
        assertEquals( "START a=node(3) MATCH p=(a)-->(b) RETURN p",
                      newQuery().
                          nodes( "a", 3 ).
                          match( named( "p", path( "a", OUTGOING, "b" ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_4_16()
    {
        assertEquals( "START r=relationship(0) MATCH (a)-[r]-(b) RETURN a,b",
                      newQuery().
                          relationships( "r", 0 ).
                          match( path( "a", "b" ).name( "r" ) ).
                          returnNodes( "a", "b" ).
                          toString() );
    }

    @Test
    public void test15_5_1()
    {
        assertEquals( "START n=node(3,1) WHERE (n.age<30 and n.name=\"Tobias\") or not(n.name=\"Tobias\") RETURN n",
                      newQuery().
                          nodes( "n", 3, 1 ).
                          where( lt( "n.age", 30 ).and( eq( "n.name", "Tobias" ) )
                                     .or( not( eq( "n.name", "Tobias" ) ) ) ).
                          returnNodes( "n" ).
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
                          where( regexp( "n.name", "Tob.*" ) ).
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
                          nodes( "a", 1 ).nodes( "b", 3, 2 ).
                          match( path( "a", INCOMING, "r", null, "b" ).optional() ).
                          where( isNull( "r" ) ).
                          returnNodes( "b" ).
                          toString() );
    }

    @Test
    public void test15_5_5_2()
    {
        assertEquals( "START a=node(1),b=node(3,2) MATCH (a)<-[r?]-(b) WHERE r is not null RETURN b",
                      newQuery().
                          nodes( "a", 1 ).nodes( "b", 3, 2 ).
                          match( path( "a", INCOMING, "r", null, "b" ).optional() ).
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
                          match( MatchExpression.path( "n", OUTGOING, "r", "KNOWS", "c" )).
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
                          nodes( "n", 1, 2 ).
                          returnProperties( true, "n.age" ).
                          toString() );
    }

    @Test
    public void test15_6_6()
    {
        assertEquals( "START a=node(1) MATCH (a)-->(b) RETURN distinct b",
                      newQuery().
                          nodes( "a", 1 ).
                          match( path( "a", OUTGOING, "b" ) ).
                          returnNodes( true, "b" ).
                          toString() );
    }

    @Test
    public void test15_7_2()
    {
        assertEquals( "START n=node(2) MATCH (n)-->(x) RETURN n,count(*)",
                      newQuery().
                          nodes( "n", 2 ).
                          match( path( "n", OUTGOING, "x" ) ).
                          returnNodes( "n" ).count().
                          toString() );
    }

    @Test
    public void test15_7_3()
    {
        assertEquals( "START n=node(2) MATCH (n)-[r]->() RETURN type(r),count(*)",
                      newQuery().
                          nodes( "n", 2 ).
                          match( path( "n", OUTGOING, "r", null, "" ) ).
                          returnExpr( ReturnExpression.type( "r" ) ).count().
                          toString() );
    }

    @Test
    public void test15_7_4()
    {
        assertEquals( "START n=node(2) MATCH (n)-->(x) RETURN count(x)",
                      newQuery().
                          nodes( "n", 2 ).
                          match( path( "n", OUTGOING, "x" )).
                          count( "x" ).
                          toString() );
    }

    @Test
    public void test15_7_5()
    {
        assertEquals( "START n=node(2,3,4,1) RETURN count(n.property?)",
                      newQuery().
                          nodes( "n", 2, 3, 4, 1 ).
                          returnExpr( count( "n.property" ).optional() ).
                          toString() );
    }

    @Test
    public void test15_7_6()
    {
        assertEquals( "START n=node(2,3,4) RETURN sum(n.property)",
                      newQuery().
                          nodes( "n", 2, 3, 4 ).
                          sum( "n.property" ).
                          toString() );
    }

    @Test
    public void test15_7_7()
    {
        assertEquals( "START n=node(2,3,4) RETURN avg(n.property)",
                      newQuery().
                          nodes( "n", 2, 3, 4 ).
                          avg( "n.property" ).
                          toString() );
    }

    @Test
    public void test15_7_8()
    {
        assertEquals( "START n=node(2,3,4) RETURN max(n.property)",
                      newQuery().
                          nodes( "n", 2, 3, 4 ).
                          max( "n.property" ).
                          toString() );
    }

    @Test
    public void test15_7_9()
    {
        assertEquals( "START n=node(2,3,4) RETURN min(n.property)",
                      newQuery().
                          nodes( "n", 2, 3, 4 ).
                          min( "n.property" ).
                          toString() );
    }

    @Test
    public void test15_7_10()
    {
        assertEquals( "START n=node(2,3,4) RETURN collect(n.property)",
                      newQuery().
                          nodes( "n", 2, 3, 4 ).
                          collect( "n.property" ).
                          toString() );
    }

    @Test
    public void test15_7_11()
    {
        assertEquals( "START a=node(2) MATCH (a)-->(b) RETURN count(distinct b.eyes)",
                      newQuery().
                          nodes( "a", 2 ).
                          match( path( "a", OUTGOING, "b" ) ).
                          returnExpr( count( "b.eyes" ).distinct() ).
                          toString() );
    }

    @Test
    public void test15_8_1()
    {
        assertEquals( "START n=node(3,1,2) RETURN n ORDER BY n.name",
                      newQuery().
                          nodes( "n", 3, 1, 2 ).
                          returnNodes( "n" ).
                          orderBy( "n.name" ).
                          toString() );
    }

    @Test
    public void test15_8_2()
    {
        assertEquals( "START n=node(3,1,2) RETURN n ORDER BY n.age,n.name",
                      newQuery().
                          nodes( "n", 3, 1, 2 ).
                          returnNodes( "n" ).
                          orderBy( "n.age" ).orderBy( "n.name" ).
                          toString() );
    }

    @Test
    public void test15_8_3()
    {
        assertEquals( "START n=node(3,1,2) RETURN n.length?,n ORDER BY n.length?",
                      newQuery().
                          nodes( "n", 3, 1, 2 ).
                          returnExpr( properties( "n.length" ).optional() ).returnNodes( "n" ).
                          orderBy( OrderByExpression.orderBy( "n.length" ).optional() ).
                          toString() );
    }

    @Test
    public void test15_9_1()
    {
        assertEquals( "START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 3",
                      newQuery().
                          nodes( "n", 3, 4, 5, 1, 2 ).
                          returnNodes( "n" ).
                          orderBy( "n.name" ).
                          skip( 3 ).
                          toString() );
    }

    @Test
    public void test15_9_2()
    {
        assertEquals( "START n=node(3,4,5,1,2) RETURN n ORDER BY n.name SKIP 1 LIMIT 2",
                      newQuery().
                          nodes( "n", 3, 4, 5, 1, 2 ).
                          returnNodes( "n" ).
                          orderBy( "n.name" ).
                          skip( 1 ).
                          limit( 2 ).
                          toString() );
    }

    @Test
    public void test15_10_1()
    {
        assertEquals( "START n=node(3,4,5,1,2) RETURN n LIMIT 3",
                      newQuery().
                          nodes( "n", 3, 4, 5, 1, 2 ).
                          returnNodes( "n" ).
                          limit( 3 ).
                          toString() );
    }

    @Test
    public void test15_11_2()
    {
        assertEquals( "START a=node(3),b=node(1) MATCH p=(a)-[*1..3]->(b) WHERE all(x in nodes(p):x.age>30) RETURN p",
                      newQuery().
                          nodes( "a", 3 ).nodes( "b", 1 ).
                          match( named( "p", path( "a", OUTGOING, "b" ).hops( 1, 3 ) ) ).
                          where( all( "x", "nodes(p)", gt( "x.age", 30 ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_11_3()
    {
        assertEquals( "START a=node(3) MATCH p=(a)-[*1..3]->(b) WHERE any(x in nodes(p):x.eyes=\"blue\") RETURN p",
                      newQuery().
                          nodes( "a", 3 ).
                          match( named( "p", path( "a", OUTGOING, "b" ).hops( 1, 3 ) ) ).
                          where( any( "x", "nodes(p)", eq( "x.eyes", "blue" ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_11_4()
    {
        assertEquals( "START n=node(3) MATCH p=(n)-[*1..3]->(b) WHERE none(x in nodes(p):x.age=25) RETURN p",
                      newQuery().
                          nodes( "n", 3 ).
                          match( named( "p", path( "n", OUTGOING, "b" ).hops( 1, 3 ) ) ).
                          where( none( "x", "nodes(p)", eq( "x.age", 25 ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_11_5()
    {
        assertEquals( "START n=node(3) MATCH p=(n)-->(b) WHERE single(var in nodes(p):var.eyes=\"blue\") RETURN p",
                      newQuery().
                          nodes( "n", 3 ).
                          match( named( "p", path( "n", OUTGOING, "b" )) ).
                          where( single( "var", "nodes(p)", eq( "var.eyes", "blue" ) ) ).
                          returnPaths( "p" ).
                          toString() );
    }

    @Test
    public void test15_11_7()
    {
        assertEquals( "START a=node(3) MATCH p=(a)-->(b)-->(c) RETURN length(p)",
                      newQuery().
                          nodes( "a", 3 ).
                          match( named( "p", path( "a", OUTGOING, "b" ).path( OUTGOING, "c" ))).
                          returnExpr( length( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_8()
    {
        assertEquals( "START n=node(3) MATCH (n)-[r]->() RETURN type(r)",
                      newQuery().
                          nodes( "n", 3 ).
                          match( path( "n", OUTGOING, "r", null, "" )).
                          returnExpr( ReturnExpression.type( "r" ) ).
                          toString() );
    }

    @Test
    public void test15_11_9()
    {
        assertEquals( "START a=node(3,4,5) RETURN id(a)",
                      newQuery().
                          nodes( "a", 3, 4, 5 ).
                          returnExpr( ReturnExpression.id( "a" ) ).
                          toString() );
    }

    @Test
    public void test15_11_11()
    {
        assertEquals( "START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN nodes(p)",
                      newQuery().
                          nodes( "a", 3 ).nodes( "c", 2 ).
                          match( named( "p", path( "a", OUTGOING, "b" ).path( OUTGOING, "c" ) ) ).
                          returnExpr( ReturnExpression.nodes( "p" ) ).
                          toString() );
    }

    @Test
    public void test15_11_12()
    {
        assertEquals( "START a=node(3),c=node(2) MATCH p=(a)-->(b)-->(c) RETURN relationships(p)",
                      newQuery().
                          nodes( "a", 3 ).nodes( "c", 2 ).
                          match( named( "p", path( "a", OUTGOING, "b" ).path( OUTGOING, "c" ) ) ).
                          returnExpr( ReturnExpression.relationships( "p" ) ).
                          toString() );
    }

    // Cookbook
    @Test
    public void test15_12_1()
    {
        // This test shows how to do partial queries. When the Query from toQuery() is passed into a new CypherQuery
        // it is cloned, so any modifications do not affect the original query

        Query query = newQuery().nodesLookup( "n", "node_auto_index", "name", "User1" ).
            match( path( "n", OUTGOING, "hyperEdge" ).relationship( "hasRoleInGroup" )
                       .path( OUTGOING, "group" ).relationship( "hasGroup" ) ).
            match( path( "hyperEdge", OUTGOING, "role" ).relationship( "hasRole" ) ).toQuery();

        assertEquals( "START n=node:node_auto_index(name=\"User1\") MATCH (n)-[:hasRoleInGroup]->(hyperEdge)-[:hasGroup]->(group),(hyperEdge)-[:hasRole]->(role) WHERE group.name=\"Group2\" RETURN role.name",
                      CypherQuery.newWhereQuery( query ).
                          where( eq( "group.name", "Group2" ) ).
                          returnProperties( "role.name" ).
                          toString() );

        assertEquals( "START n=node:node_auto_index(name=\"User1\") MATCH (n)-[:hasRoleInGroup]->(hyperEdge)-[:hasGroup]->(group),(hyperEdge)-[:hasRole]->(role) RETURN role.name,group.name ORDER BY role.name ASCENDING",
                      CypherQuery.newWhereQuery( query ).
                          returnProperties( "role.name", "group.name" ).
                          orderBy( "role.name", ASCENDING ).
                          toString() );
    }

    @Test
    public void test15_12_2()
    {
        assertEquals( "START joe=node:node_auto_index(name=\"Joe\") MATCH (joe)-[:knows]->(friend)-[:knows]->(friend_of_friend),(joe)-[r?:knows]->(friend_of_friend) WHERE r is null RETURN friend_of_friend.name,count(*) ORDER BY count(*) DESCENDING,friend_of_friend.name",
                      newQuery().
                          nodesLookup( "joe", "node_auto_index", "name", "Joe" ).
                          match( path( "joe", OUTGOING, "friend" ).relationship( "knows" )
                                     .path( OUTGOING, "friend_of_friend" )
                                     .relationship( "knows" ) ).
                          match( path( "joe", OUTGOING, "friend_of_friend" ).name( "r" )
                                     .optional()
                                     .relationship( "knows" ) ).
                          where( isNull( "r" ) ).
                          returnProperties( "friend_of_friend.name" ).count().
                          orderBy( "count(*)", DESCENDING ).orderBy( "friend_of_friend.name" ).
                          toString() );
    }

    @Test
    public void test15_12_3()
    {
        assertEquals( "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                      newQuery().
                          nodesLookup( "place", "node_auto_index", "name", "CoffeShop1" ).
                          match( path( "place", INCOMING, "person" ).relationship( "favorite" )
                                     .path( OUTGOING, "stuff" ).relationship( "favorite" ) ).
                          returnProperties( "stuff.name" ).count().
                          orderBy( "count(*)", DESCENDING ).orderBy( "stuff.name" ).
                          toString() );
        
        assertEquals( "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)-[:tagged]->(tag)<-[:tagged]-(otherPlace) RETURN otherPlace.name,collect(tag.name) ORDER BY otherPlace.name DESCENDING",
                      newQuery().
                          nodesLookup( "place", "node_auto_index", "name", "CoffeShop1" ).
                          match( path( "place", OUTGOING, "tag" ).relationship( "tagged" ).path( INCOMING, "otherPlace" ).relationship( "tagged" ) ).
                          returnProperties( "otherPlace.name" ).collect( "tag.name" ).
                          orderBy( "otherPlace.name", DESCENDING ).
                          toString());
    }

    @Test
    public void test15_12_4()
    {
        assertEquals( "START me=node:node_auto_index(name=\"Joe\") MATCH (me)-[:favorite]->(stuff)<-[:favorite]-(person),(me)-[r?:friend]-(person) WHERE r is null RETURN person.name,count(stuff) ORDER BY count(stuff) DESCENDING",
                      newQuery().
                        nodesLookup( "me", "node_auto_index", "name", "Joe" ).
                        match( path( "me", OUTGOING, "stuff" ).relationship( "favorite" ).path( INCOMING, "person" ).relationship( "favorite" ) ).
                        match( path("me","person").name( "r" ).optional().relationship( "friend" ) ).
                        where( isNull( "r" ) ).
                        returnProperties( "person.name" ).count( "stuff" ).
                        orderBy( "count(stuff)", DESCENDING ).
                        toString());
    }
}
