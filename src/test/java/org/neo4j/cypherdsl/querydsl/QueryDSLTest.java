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
package org.neo4j.cypherdsl.querydsl;

import static com.querydsl.core.alias.Alias.$;
import static com.querydsl.core.alias.Alias.alias;
import static com.querydsl.core.types.dsl.Expressions.constant;
import static com.querydsl.core.types.dsl.Expressions.predicate;
import static org.junit.Assert.assertEquals;
import static org.neo4j.cypherdsl.CypherQuery.nodesById;
import static org.neo4j.cypherdsl.CypherReferenceTest.CYPHER;
import static org.neo4j.cypherdsl.Order.DESCENDING;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.count;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.identifier;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.literal;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.lookup;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.node;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.order;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.query;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.start;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.string;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.toBooleanExpression;
import static org.neo4j.cypherdsl.querydsl.CypherQueryDSL.toQuery;

import com.querydsl.core.BooleanBuilder;
import com.querydsl.core.types.dsl.Expressions;
import com.querydsl.core.types.ExpressionUtils;
import com.querydsl.core.types.Ops;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.PredicateOperation;
import com.querydsl.core.types.dsl.BooleanOperation;
import com.querydsl.core.types.dsl.Param;
import org.junit.Assert;
import org.junit.Test;
import org.neo4j.cypherdsl.Order;

/**
 * Test for integration with QueryDSL
 */
public class QueryDSLTest
{
    @Test
    public void testQueryDSL()
    {
        {
            Path<Person> person = Expressions.path( Person.class, "n" );
            Path<String> personFirstName = Expressions.path( String.class, person, "firstName" );
            Path<Integer> personAge = Expressions.path( Integer.class, person, "age" );
            BooleanBuilder expr = new BooleanBuilder( predicate( Ops.EQ, personFirstName,
                    constant( "P" ) ) ).and( predicate( Ops.GT, personAge, constant( 25 ) ) );

            Assert.assertEquals( CYPHER + "START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                    start( nodesById( identifier( person ), 1, 2, 3 ) )
                            .where( toBooleanExpression( expr ) )
                            .returns( identifier( person ) )
                            .toString() );
        }

        {
            Person person = alias( Person.class, "n" );
            Assert.assertEquals( CYPHER + "START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                    start( nodesById( person.toString(), 1, 2, 3 ) )
                            .where( toBooleanExpression( $( person.getFirstName() ).eq( "P" )
                                    .and( $( person.getAge() ).gt( 25 ) ) ) )
                            .returns( identifier( person.toString() ) )
                            .toString() );
        }

        {
            QPerson person = QPerson.person;
            Assert.assertEquals( CYPHER + "START person=node(1,2,3) WHERE person.firstName=\"P\" and person.age>25 " +
                    "RETURN person",
                    start( nodesById( identifier( person ), 1, 2, 3 ) )
                            .where( toBooleanExpression( person.firstName.eq( "P" ).and( person.age.gt( 25 ) ) ) )
                            .returns( identifier( person ) )
                            .toString() );
            
        }
        {
            QPerson person = QPerson.person;
            Assert.assertEquals( CYPHER + "START person=node:node_auto_index(\"firstName:rickard\") RETURN person" +
                    ".firstName ORDER BY person.firstName DESCENDING",
                    start( query( identifier( person ), identifier( "node_auto_index" ),
                            toQuery( person.firstName.eq( "Rickard" ) ) ) )
                            .returns( string( person.firstName ) )
                            .orderBy( order( string( person.firstName ), Order.DESCENDING ) )
                            .toString() );
        }

        {
            Assert.assertEquals( CYPHER + "START person=node:node_auto_index(\"firstName:rickard\") RETURN person" +
                    ".firstName ORDER BY person.firstName DESCENDING",
                    new CypherQueryDSL()
                    {{
                            QPerson person = QPerson.person;
                            starts( query( identifier( person ), identifier( "node_auto_index" ),
                                    toQuery( person.firstName.eq( "Rickard" ) ) ) )
                                    .returns( string( person.firstName ) )
                                    .orderBy( order( string( person.firstName ), Order.DESCENDING ) );
                        }}
                            .toString() );
        }

        {
            QPerson n = new QPerson( "n" );
            Assert.assertEquals( CYPHER + "START n=node(1,2,3) WHERE n.firstName=\"P\" and n.age>25 RETURN n",
                    start( nodesById( identifier( n ), 1, 2, 3 ) )
                            .where( toBooleanExpression( n.firstName.eq( "P" ).and( n.age.gt( 25 ) ) ) )
                            .returns( identifier( n ) )
                            .toString() );
        }

        {
            QPerson n = new QPerson( "n" );
            Assert.assertEquals( CYPHER + "START n=node(1,2,3) WHERE n.firstName={name} and n.age>{age} RETURN n",
                    start( nodesById( identifier( n ), 1, 2, 3 ) )
                            .where( toBooleanExpression( n.firstName
                                    .eq( new Param<String>( String.class, "name" ) )
                                    .and( n.age
                                            .gt( new Param<Integer>( Integer.class, "age" ) ) ) ) )
                            .returns( identifier( n ) )
                            .toString() );
        }

        {
            QPerson n = new QPerson( "n" );
            Assert.assertEquals( CYPHER + "START n=node(1,2,3) WHERE n.firstName=~\"(?i).*rick.*\" RETURN n",
                    start( nodesById( identifier( n ), 1, 2, 3 ) )
                            .where( toBooleanExpression( n.firstName.like( "(?i).*rick.*" )))
                            .returns( identifier( n ) )
                            .toString() );
        }

        {
            QPerson n = new QPerson( "n" );
            Assert.assertEquals(CYPHER + "START n=node(1,2,3) WHERE has(n.firstName) RETURN n",
                    start(nodesById(identifier(n), 1, 2, 3))
                    		.where(toBooleanExpression((PredicateOperation)ExpressionUtils.operation(Boolean.class,Ops.EXISTS, n.firstName)))
                                    .returns(identifier(n))
                                    .toString());
        }
    }

    @Test
    public void testCookbookExample()
    {
        // Static method style
        QStuff stuff = QStuff.stuff;
        QPlace place = QPlace.place;
        QPerson person = QPerson.person;
        assertEquals( CYPHER + "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-" +
                "(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                start( lookup( identifier( place ), identifier( "node_auto_index" ), identifier( place.name ),
                        literal( "CoffeShop1" ) ) ).
                        match( node( identifier( place ) ).in( "favorite" ).node( identifier( person ) ).out(
                                "favorite" ).node( identifier( stuff ) ) ).
                        returns( string( stuff.name ), count() ).
                        orderBy( order( count(), DESCENDING ), string( stuff.name ) ).
                        toString() );

        // Java instance initialization block style
        assertEquals( CYPHER + "START place=node:node_auto_index(name=\"CoffeShop1\") MATCH (place)<-[:favorite]-" +
                "(person)-[:favorite]->(stuff) RETURN stuff.name,count(*) ORDER BY count(*) DESCENDING,stuff.name",
                new CypherQueryDSL()
                {{
                        QStuff stuff = QStuff.stuff;
                        QPlace place = QPlace.place;
                        QPerson person = QPerson.person;

                        starts( lookup( identifier( place ), identifier( "node_auto_index" ),
                                identifier( place.name ), literal( "CoffeShop1" ) ) ).
                                match( node( identifier( place ) ).in( "favorite" ).node( identifier( person ) ).out( "favorite" ).node( identifier( stuff ) ) ).
                                returns( string( stuff.name ), count() ).
                                orderBy( order( count(), DESCENDING ), string( stuff.name ) );
                    }}.toString() );

    }

}
