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

import static org.junit.Assert.assertEquals;
import static org.neo4j.cypherdsl.CypherQuery.allNodes;
import static org.neo4j.cypherdsl.CypherQuery.as;
import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.CypherQuery.identifiers;
import static org.neo4j.cypherdsl.CypherQuery.literal;
import static org.neo4j.cypherdsl.CypherQuery.lookup;
import static org.neo4j.cypherdsl.CypherQuery.nodesById;
import static org.neo4j.cypherdsl.CypherQuery.nodesByParameter;
import static org.neo4j.cypherdsl.CypherQuery.not;
import static org.neo4j.cypherdsl.CypherQuery.param;
import static org.neo4j.cypherdsl.CypherQuery.query;
import static org.neo4j.cypherdsl.CypherQuery.queryByParameter;
import static org.neo4j.cypherdsl.CypherQuery.relationshipLookup;
import static org.neo4j.cypherdsl.CypherQuery.relationshipsById;
import static org.neo4j.cypherdsl.CypherQuery.relationshipsByParameter;
import static org.neo4j.cypherdsl.CypherQuery.start;

import org.junit.Assert;
import org.junit.Test;

/**
 * Tests for all parts of the Cypher DSL.
 */
public class CypherQueryTest extends AbstractCypherTest
{
    @Test
    public void testStartNodes()
    {
        // Start with id
        assertQueryEquals( CYPHER + "START john=node(0) RETURN john", start( nodesById( "john",
                0 ) ).returns( identifier( "john" ) ).toString() );

        assertQueryEquals( CYPHER + "START family=node(0,1) RETURN family", start( nodesById( "family", 0,
                1 ) ).returns( identifier( "family" ) ).toString() );

        // Start with parameters
        assertQueryEquals( CYPHER + "START john=node({name}) RETURN john", start( nodesByParameter( "john",
                "name" ) ).returns( identifier( "john" ) ).toString() );

        assertQueryEquals( CYPHER + "START family=node(*) RETURN family", start( allNodes( "family" ) ).returns(
                identifier( "family" ) ).toString() );

        // Start with lookup
        assertQueryEquals( CYPHER + "START john=node:nodes(name=\"John\") RETURN john", start( lookup( "john",
                "nodes", "name", "John" ) ).returns( identifier( "john" ) ).toString() );

        // Start with query
        assertQueryEquals( CYPHER + "START john=node:nodes(\"name:John\") RETURN john", start( query( "john",
                "nodes", "name:John" ) ).returns( identifier( "john" ) ).toString() );

        // Start with query with spaces
        assertQueryEquals( CYPHER + "START john=node:nodes('name:\"John doe\"') RETURN john", start( query( "john",
                "nodes", "name:\"John doe\"" ) ).returns( identifier( "john" ) ).toString() );

        // Start with query-param
        assertEquals( CYPHER + "START john=node:nodes({param}) RETURN john", start( queryByParameter( "john",
                "nodes", "param" ) ).returns( identifier( "john" ) ).toString() );

        // Error handling
        try
        {
            start( nodesById( "", 0 ) );
            Assert.fail( "Expected exception" );
        }
        catch ( Exception e )
        {
            // Ok
        }

        try
        {
            start( nodesById( "john", -1 ) );
            Assert.fail( "Expected exception" );
        }
        catch ( Exception e )
        {
            // Ok
        }

        try
        {
            start( lookup( "john", "foo", null, "bar" ) );
            Assert.fail( "Expected exception" );
        }
        catch ( Exception e )
        {
            // Ok
        }
    }

    @Test
    public void testStartRelationships()
    {
        // Start with id
        assertQueryEquals( CYPHER + "START knows=relationship(0) RETURN knows", start( relationshipsById( "knows",
                0 ) ).returns( identifier( "knows" ) ).toString() );

        assertQueryEquals( CYPHER + "START likes=relationship(0,1) RETURN likes", start( relationshipsById( "likes",
                0, 1 ) ).returns( identifier( "likes" ) ).toString() );

        // Start with parameters
        assertQueryEquals( CYPHER + "START knows=relationship({name}) RETURN knows",
                start( relationshipsByParameter( "knows", "name" ) ).returns( identifier( "knows" ) ).toString() );

        assertQueryEquals( CYPHER + "START likes=relationship({websitea}) RETURN likes",
                start( relationshipsByParameter( "likes", "websitea" ) ).returns( identifier( "likes" ) ).toString() );

        // Start with index
        assertQueryEquals( CYPHER + "START knows=relationship:relationships(type=\"Starred\") RETURN knows",
                start( relationshipLookup( "knows", "relationships", "type", "Starred" ) ).returns( identifier(
                        "knows" ) ).toString() );

        // Error handling
        try
        {
            start( relationshipsById( "", 0 ) );
            Assert.fail( "Expected exception" );
        }
        catch ( Exception e )
        {
            // Ok
        }

        try
        {
            start( relationshipsById( "john", -1 ) );
            Assert.fail( "Expected exception" );
        }
        catch ( Exception e )
        {
            // Ok
        }

        try
        {
            start( relationshipLookup( "likes", "websitea", null, "websiteb" ) );
            Assert.fail( "Expected exception" );
        }
        catch ( Exception e )
        {
            // Ok
        }
    }

    @Test
    public void testWhere()
    {
        assertQueryEquals( CYPHER + "START n=node(0) WHERE n.name={name} RETURN n", start( nodesById( "n",
                0 ) ).where( identifier( "n" ).string( "name" ).eq( param( "name" ) ) ).returns( identifier( "n" ) )
                .toString() );

        assertQueryEquals( CYPHER + "START n=node(0) WHERE (n.age>30 and n.name=\"Tobias\") or not(n.name=\"Tobias\")" +
                " RETURN n", start( nodesById( "n", 0 ) )
                .
                        where( identifier( "n" ).number( "age" ).gt( 30 )
                                .and( identifier( "n" ).string( "name" ).eq( "Tobias" ) )
                                .or( not( identifier( "n" ).string( "name" ).eq( "Tobias" ) ) ) )
                .
                        returns( identifier( "n" ) )
                .toString() );
    }

    @Test
    public void testReturn()
    {
        // Return with node
        assertQueryEquals( CYPHER + "START john=node(0) RETURN john", start( nodesById( "john",
                0 ) ).returns( identifier( "john" ) ).toString() );

        assertQueryEquals( CYPHER + "START mom=node(0),dad=node(1) RETURN mom,dad", start( nodesById( "mom", 0 ),
                nodesById( "dad", 1 ) ).returns( identifiers( "mom", "dad" ) ).toString() );

        assertQueryEquals( CYPHER + "START mom=node(0),dad=node(1) RETURN mom.age AS momsAge,dad.age AS dadsAge",
                start( nodesById( "mom", 0 ), nodesById( "dad", 1 ) ).
                        returns( as( identifier( "mom" ).property( "age" ), "momsAge" ),
                                as( identifier( "dad" ).property( "age" ), "dadsAge" ) ).toString() );
    }

    @Test
    public void testLiteral()
    {
        StringBuilder builder = new StringBuilder();
        literal( "x\\x\"x" ).asString( builder );
        assertEquals( "\"x\\\\x\\\"x\"", builder.toString() );
    }
}
