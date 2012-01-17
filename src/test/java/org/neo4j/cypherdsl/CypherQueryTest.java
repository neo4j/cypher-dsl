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

import org.junit.Assert;
import org.junit.Test;

import static org.junit.Assert.*;
import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.Expression.param;
import static org.neo4j.cypherdsl.query.ReturnExpression.*;
import static org.neo4j.cypherdsl.query.StartExpression.*;
import static org.neo4j.cypherdsl.query.WhereExpression.*;

/**
 * Tests for all parts of the Cypher DSL.
 */
public class CypherQueryTest
{
    @Test
    public void testStartNodes()
    {
        // Start with id
        assertEquals( "START john=node(0) RETURN john", start( node( "john", 0 ) ).returns( nodes( "john" ) ).toString() );

        assertEquals( "START family=node(0,1) RETURN family", start(node( "family", 0, 1 )).returns( nodes("family" )).toString() );

        // Start with parameters
        assertEquals( "START john=node({name}) RETURN john", start(node( "john", "name" )).returns( nodes("john" )).toString());

        assertEquals( "START family=node() RETURN family", start(node( "family")).returns( nodes("family" )).toString());

        // Start with lookup
        assertEquals( "START john=node:nodes(name=\"John\") RETURN john", start(lookup( "john", "nodes", "name", "John" )).returns( nodes("john" )).toString());

        // Start with query
        assertEquals( "START john=node:nodes(\"name:John\") RETURN john", start(query( "john", "nodes", "name:John" )).returns( nodes("john" )).toString());

        // Error handling
        try
        {
            start(node( null, 0 ));
            Assert.fail( "Expected exception" );
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            start(node( "john", -1 ));
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            start(lookup( "john", "foo", null, "bar" ));
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }
    }

    @Test
    public void testStartRelationships()
    {
        // Start with id
        assertEquals( "START knows=relationship(0) RETURN knows", start(relationship( "knows", 0 )).returns( relationships("knows") ).toString());

        assertEquals( "START likes=relationship(0,1) RETURN likes", start(relationship( "likes", 0, 1 )).returns( relationships("likes") ).toString() );

        // Start with parameters
        assertEquals( "START knows=relationship({name}) RETURN knows", start(relationship( "knows", "name" )).returns( relationships("knows") ).toString());

        assertEquals( "START likes=relationship({websitea}) RETURN likes", start(relationship( "likes", "websitea")).returns( relationships("likes") ).toString());

        // Start with index
        assertEquals( "START knows=relationship:relationships(type=\"Starred\") RETURN knows", start(relationshipLookup( "knows", "relationships", "type", "Starred" )).returns( relationships("knows") ).toString());

        // Error handling
        try
        {
            start(relationship( null, 0 ));
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            start(relationship( "john", -1 ));
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            start(relationshipLookup( "likes", "websitea", null, "websiteb" ));
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }
    }

    @Test
    public void testWhere()
    {
        assertEquals( "START n=node(0) WHERE n.name={name} RETURN n", start(node( "n", 0 )).where( eq( "n.name", param( "name" ) ) ).returns( nodes("n") ).toString());
        
        assertEquals( "START n=node(0) WHERE (n.age>30 and n.name=\"Tobias\") or not(n.name=\"Tobias\") RETURN n", start(node( "n", 0 )).
            where( gt( "n.age", 30 ).and( eq( "n.name", "Tobias" ) ).or( not( eq( "n.name", "Tobias" ) ) ) ).
            returns( properties("n") ).toString());
    }

    @Test
    public void testReturn()
    {
        // Return with node
        assertEquals( "START john=node(0) RETURN john", start(node( "john", 0 )).returns( nodes("john") ).toString());

        assertEquals( "START mom=node(0),dad=node(1) RETURN mom,dad", start(node( "mom", 0 ), node( "dad", 1 )).returns( nodes("mom", "dad") ).toString());

        assertEquals( "START mom=node(0),dad=node(1) RETURN mom.age AS momsAge,dad.age AS dadsAge", start(node( "mom", 0 ), node( "dad", 1 )).returns( properties("mom.age", "dad.age").as("momsAge","dadsAge") ).toString());
    }
}
