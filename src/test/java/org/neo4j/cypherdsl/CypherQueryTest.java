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

import org.junit.Assert;
import org.junit.Test;
import org.neo4j.cypherdsl.query.Expression;

import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.Expression.param;
import static org.neo4j.cypherdsl.query.StartExpression.*;
import static org.neo4j.cypherdsl.query.MatchExpression.*;
import static org.neo4j.cypherdsl.query.WhereExpression.*;
import static org.neo4j.cypherdsl.query.OrderByExpression.*;
import static org.neo4j.cypherdsl.query.ReturnExpression.*;

/**
 * Tests for all parts of the Cypher DSL.
 */
public class CypherQueryTest
{
    @Test
    public void testDSL()
    {
        // Minimal
        System.out.println( start( node( "john", 0 )).returns( nodes("john") ).toString());

        // Maximal
        System.out.println( start( node( "john", 0 )).returns( nodes("john") ).orderBy( property("john.name") ).skip( 3 ).limit( 10 ).toString());
    }

    @Test
    public void testStartNodes()
    {
        // Start with id
        System.out.println( start(node( "john", 0 )).returns( nodes("john") ).toString());

        System.out.println( start(node( "family", 0, 1 )).returns( nodes("family" )).toString() );

        // Start with parameters
        System.out.println( start(node( "john", "name" )).returns( nodes("john" )).toString());

        System.out.println( start(node( "family")).returns( nodes("family" )).toString());

        // Start with lookup
        System.out.println( start(lookup( "john", "nodes", "name", "John" )).returns( nodes("john" )).toString());

        // Start with query
        System.out.println( start(query( "john", "nodes", "name:John" )).returns( nodes("john" )).toString());

        // Error handling
        try
        {
            start(node( null, 0 ));
            Assert.fail( "Expected exception");
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
        System.out.println( start(relationship( "knows", 0 )).returns( relationships("knows") ).toString());

        System.out.println( start(relationship( "likes", 0, 1 )).returns( relationships("likes") ).toString() );

        // Start with parameters
        System.out.println( start(relationship( "knows", "name" )).returns( relationships("knows") ).toString());

        System.out.println( start(relationship( "likes", "websitea", "websiteb" )).returns( relationships("likes") ).toString());

        // Start with index
        System.out.println( start(relationshipLookup( "knows", "relationships", "type", "Starred" )).returns( relationships("knows") ).toString());

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
        System.out.println( start(node( "n", 0 )).where( eq( "n.name", param( "name" ) ) ).returns( nodes("n") ));
        
        System.out.println( start(node( "n", 0 )).
            where( gt( "n.age", 30 ).and( eq( "n.name", "Tobias" ) ).or( not( eq( "n.name", "Tobias" ) ) ) ).
            returns( properties("n") ));
    }

    @Test
    public void testReturn()
    {
        // Return with node
        System.out.println( start(node( "john", 0 )).returns( nodes("john") ).toString());

        System.out.println( start(node( "mom", 0 ), node( "dad", 1 )).returns( nodes("mom", "dad") ).toString());
    }
}
