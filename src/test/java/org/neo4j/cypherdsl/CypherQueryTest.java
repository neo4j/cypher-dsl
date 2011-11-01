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

import org.junit.Assert;
import org.junit.Test;

import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.WhereExpression.*;

/**
 * Tests for all parts of the Cypher DSL.
 */
public class CypherQueryTest
{
    @Test
    public void testDSL()
    {
        // Minimal
        System.out.println( newQuery().nodes( "john", 0 ).returnNode( "john" ).toString());

        // Maximal
        System.out.println( newQuery().nodes( "john", 0 ).returnNode( "john" ).orderBy( "john.name" ).skip( 3 ).limit( 10 ).toString());
    }

    @Test
    public void testStartNodes()
    {
        // Start with id
        System.out.println( newQuery().nodes( "john", 0 ).returnNode( "john" ).toString());

        System.out.println( newQuery().nodes( "family", 0, 1 ).returnNode( "family" ).toString() );

        // Start with parameters
        System.out.println( newQuery().nodes( "john", "name" ).returnNode( "john" ).toString());

        System.out.println( newQuery().nodes( "family", "mom", "dad" ).returnNode( "family" ).toString());

        // Start with lookup
        System.out.println( newQuery().nodesLookup( "john", "nodes", "name", "John" ).returnNode( "john" ).toString());

        // Start with query
        System.out.println( newQuery().nodesQuery( "john", "nodes", "name:John" ).returnNode( "john" ).toString());

        // Error handling
        try
        {
            newQuery().nodes( null, 0 );
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            newQuery().nodes( "john", -1 );
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            newQuery().nodesLookup( "john", "foo", null, "bar" );
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
        System.out.println( newQuery().relationships( "knows", 0 ).returnNode( "knows" ).toString());

        System.out.println( newQuery().relationships( "likes", 0, 1 ).returnNode( "likes" ).toString() );

        // Start with parameters
        System.out.println( newQuery().relationships( "knows", "name" ).returnNode( "knows" ).toString());

        System.out.println( newQuery().relationships( "likes", "websitea", "websiteb" ).returnNode( "likes" ).toString());

        // Start with index
        System.out.println( newQuery().relationshipsLookup( "knows", "relationships", "type", "Starred" ).returnNode( "knows" ).toString());

        // Error handling
        try
        {
            newQuery().relationships( null, 0 );
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            newQuery().relationships( "john", -1 );
            Assert.fail( "Expected exception");
        }
        catch( Exception e )
        {
            // Ok
        }

        try
        {
            newQuery().relationshipsLookup( "likes", "websitea", null, "websiteb" );
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
        System.out.println( newQuery().nodes( "n", 0 ).where( eq( "n.name", "Tobias" ) ).returnNode( "n" ));
        
        System.out.println( newQuery().
            nodes( "n", 0 ).
            where( gt( "n.age", 30 ).and( eq( "n.name", "Tobias" ) ).or( not( eq( "n.name", "Tobias" ) ) ) ).
            returnNode( "n" ));
    }

    @Test
    public void testReturn()
    {
        // Return with node
        System.out.println( newQuery().nodes( "john", 0 ).returnNode( "john" ).toString());

        System.out.println( newQuery().nodes( "mom", 0 ).nodes( "dad", 1 ).returnNode( "mom", "dad" ).toString());
    }
}
