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

package org.neo4j.cypherdsl.query;

import java.io.Serializable;

import static org.neo4j.cypherdsl.query.Query.*;

/**
 * Provides the possible expressions for the START clause.
 */
public abstract class StartExpression
    implements AsString, Serializable,Cloneable
{
    public static StartNodes nodes( String name, int... id )
    {
        checkEmpty( name, "Name" );

        for( int i : id )
        {
            if (i < 0)
                throw new IllegalArgumentException( "Id may not be below zero" );
        }

        StartExpression.StartNodes startNodes = new StartExpression.StartNodes();
        startNodes.name = name;
        startNodes.nodes = id;
        return startNodes;
    }

    public static StartNodesParameters nodes( String name, String... parameters )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameters, "Parameters" );

        StartExpression.StartNodesParameters startNodes = new StartExpression.StartNodesParameters();
        startNodes.name = name;
        startNodes.parameters = parameters;
        return startNodes;
    }

    public static StartNodesLookup nodesLookup( String name, String indexName, String key, String value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        StartExpression.StartNodesLookup startNodesLookup = new StartExpression.StartNodesLookup();
        startNodesLookup.name = name;
        startNodesLookup.index = indexName;
        startNodesLookup.key = key;
        startNodesLookup.value = value;
        return startNodesLookup;
    }

    public static StartNodesQuery nodesQuery( String name, String indexName, String query )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( query, "Query" );

        StartExpression.StartNodesQuery startNodesQuery = new StartExpression.StartNodesQuery();
        startNodesQuery.name = name;
        startNodesQuery.index = indexName;
        startNodesQuery.query = query;
        return startNodesQuery;
    }

    public static StartRelationships relationships( String name, int... id )
    {
        checkEmpty( name, "Name" );

        for( int i : id )
        {
            if (i < 0)
                throw new IllegalArgumentException( "Id may not be below zero" );
        }

        StartExpression.StartRelationships startRelationships = new StartExpression.StartRelationships();
        startRelationships.name = name;
        startRelationships.relationships = id;
        return startRelationships;
    }

    public static StartRelationshipsParameters relationships( String name, String... parameters )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameters, "Parameters" );

        StartExpression.StartRelationshipsParameters startRelationships = new StartExpression.StartRelationshipsParameters();
        startRelationships.name = name;
        startRelationships.parameters = parameters;
        return startRelationships;
    }

    public static StartRelationshipsIndex relationshipsLookup( String name, String indexName, String key, String value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        StartExpression.StartRelationshipsIndex startRelationshipsIndex = new StartExpression.StartRelationshipsIndex();
        startRelationshipsIndex.name = name;
        startRelationshipsIndex.index = indexName;
        startRelationshipsIndex.key = key;
        startRelationshipsIndex.value = value;
        return startRelationshipsIndex;
    }

    public String name;

    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        return super.clone();
    }

    public static class StartNodes
        extends StartExpression
    {
        public int[] nodes;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=node(" );
            for( int i = 0; i < nodes.length; i++ )
            {
                int node = nodes[ i ];
                if (i > 0)
                    builder.append( ',' );
                builder.append( node );
            }
            builder.append( ')' );
        }
    }

    public static class StartNodesParameters
        extends StartExpression
    {
        public String[] parameters;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=node(" );
            for( int i = 0; i < parameters.length; i++ )
            {
                String parameter = parameters[ i ];
                if (i > 0)
                    builder.append( ',' );
                builder.append( '{' ).append( parameter ).append( '}' );
            }
            builder.append( ')' );
        }
    }

    public static class StartNodesLookup
        extends StartExpression
    {
        public String index;
        public String key;
        public String value;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=node:" ).append( index ).append( '(' ).
                append( key ).append( "=\"" ).append( value ).append( "\")" );
        }
    }

    public static class StartNodesQuery
        extends StartExpression
    {
        public String index;
        public String query;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=node:" ).append( index ).append( "(\"" ).
                append( query ).append( "\")" );
        }
    }

    public static class StartRelationships
        extends StartExpression
    {
        public int[] relationships;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=relationship(" );
            for( int i = 0; i < relationships.length; i++ )
            {
                int rel = relationships[ i ];
                if (i > 0)
                    builder.append( ',' );
                builder.append( rel );
            }
            builder.append( ')' );
        }
    }

    public static class StartRelationshipsParameters
        extends StartExpression
    {
        public String[] parameters;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=relationship(" );
            for( int i = 0; i < parameters.length; i++ )
            {
                String parameter = parameters[ i ];
                if (i > 0)
                    builder.append( ',' );
                builder.append( '{' ).append( parameter ).append( '}' );
            }
            builder.append( ')' );
        }
    }

    public static class StartRelationshipsIndex
        extends StartExpression
    {
        public String index;
        public String key;
        public String value;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=relationship:" ).append( index ).append( '(' ).
                append( key ).append( "=\"" ).append( value ).append( "\")" );
        }
    }

}
