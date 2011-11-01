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
package org.neo4j.cypherdsl.query;

import java.io.Serializable;

import static org.neo4j.cypherdsl.query.Query.*;

/**
 * Provides the possible expressions for the START clause.
 */
public abstract class StartExpression
    extends Expression
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
        startNodes.nodes = literals( id );
        return startNodes;
    }

    public static StartNodes nodes( String name, String... parameters )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameters, "Parameters" );

        StartExpression.StartNodes startNodes = new StartExpression.StartNodes();
        startNodes.name = name;
        startNodes.nodes = parameters( parameters );
        return startNodes;
    }

    public static StartNodesLookup nodesLookup( String name, String indexName, String key, String value )
    {
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        return nodesLookup( name, indexName, literal( key ), literal( value ) );
    }

    public static StartNodesLookup nodesLookup( String name, String indexName, Value key, Value value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );

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
        startRelationships.relationships = literals( id );
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
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        return relationshipsLookup( name, indexName, literal( key ), literal( value ) );
    }

    public static StartRelationshipsIndex relationshipsLookup( String name, String indexName, Value key, Value value )
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
        public Value[] nodes;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=node(" );
            for( int i = 0; i < nodes.length; i++ )
            {
                Value node = nodes[ i ];
                if (i > 0)
                    builder.append( ',' );
                node.asString( builder );
            }
            builder.append( ')' );
        }
    }

    public static class StartNodesLookup
        extends StartExpression
    {
        public String index;
        public Value key;
        public Value value;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=node:" ).append( index ).append( '(' );
            key.asString( builder );
            builder.append( "=" );
            value.asString( builder );
            builder.append( ')' );
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
        public Value[] relationships;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=relationship(" );
            for( int i = 0; i < relationships.length; i++ )
            {
                Value rel = relationships[ i ];
                if (i > 0)
                    builder.append( ',' );
                rel.asString( builder );
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
        public Value key;
        public Value value;

        public void asString(StringBuilder builder)
        {
            builder.append( name ).append( "=relationship:" ).append( index ).append( '(' );
            key.asString( builder );
            builder.append( '=' );
            value.asString( builder );
            builder.append( ')' );
        }
    }

}
