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
