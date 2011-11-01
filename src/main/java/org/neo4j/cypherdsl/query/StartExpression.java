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

/**
 * TODO
 */
public abstract class StartExpression
    implements AsString, Serializable,Cloneable
{
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
