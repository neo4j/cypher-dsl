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
package org.neo4j.cypherdsl.expression;

import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.query.AbstractExpression;

/**
 * Provides the possible expressions for the START clause.
 */
public abstract class StartExpression
        extends AbstractExpression
{
    public final Identifier name;

    protected StartExpression( Identifier name )
    {
        this.name = name;
    }

    public static class AllNodes
            extends AbstractExpression
    {
        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( "*" );
        }
    }

    public static class StartNodes
            extends StartExpression
    {
        public final Expression[] nodes;

        public StartNodes( Identifier name, Expression[] nodes )
        {
            super( name );
            this.nodes = nodes;
        }

        public void asString( StringBuilder builder )
        {
            name.asString( builder );
            builder.append( "=node(" );
            for ( int i = 0; i < nodes.length; i++ )
            {
                Expression node = nodes[i];
                if ( i > 0 )
                {
                    builder.append( ',' );
                }
                node.asString( builder );
            }
            builder.append( ')' );
        }
    }

    public static class StartNodesLookup
            extends StartExpression
    {
        public final Identifier index;
        public final ReferenceExpression key;
        public final Expression value;

        public StartNodesLookup( Identifier name, Identifier index, ReferenceExpression key, Expression value )
        {
            super( name );
            this.index = index;
            this.key = key;
            this.value = value;
        }

        public void asString( StringBuilder builder )
        {
            name.asString( builder );
            builder.append( "=node:" );
            index.asString( builder );
            builder.append( '(' );
            key.asString( builder );
            builder.append( "=" );
            value.asString( builder );
            builder.append( ')' );
        }
    }

    public static class StartNodesQuery
            extends StartExpression
    {
        public final Identifier index;
        public final String query;

        public StartNodesQuery( Identifier name, Identifier index, String query )
        {
            super( name );
            this.index = index;
            this.query = query;
        }

        public void asString( StringBuilder builder )
        {
            name.asString( builder );
            builder.append( "=node:" );
            index.asString( builder );
            if (query.contains("\"")) {
                builder.append( "('" ).append( query ).append( "')" );
            } else {
                builder.append( "(\"" ).append( query ).append( "\")" );
            }
        }
    }

    public static class StartNodesQueryParam
            extends StartExpression
    {
        public final Identifier index;
        public final String param;

        public StartNodesQueryParam( Identifier name, Identifier index, String param )
        {
            super( name );
            this.index = index;
            this.param = param;
        }

        public void asString( StringBuilder builder )
        {
            name.asString( builder );
            builder.append( "=node:" );
            index.asString( builder );
            builder.append( "({" ).append( param ).append( "})" );
        }
    }

    public static class StartRelationships
            extends StartExpression
    {
        public final Expression[] relationships;

        public StartRelationships( Identifier name, Expression[] relationships )
        {
            super( name );
            this.relationships = relationships;
        }

        public void asString( StringBuilder builder )
        {
            name.asString( builder );
            builder.append( "=relationship(" );
            for ( int i = 0; i < relationships.length; i++ )
            {
                Expression rel = relationships[i];
                if ( i > 0 )
                {
                    builder.append( ',' );
                }
                rel.asString( builder );
            }
            builder.append( ')' );
        }
    }

    public static class StartRelationshipsParameters
            extends StartExpression
    {
        public final String parameter;

        public StartRelationshipsParameters( Identifier name, String parameter )
        {
            super( name );
            this.parameter = parameter;
        }

        public void asString( StringBuilder builder )
        {
            name.asString( builder );
            builder.append( "=relationship({" ).append( parameter ).append( "})" );
        }
    }

    public static class StartRelationshipsIndex
            extends StartExpression
    {
        public final Identifier index;
        public final Identifier key;
        public final StringExpression value;

        public StartRelationshipsIndex( Identifier name, Identifier index, Identifier key, StringExpression value )
        {
            super( name );
            this.index = index;
            this.key = key;
            this.value = value;
        }

        public void asString( StringBuilder builder )
        {
            name.asString( builder );
            builder.append( "=relationship:" );
            index.asString( builder );
            builder.append( '(' );
            key.asString( builder );
            builder.append( '=' );
            value.asString( builder );
            builder.append( ')' );
        }
    }

}
