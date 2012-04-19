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
    public Identifier name;

    public static class AllNodes
        extends Expression
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
        public Expression[] nodes;

        public void asString(StringBuilder builder)
        {
            name.asString( builder );
            builder.append( "=node(" );
            for( int i = 0; i < nodes.length; i++ )
            {
                Expression node = nodes[ i ];
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
        public Identifier index;
        public Identifier key;
        public Expression value;

        public void asString(StringBuilder builder)
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
        public Identifier index;
        public String query;

        public void asString(StringBuilder builder)
        {
            name.asString( builder );
            builder.append( "=node:" );
            index.asString( builder );
            builder.append( "(\"" ).append( query).append( "\")" );
        }
    }

    public static class StartRelationships
        extends StartExpression
    {
        public Expression[] relationships;

        public void asString(StringBuilder builder)
        {
            name.asString( builder );
            builder.append( "=relationship(" );
            for( int i = 0; i < relationships.length; i++ )
            {
                Expression rel = relationships[ i ];
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
        public String parameter;

        public void asString(StringBuilder builder)
        {
            name.asString( builder );
            builder.append( "=relationship({" ).append( parameter ).append( "})" );
        }
    }

    public static class StartRelationshipsIndex
        extends StartExpression
    {
        public Identifier index;
        public Identifier key;
        public Literal value;

        public void asString(StringBuilder builder)
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
