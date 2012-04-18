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

/**
 * Provides the possible expressions for the WHERE clause.
 */
public abstract class WhereExpression
    extends PredicateExpression
{

    public static class WhereRelationship
        extends PredicateExpression
    {
        public String from = "";
        public String to = "";
        public Direction direction = Direction.BOTH;
        public String[] relationships = new String[0];
        public Integer minHops;
        public Integer maxHops;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( '(' ).append( from ).append( ')' );
            builder.append( direction.equals( Direction.IN ) ? "<-" : "-" );

            if( relationships.length > 0 || minHops != null || maxHops != null )
            {
                builder.append( '[' );
                if( relationships.length > 0 )
                {
                    builder.append( ':' );
                    for( int i = 0; i < relationships.length; i++ )
                    {
                        String rel = relationships[ i ];
                        if (i>0)
                            builder.append( "|" );
                        builder.append( rel );
                    }
                }

                if( minHops != null || maxHops != null )
                {
                    builder.append( '*' );
                    if( minHops != null )
                    {
                        builder.append( minHops );
                    }
                    builder.append( ".." );
                    if( maxHops != null )
                    {
                        builder.append( maxHops );
                    }
                }

                builder.append( ']' );
            }

            builder.append( direction.equals( Direction.OUT ) ? "->" : "-" );

            builder.append( '(' ).append( to ).append( ')' );
        }

        public WhereRelationship out(String... relationship)
        {
            this.direction = Direction.OUT;
            this.relationships = relationship;
            return this;
        }

        public WhereRelationship in(String... relationship)
        {
            this.direction = Direction.IN;
            this.relationships = relationship;
            return this;
        }

        public WhereRelationship both( String... relationship )
        {
            this.direction = Direction.BOTH;
            this.relationships = relationship;
            return this;
        }

        public WhereRelationship hops( Integer minHops, Integer maxHops )
        {
            if (minHops != null && minHops < 0)
                throw new IllegalArgumentException( "Minimum number of hops must be over zero" );

            if (maxHops != null && maxHops < 0)
                throw new IllegalArgumentException( "Maximum number of hops must be over zero" );

            this.minHops = minHops;
            this.maxHops = maxHops;
            return this;
        }

        public WhereRelationship from(String from)
        {
            this.from = from;
            return this;
        }

        public WhereRelationship to(String to)
        {
            this.to = to;
            return this;
        }
    }

    public static class WhereIn
        extends PredicateExpression
    {
        public Expression expression;
        public Expression[] elements;

        @Override
        public void asString( StringBuilder builder )
        {
            expression.asString( builder );
            builder.append( " IN [" );
            for( int i = 0; i < elements.length; i++ )
            {
                Expression element = elements[ i ];
                if (i>0)
                    builder.append( "," );
                element.asString( builder );
            }
            builder.append( "]" );
        }
    }
}
