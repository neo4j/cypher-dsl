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

/**
 * Provides the possible expressions for the MATCH clause.
 */
public abstract class MatchExpression
    implements AsString, Serializable, Cloneable
{
    /**
     * The direction of the path expression.
     */
    public enum Direction
    {
        BOTH,
        OUT,
        IN
    }

    public static Path path()
    {
        return new Path();
    }

    public static Path path(String name)
    {
        Path path = new Path();
        path.pathName = name;
        return path;
    }

    /**
     * Use this to invoke the shortestPath function
     *
     * @param name
     * @return
     */
    public static FunctionPath shortestPath( String name )
    {
        Query.checkNull( name, "Name" );

        FunctionPath functionPath = new FunctionPath();
        functionPath.function = "shortestPath";
        functionPath.pathName = name;
        return functionPath;
    }

    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        return super.clone();
    }

    public static class AbstractPath<T extends AbstractPath>
        extends MatchExpression
    {
        public String to = "";
        public Direction direction = Direction.BOTH;
        public String as;
        public String relationship;
        public boolean optional;
        public Integer minHops;
        public Integer maxHops;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( direction.equals( Direction.IN ) ? "<-" : "-" );

            if( as != null || relationship != null || optional || minHops != null || maxHops != null )
            {
                builder.append( '[' );
                if( as != null )
                {
                    builder.append( as );
                }
                if( optional )
                {
                    builder.append( '?' );
                }
                if( relationship != null )
                {
                    builder.append( ':' ).append( relationship );
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

        public T out()
        {
            this.direction = Direction.OUT;
            return (T) this;
        }

        public T in()
        {
            this.direction = Direction.IN;
            return (T) this;
        }

        public T both()
        {
            this.direction = Direction.BOTH;
            return (T) this;
        }

        public T out(String relationship)
        {
            this.direction = Direction.OUT;
            this.relationship = relationship;
            return (T) this;
        }

        public T in(String relationship)
        {
            this.direction = Direction.IN;
            this.relationship = relationship;
            return (T) this;
        }

        public T both( String relationship )
        {
            this.direction = Direction.BOTH;
            this.relationship = relationship;
            return (T) this;
        }

        public T as( String name )
        {
            Query.checkEmpty( name, "Name" );
            this.as = name;
            return (T) this;
        }

        public T rel( Enum relationship )
        {
            Query.checkNull( relationship, "Relationship" );
            this.relationship = relationship.name();
            return (T) this;
        }

        public T rel( String relationship )
        {
            Query.checkEmpty( relationship, "Relationship" );
            this.relationship = relationship;
            return (T) this;
        }

        public T optional()
        {
            this.optional = true;
            return (T) this;
        }

        public T hops( Integer minHops, Integer maxHops )
        {
            if (minHops != null && minHops < 0)
                throw new IllegalArgumentException( "Minimum number of hops must be over zero" );

            if (maxHops != null && maxHops < 0)
                throw new IllegalArgumentException( "Maximum number of hops must be over zero" );

            this.minHops = minHops;
            this.maxHops = maxHops;
            return (T) this;
        }

        public T to(String to)
        {
            this.to = to;
            return (T) this;
        }

        public Link link()
        {
            Link path = new Link();
            path.leftPath = this;

            return path;
        }
    }

    public static class Path
        extends AbstractPath<Path>
    {
        public String pathName;
        public String from = "";

        @Override
        public void asString( StringBuilder builder )
        {
            if (pathName != null)
                builder.append( pathName ).append( '=' );
            builder.append( '(' ).append( from ).append( ')' );
            super.asString( builder );
        }

        public Path from(String from)
        {
            this.from = from;
            return this;
        }
    }

    public static class Link
        extends AbstractPath<Link>
    {
        public AbstractPath leftPath;

        @Override
        public void asString( StringBuilder builder )
        {
            leftPath.asString( builder );
            super.asString( builder );
        }
    }

    public static class FunctionPath
        extends MatchExpression
    {
        public String pathName;
        public String function;
        public String from = "";
        public String to = "";
        public Direction direction = Direction.BOTH;
        public Integer minHops;
        public Integer maxHops;

        public FunctionPath out()
        {
            this.direction = Direction.OUT;
            return this;
        }

        public FunctionPath in()
        {
            this.direction = Direction.IN;
            return this;
        }

        public FunctionPath both()
        {
            this.direction = Direction.BOTH;
            return this;
        }

        public FunctionPath from(String from)
        {
            Query.checkEmpty( from, "From" );
            this.from = from;
            return this;
        }

        public FunctionPath to(String to)
        {
            Query.checkEmpty( to, "To" );
            this.to = to;
            return this;
        }

        public FunctionPath hops( Integer minHops, Integer maxHops )
        {
            if (minHops != null && minHops < 0)
                throw new IllegalArgumentException( "Minimum number of hops must be over zero" );

            if (maxHops != null && maxHops < 0)
                throw new IllegalArgumentException( "Maximum number of hops must be over zero" );

            this.minHops = minHops;
            this.maxHops = maxHops;
            return this;
        }

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( pathName ).append( "=" );
            builder.append( function ).append( '(' );
            builder.append( '(' ).append( from ).append( ')' );

            builder.append( direction.equals( Direction.IN ) ? "<-" : "-" );

            if( minHops != null || maxHops != null )
            {
                builder.append( '[' );
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
            builder.append( ')' );
        }
    }
}
