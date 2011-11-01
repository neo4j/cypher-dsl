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
public abstract class MatchExpression
    implements AsString, Serializable, Cloneable
{
    public enum Direction
    {
        ANY,
        OUTGOING,
        INCOMING
    }

    public static NamedPath named( String name, AbstractPath path )
    {
        NamedPath namedPath = new NamedPath();
        namedPath.name = name;
        namedPath.path = path;
        return namedPath;
    }

    public static Path path( String from,
                             String to
    )
    {
        Path path = new Path();
        path.from = from;
        path.to = to;

        return path;
    }

    public static Path path( String from,
                             Direction direction,
                             String to
    )
    {
        Path path = new Path();
        path.from = from;
        path.to = to;
        path.direction = direction;

        return path;
    }

    public static Path path( String from,
                             Direction direction,
                             String name,
                             String relationShip,
                             String to
    )
    {
        Path path = new Path();
        path.from = from;
        path.name = name;
        path.relationship = relationShip;
        path.to = to;
        path.direction = direction;

        return path;
    }

    public static Path path( String from,
                             Direction direction,
                             String name,
                             boolean optional,
                             String relationship,
                             Integer minHops,
                             Integer maxHops,
                             String to
    )
    {
        Path path = new Path();
        path.from = from;
        path.to = to;
        path.direction = direction;
        path.name = name;
        path.relationship = relationship;
        path.optional = optional;
        path.minHops = minHops;
        path.maxHops = maxHops;

        return path;
    }

    public static FunctionPath shortestPath( String from, String to )
    {
        FunctionPath functionPath = new FunctionPath();
        functionPath.function = "shortestPath";
        functionPath.from = from;
        functionPath.to = to;
        return functionPath;
    }

    public static FunctionPath shortestPath( String from, Direction direction, String to )
    {
        FunctionPath functionPath = new FunctionPath();
        functionPath.function = "shortestPath";
        functionPath.from = from;
        functionPath.direction = direction;
        functionPath.to = to;
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
        public String to;
        public Direction direction = Direction.ANY;
        public String name;
        public String relationship;
        public boolean optional;
        public Integer minHops;
        public Integer maxHops;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( direction.equals( Direction.INCOMING ) ? "<-" : "-" );

            if( name != null || relationship != null || optional || minHops != null || maxHops != null )
            {
                builder.append( '[' );
                if( name != null )
                {
                    builder.append( name );
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

            builder.append( direction.equals( Direction.OUTGOING ) ? "->" : "-" );

            builder.append( '(' ).append( to ).append( ')' );
        }

        public T direction( Direction direction )
        {
            this.direction = direction;
            return (T) this;
        }

        public T name( String name )
        {
            this.name = name;
            return (T) this;
        }

        public T relationship( String relationship )
        {
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
            this.minHops = minHops;
            this.maxHops = maxHops;
            return (T) this;
        }
    }

    public static class Path
        extends AbstractPath<Path>
    {
        public String from;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( '(' ).append( from ).append( ')' );
            super.asString( builder );
        }

        public MultiPath path( String to )
        {
            MultiPath path = new MultiPath();
            path.leftPath = this;
            path.to = to;

            return path;
        }

        public MultiPath path( Direction direction, String to )
        {
            MultiPath path = new MultiPath();
            path.leftPath = this;
            path.direction = direction;
            path.to = to;

            return path;
        }

        public MultiPath path( Direction direction, String name, String relationship, String to )
        {
            MultiPath path = new MultiPath();
            path.leftPath = this;
            path.direction = direction;
            path.name = name;
            path.relationship = relationship;
            path.to = to;

            return path;
        }
    }

    public static class MultiPath
        extends AbstractPath<MultiPath>
    {
        public AbstractPath leftPath;

        @Override
        public void asString( StringBuilder builder )
        {
            leftPath.asString( builder );
            super.asString( builder );
        }

        public MultiPath path( String to )
        {
            MultiPath path = new MultiPath();
            path.leftPath = this;
            path.to = to;

            return path;
        }
    }

    public static class FunctionPath
        extends AbstractPath<FunctionPath>
    {
        public String function;
        public String from;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( function ).append( '(' );
            builder.append( '(' ).append( from ).append( ')' );
            super.asString( builder );
            builder.append( ')' );
        }
    }

    public static class NamedPath
        extends MatchExpression
    {
        public String name;
        public AbstractPath path;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( name ).append( '=' );
            path.asString( builder );
        }
    }
}
