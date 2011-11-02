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
        ANY,
        OUT,
        IN
    }

    /**
     * Use this to create a named path.
     *
     * Example: named("p",path("a","b")) -> p=(a)--(b)
     *
     * @param name
     * @param path
     * @return
     */
    public static NamedPath named( String name, AbstractPath path )
    {
        Query.checkEmpty( name, "Name" );
        if (path == null)
            throw new IllegalArgumentException( "Path may not be null" );

        NamedPath namedPath = new NamedPath();
        namedPath.name = name;
        namedPath.path = path;
        return namedPath;
    }

    public static Path pathX(String name)
    {
        Path path = new Path();
        path.name = name;
        return path;
    }

    /**
     * Use this to create a basic path. Use the methods on the returned
     * Path to further modify the path if necessary.
     *
     * Example: path("a","b") -> (a)--(b)
     *
     * @param from
     * @param to
     * @return
     */
    public static Path path( String from,
                             String to
    )
    {
        Query.checkNull( from, "From" );
        Query.checkNull( to, "To" );

        Path path = new Path();
        path.from = from;
        path.to = to;

        return path;
    }

    /**
     * Use this to create a path with a given direction. Use the methods
     * on the returned Path to further modify the path if necessary.
     *
     * Example: path("a",OUT,"b") -> (a)-->(b)
     *
     * @param from
     * @param direction
     * @param to
     * @return
     */
    public static Path path( String from,
                             Direction direction,
                             String to
    )
    {
        Query.checkNull( from, "From" );
        if (direction == null)
            throw new IllegalArgumentException( "Direction may not be null" );
        Query.checkNull( to, "To" );

        Path path = new Path();
        path.from = from;
        path.to = to;
        path.direction = direction;

        return path;
    }

    /**
     * Use this to create a path with almost all information provided.
     * Use the methods on the returned Path to further modify the path if necessary
     *
     * Example: path("a",OUT,"r","b") -> (a)-[r]->(b)
     *
     * @param from
     * @param direction
     * @param name
     * @param to
     * @return
     */
    public static Path path( String from,
                             Direction direction,
                             String name,
                             String to
    )
    {
        return path( from, direction, name, (String) null, to );
    }

    /**
     * Use this to create a path with almost all information provided.
     * Use the methods on the returned Path to further modify the path if necessary
     *
     * Example: path("a",OUT,"r",KNOWS,"b") -> (a)-[r:KNOWS]->(b)
     *
     * @param from
     * @param direction
     * @param name
     * @param relationship
     * @param to
     * @return
     */
    public static Path path( String from,
                             Direction direction,
                             String name,
                             Enum relationship,
                             String to
    )
    {
        return path( from, direction, name, relationship == null ? null : relationship.name(), to );
    }

    /**
     * Use this to create a path with almost all information provided.
     * Use the methods on the returned Path to further modify the path if necessary
     *
     * Example: path("a",OUT,"r","KNOWS","b") -> (a)-[r:KNOWS]->(b)
     *
     * @param from
     * @param direction
     * @param name
     * @param relationShip
     * @param to
     * @return
     */
    public static Path path( String from,
                             Direction direction,
                             String name,
                             String relationShip,
                             String to
    )
    {
        Query.checkNull( from, "From" );
        Query.checkNull( to, "To" );

        Path path = new Path();
        path.from = from;
        path.name = name;
        path.relationship = relationShip;
        path.to = to;
        path.direction = direction;

        return path;
    }

    /**
     * Use this to create a path with all information provided.
     *
     * Example: path("a",OUT,"r",true, "KNOWS",1,3,"b") -> (a)-[r?:KNOWS*1..3]->(b)
     *
     * @param from
     * @param direction
     * @param name
     * @param optional
     * @param relationship
     * @param minHops
     * @param maxHops
     * @param to
     * @return
     */
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
        Query.checkNull( from, "From" );
        Query.checkNull( to, "To" );

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

    /**
     * Use this to invoke the shortestPath function
     *
     * @param from
     * @param to
     * @return
     */
    public static FunctionPath shortestPath( String from, String to )
    {
        Query.checkNull( from, "From" );
        Query.checkNull( to, "To" );

        FunctionPath functionPath = new FunctionPath();
        functionPath.function = "shortestPath";
        functionPath.from = from;
        functionPath.to = to;
        return functionPath;
    }

    public static FunctionPath shortestPath( String from, Direction direction, String to )
    {
        Query.checkNull( from, "From" );
        Query.checkNull( to, "To" );
        if (direction == null)
            throw new IllegalArgumentException( "Direction may not be null" );

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
            builder.append( direction.equals( Direction.IN ) ? "<-" : "-" );

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

            builder.append( direction.equals( Direction.OUT ) ? "->" : "-" );

            builder.append( '(' ).append( to ).append( ')' );
        }

        public T direction( Direction direction )
        {
            Query.checkNull( direction, "Direction" );
            this.direction = direction;
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

        public T any(String relationship)
        {
            this.direction = Direction.ANY;
            this.relationship = relationship;
            return (T) this;
        }

        public T name( String name )
        {
            Query.checkEmpty( name, "Name" );
            this.name = name;
            return (T) this;
        }

        public T relationship( Enum relationship )
        {
            Query.checkNull( relationship, "Relationship" );
            this.relationship = relationship.name();
            return (T) this;
        }

        public T relationship( String relationship )
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
            Query.checkNull(to, "To");

            MultiPath path = new MultiPath();
            path.leftPath = this;
            path.to = to;

            return path;
        }

        public MultiPath path( Direction direction, String to )
        {
            Query.checkNull(to, "To");

            MultiPath path = new MultiPath();
            path.leftPath = this;
            path.direction = direction;
            path.to = to;

            return path;
        }

        public MultiPath path( Direction direction, String name, Enum relationship, String to )
        {
            return path( direction, name, relationship == null ? null : relationship.name(), to );
        }

        public MultiPath path( Direction direction, String name, String relationship, String to )
        {
            Query.checkNull(to, "To");

            MultiPath path = new MultiPath();
            path.leftPath = this;
            path.direction = direction;
            path.name = name;
            path.relationship = relationship;
            path.to = to;

            return path;
        }

        public Path from(String from)
        {
            this.from = from;
            return this;
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
            Query.checkNull(to, "To");

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
