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
import org.neo4j.cypherdsl.CypherQuery;

/**
 * Provides the possible expressions for the MATCH clause.
 */
public abstract class MatchExpression
    implements AsString, Serializable, Cloneable
{
    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        return super.clone();
    }

    public static class FunctionPath<T extends FunctionPath>
        extends MatchExpression
    {
        public Identifier pathName;
        public String function;
        public Identifier from;
        public Identifier to;
        public Direction direction = Direction.BOTH;
        public Integer minHops;
        public Integer maxHops;

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

        public T from(String from)
        {
            return from(CypherQuery.identifier( from ));
        }

        public T from(Identifier from)
        {
            Query.checkNull( from, "From" );
            this.from = from;
            return (T) this;
        }

        public T to(String to)
        {
            return to(CypherQuery.identifier( to ));
        }

        public T to(Identifier to)
        {
            Query.checkNull( to, "To" );
            this.to = to;
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

        @Override
        public void asString( StringBuilder builder )
        {
            pathName.asString( builder );
            builder.append( "=" );
            builder.append( function ).append( '(' );
            builder.append( '(' );
            if (from != null)
                from.asString( builder );
            builder.append( ')' );

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

            builder.append( '(' );
            if (to != null)
                to.asString( builder );
            builder.append( ')' );
            builder.append( ')' );
        }
    }
}
