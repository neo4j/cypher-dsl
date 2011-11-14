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

package org.neo4j.cypherdsl.querydsl;

import com.mysema.query.types.Path;
import org.neo4j.cypherdsl.query.AsString;
import org.neo4j.cypherdsl.query.MatchExpression;
import org.neo4j.cypherdsl.query.Query;

import java.io.Serializable;

/**
 * Provides the possible expressions for the MATCH clause.
 */
public abstract class QueryDSLMatchExpression
{
    public static QueryDSLPath path()
    {
        return new QueryDSLPath();
    }

    public static QueryDSLPath path(String name)
    {
        QueryDSLPath path = new QueryDSLPath();
        path.pathName = name;
        return path;
    }

    /**
     * Use this to invoke the shortestPath function
     *
     * @param name
     * @return
     */
    public static QueryDSLFunctionPath shortestPath( String name )
    {
        Query.checkNull( name, "Name" );

        QueryDSLFunctionPath functionPath = new QueryDSLFunctionPath();
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

    public static class QueryDSLPath
        extends MatchExpression.Path<QueryDSLPath>
    {
        public QueryDSLPath from(String from)
        {
            return super.from(from);
        }

        public QueryDSLPath from(com.mysema.query.types.Path<?> path)
        {
            return from(path.toString());
        }

        public QueryDSLPath to(com.mysema.query.types.Path<?> to)
        {
            return super.to(to.toString());
        }

        @Override
        public QueryDSLLink link()
        {
            QueryDSLLink link = new QueryDSLLink();
            link.leftPath = this;

            return link;
        }
    }

    public static class QueryDSLLink
        extends MatchExpression.Link<QueryDSLLink>
    {
        public QueryDSLLink to(com.mysema.query.types.Path<?> to)
        {
            return super.to(to.toString());
        }

        @Override
        public QueryDSLLink link()
        {
            QueryDSLLink link = new QueryDSLLink();
            link.leftPath = this;

            return link;
        }
    }

    public static class QueryDSLFunctionPath
        extends MatchExpression.FunctionPath<QueryDSLFunctionPath>
    {
        public QueryDSLFunctionPath from(com.mysema.query.types.Path<?> from)
        {
            return super.from(from.toString());
        }

        public QueryDSLFunctionPath to(com.mysema.query.types.Path<?> to)
        {
            return super.to(to.toString());
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
