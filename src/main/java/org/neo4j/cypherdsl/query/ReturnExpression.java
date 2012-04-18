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
 * Provides the possible expressions for the RETURN clause.
 */
public class ReturnExpression<T extends ReturnExpression>
    extends Expression
{
    public Expression expression;
    public boolean distinct;
    public String as;

    public T distinct()
    {
        distinct = true;
        return (T) this;
    }

    public T as(String name)
    {
        as = name;
        return (T) this;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        if (distinct)
            builder.append( "DISTINCT " );

        expression.asString( builder );

        if (as != null)
            builder.append( " AS " ).append( as );
    }

    public static class ReturnAggregate
        extends ReturnExpression<ReturnAggregate>
    {
        public String function;
        public String name;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( function ).append( '(' );
            if (distinct)
                builder.append( "DISTINCT " );
            if (name == null)
                builder.append( "*" );
            else
            {
                builder.append( name );
            }
            builder.append( ')' );
            if (as != null)
                builder.append(" AS ").append(as);
        }
    }
}
