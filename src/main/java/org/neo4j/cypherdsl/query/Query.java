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
import java.util.ArrayList;
import java.util.List;

/**
 * TODO
 */
public class Query
    implements AsString, Serializable, Cloneable
{
    public static boolean isEmpty(String string)
    {
        return string == null || string.length() == 0;
    }

    public static void checkEmpty(String string, String name)
    {
        if (isEmpty( string ))
            throw new IllegalArgumentException( name+" may not be null or empty string" );
    }

    public static void checkEmpty(String[] strings, String name)
    {
        for( String string : strings )
        {
            if (isEmpty( string ))
                throw new IllegalArgumentException( name+" may not be null or empty string" );
        }
    }

    public ArrayList<StartExpression> startExpressions = new ArrayList<StartExpression>();
    public ArrayList<MatchExpression> matchExpressions = new ArrayList<MatchExpression>();
    public WhereExpression whereExpression;
    public ArrayList<ReturnExpression> returnExpressions = new ArrayList<ReturnExpression>();
    public ArrayList<OrderByExpression> orderByExpressions = new ArrayList<OrderByExpression>();
    public Integer skip;
    public Integer limit;

    public void asString(StringBuilder builder)
    {
        clause( builder, "START", startExpressions );
        clause( builder, "MATCH", matchExpressions );

        if (whereExpression != null)
        {
            builder.append( " WHERE " );
            whereExpression.asString( builder );
        }

        clause( builder, "RETURN", returnExpressions );
        clause( builder, "ORDER BY", orderByExpressions );

        if (skip != null)
            builder.append( " SKIP " ).append( skip );

        if (limit != null)
            builder.append( " LIMIT " ).append( limit );
    }

    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        Query query = (Query) super.clone();
        query.startExpressions = (ArrayList<StartExpression>) query.startExpressions.clone();
        query.matchExpressions = (ArrayList<MatchExpression>) query.matchExpressions.clone();
        if (query.whereExpression != null)
            query.whereExpression = (WhereExpression) query.whereExpression.clone();
        query.returnExpressions = (ArrayList<ReturnExpression>) query.returnExpressions.clone();
        query.orderByExpressions = (ArrayList<OrderByExpression>) query.orderByExpressions.clone();
        return query;
    }

    private void clause( StringBuilder builder, String name, List<? extends AsString> asStringList )
    {
        if (!asStringList.isEmpty())
        {
            if (builder.length() > 0)
                builder.append( ' ' );
            builder.append( name ).append( ' ' );

            for( int i = 0; i < asStringList.size(); i++ )
            {
                AsString asString = asStringList.get( i );
                if (i > 0)
                    builder.append( ',' );
                asString.asString( builder );
            }
        }
    }

    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder( );
        asString( builder );
        return builder.toString();
    }
}
