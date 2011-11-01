/**
 * Licensed to Neo Technology under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Neo Technology licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.neo4j.cypherdsl.query;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Model for a Cypher Query. The model is serializable and cloneable, to make it easy to
 * save on disk or transfer over the wire. Being cloneable also helps with query builder continuation.
 */
public class Query
    implements AsString, Serializable, Cloneable
{
    public static boolean isEmpty(String string)
    {
        return string == null || string.length() == 0;
    }

    public static void checkNull(Object object, String name)
    {
        if (object.getClass().isArray())
        {
            Object[] array = (Object[]) object;
            for( Object obj : array )
            {
                if (obj == null)
                    throw new IllegalArgumentException( name+" may not be null" );
            }
        } else
            if (object == null)
                throw new IllegalArgumentException( name+" may not be null" );
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
