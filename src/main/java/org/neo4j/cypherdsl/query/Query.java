/**
 * Copyright (c) 2002-2013 "Neo Technology,"
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

import org.neo4j.cypherdsl.AsString;
import org.neo4j.cypherdsl.Literal;
import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.query.clause.Clause;
import org.neo4j.cypherdsl.query.clause.WhereClause;

/**
 * Model for a Cypher Query. The model is serializable and cloneable, to make it easy to
 * save on disk or transfer over the wire. Being cloneable also helps with query builder continuation.
 */
public class Query
        implements AsString, Serializable, Cloneable
{
    public static boolean isEmpty( String string )
    {
        return string == null || string.length() == 0;
    }

    public static void checkNull( Object object, String name )
    {
        if ( object.getClass().isArray() )
        {
            Object[] array = (Object[]) object;
            for ( Object obj : array )
            {
                if ( obj == null )
                {
                    throw new IllegalArgumentException( name + " may not be null" );
                }
            }
        }
        else if ( object == null )
        {
            throw new IllegalArgumentException( name + " may not be null" );
        }
    }

    public static void checkEmpty( String string, String name )
    {
        if ( isEmpty( string ) )
        {
            throw new IllegalArgumentException( name + " may not be null or empty string" );
        }
    }

    public static void checkEmpty( Expression value, String name )
    {
        if ( value instanceof Literal && isEmpty( value.toString() ) )
        {
            throw new IllegalArgumentException( name + " may not be null or empty string" );
        }
    }

    public static void checkEmpty( String[] strings, String name )
    {
        for ( String string : strings )
        {
            if ( isEmpty( string ) )
            {
                throw new IllegalArgumentException( name + " may not be null or empty string" );
            }
        }
    }

    private ArrayList<Clause> clauses = new ArrayList<Clause>();

    public void add( Clause clause )
    {
        // Check if we should merge to consecutive WHERE clauses
        if ( !clauses.isEmpty() && clause instanceof WhereClause )
        {
            Clause previousClause = clauses.get( clauses.size() - 1 );
            if ( previousClause instanceof WhereClause )
            {
                WhereClause previousWhere = (WhereClause) previousClause;
                previousWhere.mergeWith( (WhereClause) clause );
                return;
            }
        }

        clauses.add( clause );
    }

    public void asString( StringBuilder builder )
    {
        builder.append( "CYPHER 1.8" );

        for ( Clause clause : clauses )
        {
            clause.asString( builder );
        }

/*
        clause( builder, "START", startExpressions,"," );
        clause( builder, "MATCH", matchExpressions,"," );
        clause( builder, "WHERE", whereExpressions," AND " );

        clause( builder, "RETURN", returnExpressions,"," );
        clause( builder, "ORDER BY", orderByExpressions,"," );

        if (skip != null)
            builder.append( " SKIP " ).append( skip );

        if (limit != null)
            builder.append( " LIMIT " ).append( limit );
*/
    }

    @Override
    public Object clone()
            throws CloneNotSupportedException
    {
        Query query = (Query) super.clone();
        query.clauses = (ArrayList<Clause>) query.clauses.clone();
        return query;
    }

    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder();
        asString( builder );
        return builder.toString();
    }
}
