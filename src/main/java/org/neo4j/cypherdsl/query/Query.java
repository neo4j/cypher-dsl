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
import org.neo4j.graphdb.factory.GraphDatabaseSettings;

/**
 * Model for a Cypher Query. The model is serializable and cloneable, to make it easy to
 * save on disk or transfer over the wire. Being cloneable also helps with query builder continuation.
 */
public class Query
        implements AsString, Serializable, Cloneable
{
    
    private static final String QUERY_PREFIX = "CYPHER ";
    private static final String DEFAULT_CYPHER_VERSION = "2.3";
    
    public static boolean isEmpty( String string )
    {
        return string == null || string.length() == 0;
    }

    public static void checkNull( Object object, String name )
    {
        if ( object == null )
        {
            throw new IllegalArgumentException( name + " may not be null" );
        }
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

    private final ArrayList<Clause> clauses;

    public void add( Clause clause )
    {
        // Check if we should merge to consecutive WHERE clauses
        if ( !clauses.isEmpty() && clause instanceof WhereClause )
        {
            WhereClause previousWhere = lastClause(WhereClause.class);
            if ( previousWhere != null )
            {
                previousWhere.mergeWith( (WhereClause) clause );
                return;
            }
        }

        clauses.add( clause );
    }

    public  <T extends Clause> T lastClause(Class<T> type) {
        Clause clause = clauses.get(clauses.size() - 1);
        return type.isInstance(clause) ? type.cast(clause) : null;
    }

    public void asString( StringBuilder builder )
    {
        asString(builder, DEFAULT_CYPHER_VERSION);
    }
    
    public void asString( StringBuilder builder, String cypherVersion )
    {
        builder.append( QUERY_PREFIX );
        builder.append( cypherVersion );

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

    public Query()
    {
        this( new ArrayList<Clause>() );
    }

    private Query( ArrayList<Clause> clauses )
    {
        this.clauses = clauses;
    }

    @Override
    public Object clone()
            throws CloneNotSupportedException
    {
        return new Query( (ArrayList<Clause>) clauses.clone() );
    }

    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder();
        asString( builder );
        return builder.toString();
    }
}
