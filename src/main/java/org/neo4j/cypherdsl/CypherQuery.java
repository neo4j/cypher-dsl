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
package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.query.MatchExpression;
import org.neo4j.cypherdsl.query.OrderByExpression;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.cypherdsl.query.ReturnExpression;
import org.neo4j.cypherdsl.query.StartExpression;
import org.neo4j.cypherdsl.query.WhereExpression;

/**
 * DSL for creating Cypher queries. Once created you can serialize to a string,
 * or retrieve the internal Query model for further processing.
 *
 * It is possible to iteratively construct Cypher queries by calling toQuery()
 * and then use the Query as continuation point. When a new CypherQuery is created
 * by using the newQuery() method the Query is cloned, so that the original Query
 * is not modified. This can be used effectively to create a base query which can then
 * be used many times. Typical examples would be to create a query which is reused many times
 * for paging purposes, and also to provide a base query using START and MATCH which is then
 * expanded using WHERE and RETURN clauses.
 */
public class CypherQuery
    implements Match, ReturnNext, OrderBy, Skip, Limit, Execute
{
    private Query query;

    public static Match start(StartExpression... startExpressions)
    {
        CypherQuery query = new CypherQuery(  );
        return query.startX( startExpressions );
    }

/*
    public static Start newQuery()
    {
        return new CypherQuery( );
    }
*/

    public static CypherQuery newQuery( Query query )
    {
        return new CypherQuery( query);
    }

    private CypherQuery()
    {
        query = new Query();
    }

    private CypherQuery(Query query)
    {
        try
        {
            this.query = (Query) query.clone();
        }
        catch( CloneNotSupportedException e )
        {
            throw new IllegalStateException( "Query was not cloneable" );
        }
    }

    // Start --------------------------------------------------------
    public Match startX( StartExpression... startExpression )
    {
        for( StartExpression expression : startExpression )
        {
            query.startExpressions.add( expression );
        }

        return this;
    }

    // Match --------------------------------------------------------
    @Override
    public Match match( MatchExpression... expression )
    {
        for( MatchExpression matchExpression : expression )
        {
            query.matchExpressions.add( matchExpression );
        }
        return this;
    }

    // Where --------------------------------------------------------
    @Override
    public Return where( WhereExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        query.whereExpression = expression;
        return this;
    }

    // Return -------------------------------------------------------
    @Override
    public ReturnNext returns( ReturnExpression... returnExpression )
    {
        for( ReturnExpression expression : returnExpression )
        {
            query.returnExpressions.add( expression );
        }
        return this;
    }

    // OrderBy ------------------------------------------------------
    @Override
    public OrderBy orderBy( OrderByExpression... orderByExpression )
    {
        for( OrderByExpression expression : orderByExpression )
        {
            query.orderByExpressions.add( expression );
        }
        return this;
    }

    // Skip ---------------------------------------------------------
    @Override
    public Limit skip( int skip )
    {
        if (skip < 0)
            throw new IllegalArgumentException( "Skip may not be below zero" );

        query.skip = skip;
        return this;
    }

    // Limit --------------------------------------------------------
    @Override
    public Execute limit( int limit)
    {
        if (limit < 0)
            throw new IllegalArgumentException( "Limit may not be below zero" );

        query.limit = limit;
        return this;
    }

    // Execute ------------------------------------------------------
    @Override
    public void asString(StringBuilder builder)
    {
        query.asString( builder );
    }

    @Override
    public Query toQuery()
    {
        return query;
    }

    @Override
    public String toString()
    {
        return query.toString();
    }
}
