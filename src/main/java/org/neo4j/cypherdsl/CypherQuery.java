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
    implements StartNext, ReturnNext, Match, OrderBy, Skip, Limit, Execute
{
    private Query query;

    public static Start newQuery()
    {
        return new CypherQuery( );
    }

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
    @Override
    public StartNext start( StartExpression startExpression )
    {
        Query.checkNull( startExpression, "Expression" );
        query.startExpressions.add( startExpression );

        return this;
    }

    @Override
    public StartNext nodes( String name, int... id )
    {
        return start( StartExpression.nodes( name, id ) );
    }

    @Override
    public StartNext nodes( String name, String... parameters )
    {
        return start( StartExpression.nodes( name, parameters ) );
    }

    @Override
    public StartNext nodesLookup( String name, String indexName, String key, String value )
    {
        return start( StartExpression.nodesLookup( name, indexName, key, value ) );
    }

    @Override
    public StartNext nodesQuery( String name, String indexName, String query )
    {
        return start( StartExpression.nodesQuery( name, indexName, query ) );
    }

    @Override
    public StartNext relationships( String name, int... id )
    {
        return start( StartExpression.relationships( name, id ) );
    }

    @Override
    public StartNext relationships( String name, String... parameters )
    {
        return start( StartExpression.relationships( name, parameters ) );
    }

    @Override
    public StartNext relationshipsLookup( String name, String indexName, String key, String value )
    {
        return start( StartExpression.relationshipsLookup( name, indexName, key, value ) );
    }

    // Match --------------------------------------------------------
    @Override
    public Match match( MatchExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        query.matchExpressions.add( expression );
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
    public ReturnNext returnExpr( ReturnExpression returnExpression )
    {
        Query.checkNull( returnExpression, "Expression" );
        query.returnExpressions.add( returnExpression );
        return this;
    }

    @Override
    public ReturnNext returnNode( String... names )
    {
        return returnExpr( ReturnExpression.node( names ) );
    }

    @Override
    public ReturnNext returnRelationship( String... names )
    {
        return returnExpr( ReturnExpression.relationship( names ) );
    }

    @Override
    public ReturnNext returnPath( String... names )
    {
        return returnExpr( ReturnExpression.path( names ) );
    }

    @Override
    public ReturnNext returnProperty( String... names )
    {
        return returnExpr( ReturnExpression.property( names ) );
    }

    @Override
    public ReturnNext count()
    {
        return returnExpr(ReturnExpression.count());
    }

    @Override
    public ReturnNext count( String name )
    {
        return returnExpr(ReturnExpression.count( name ));
    }

    @Override
    public ReturnNext sum( String name )
    {
        return returnExpr(ReturnExpression.sum( name ));
    }

    @Override
    public ReturnNext avg( String name )
    {
        return returnExpr(ReturnExpression.avg( name ));
    }

    @Override
    public ReturnNext max( String name )
    {
        return returnExpr(ReturnExpression.max( name ));
    }

    @Override
    public ReturnNext min( String name )
    {
        return returnExpr( ReturnExpression.min( name ) );
    }

    @Override
    public ReturnNext collect( String name )
    {
        return returnExpr(ReturnExpression.collect( name ));
    }

    // OrderBy ------------------------------------------------------
    @Override
    public OrderBy orderBy( OrderByExpression orderByExpression )
    {
        Query.checkNull( orderByExpression, "Expression" );
        query.orderByExpressions.add( orderByExpression );
        return this;
    }

    @Override
    public OrderBy orderBy( String name )
    {
        return orderBy( OrderByExpression.orderBy( name ) );
    }

    @Override
    public OrderBy orderBy( String name, Order order )
    {
        return orderBy( OrderByExpression.orderBy( name ).order( order ) );
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
