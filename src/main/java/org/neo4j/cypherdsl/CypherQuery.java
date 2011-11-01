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
package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.query.MatchExpression;
import org.neo4j.cypherdsl.query.OrderByExpression;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.cypherdsl.query.ReturnExpression;
import org.neo4j.cypherdsl.query.StartExpression;
import org.neo4j.cypherdsl.query.WhereExpression;

import static org.neo4j.cypherdsl.query.Query.checkEmpty;

/**
 * TODO
 */
public class CypherQuery
    implements StartNext, ReturnNext, Match, OrderBy, Skip, Limit, Execute
{
    private Query query;

    public static Start newQuery()
    {
        return new CypherQuery( );
    }

    public static Where newWhereQuery(Query query)
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
        query.startExpressions.add( startExpression );

        return this;
    }

    @Override
    public StartNext nodes( String name, int... id )
    {
        checkEmpty( name, "Name" );

        for( int i : id )
        {
            if (i < 0)
                throw new IllegalArgumentException( "Id may not be below zero" );
        }

        StartExpression.StartNodes startNodes = new StartExpression.StartNodes();
        startNodes.name = name;
        startNodes.nodes = id;
        return start( startNodes );
    }

    @Override
    public StartNext nodes( String name, String... parameters )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameters, "Parameters" );

        StartExpression.StartNodesParameters startNodes = new StartExpression.StartNodesParameters();
        startNodes.name = name;
        startNodes.parameters = parameters;
        return start( startNodes );
    }

    @Override
    public StartNext nodesLookup( String name, String indexName, String key, String value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        StartExpression.StartNodesLookup startNodesLookup = new StartExpression.StartNodesLookup();
        startNodesLookup.name = name;
        startNodesLookup.index = indexName;
        startNodesLookup.key = key;
        startNodesLookup.value = value;
        return start( startNodesLookup );
    }

    @Override
    public StartNext nodesQuery( String name, String indexName, String query )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( query, "Query" );

        StartExpression.StartNodesQuery startNodesQuery = new StartExpression.StartNodesQuery();
        startNodesQuery.name = name;
        startNodesQuery.index = indexName;
        startNodesQuery.query = query;
        return start( startNodesQuery );
    }

    @Override
    public StartNext relationships( String name, int... id )
    {
        checkEmpty( name, "Name" );

        for( int i : id )
        {
            if (i < 0)
                throw new IllegalArgumentException( "Id may not be below zero" );
        }

        StartExpression.StartRelationships startRelationships = new StartExpression.StartRelationships();
        startRelationships.name = name;
        startRelationships.relationships = id;
        return start( startRelationships );
    }

    @Override
    public StartNext relationships( String name, String... parameters )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameters, "Parameters" );

        StartExpression.StartRelationshipsParameters startRelationships = new StartExpression.StartRelationshipsParameters();
        startRelationships.name = name;
        startRelationships.parameters = parameters;
        return start( startRelationships );
    }

    @Override
    public StartNext relationshipsLookup( String name, String indexName, String key, String value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        StartExpression.StartRelationshipsIndex startRelationshipsIndex = new StartExpression.StartRelationshipsIndex();
        startRelationshipsIndex.name = name;
        startRelationshipsIndex.index = indexName;
        startRelationshipsIndex.key = key;
        startRelationshipsIndex.value = value;
        return start( startRelationshipsIndex );
    }

    // Match --------------------------------------------------------
    @Override
    public Match match( MatchExpression expression )
    {
        query.matchExpressions.add( expression );
        return this;
    }

    // Where --------------------------------------------------------
    @Override
    public Return where( WhereExpression expression )
    {
        query.whereExpression = expression;
        return this;
    }

    // Return -------------------------------------------------------
    @Override
    public ReturnNext returnExpr( ReturnExpression returnExpression )
    {
        query.returnExpressions.add( returnExpression );
        return this;
    }

    @Override
    public ReturnNext returnNodes( String... names )
    {
        query.returnExpressions.add( ReturnExpression.nodes( names ) );
        return this;
    }

    @Override
    public ReturnNext returnNodes( boolean distinct, String... names )
    {
        checkEmpty( names, "Names" );

        ReturnExpression.ReturnNodes returnNodes = new ReturnExpression.ReturnNodes();
        returnNodes.names = names;
        returnNodes.distinct = distinct;
        query.returnExpressions.add( returnNodes );

        return this;
    }

    @Override
    public ReturnNext returnRelationships( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnExpression.ReturnRelationships returnRelationships = new ReturnExpression.ReturnRelationships();
        returnRelationships.names = names;
        query.returnExpressions.add( returnRelationships );

        return this;
    }

    @Override
    public ReturnNext returnPaths( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnExpression.ReturnPaths returnPaths = new ReturnExpression.ReturnPaths();
        returnPaths.names = names;
        query.returnExpressions.add( returnPaths );

        return this;
    }

    @Override
    public ReturnNext returnProperties( String... names )
    {
        query.returnExpressions.add( ReturnExpression.properties( names ) );
        return this;
    }

    @Override
    public ReturnNext returnProperties( boolean optional, String... names )
    {
        checkEmpty( names, "Names" );
        ReturnExpression.ReturnProperties returnProperties = new ReturnExpression.ReturnProperties();
        returnProperties.names = names;
        returnProperties.optional = optional;
        query.returnExpressions.add( returnProperties );

        return this;
    }

    @Override
    public ReturnNext count()
    {
        return returnExpr(ReturnExpression.count());
    }

    @Override
    public ReturnNext count( String name )
    {
        return returnExpr(ReturnExpression.count(name));
    }

    @Override
    public ReturnNext sum( String name )
    {
        return returnExpr(ReturnExpression.sum(name));
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
        query.orderByExpressions.add( orderByExpression );
        return this;
    }

    @Override
    public OrderBy orderBy( String name )
    {
        OrderByExpression orderBy = new OrderByExpression();
        orderBy.name = name;
        return orderBy( orderBy );
    }

    @Override
    public OrderBy orderBy( String name, Order order )
    {
        OrderByExpression orderBy = new OrderByExpression();
        orderBy.name = name;
        orderBy.order = order;
        return orderBy( orderBy );
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
