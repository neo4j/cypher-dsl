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

import org.neo4j.cypherdsl.ast.OrderBySet;
import org.neo4j.cypherdsl.ast.Query;
import org.neo4j.cypherdsl.ast.ReturnSet;
import org.neo4j.cypherdsl.ast.StartSet;

/**
 * TODO
 */
public class CypherQuery
    implements StartNext, ReturnNext, Match, OrderBy, Skip, Limit, Execute
{
    public static Start newQuery()
    {
        return new CypherQuery();
    }

    private Query query;

    private CypherQuery()
    {
        query = new Query();
    }

    // Start --------------------------------------------------------
    @Override
    public StartNext nodes( String name, int... id )
    {
        checkEmpty( name, "Name" );

        for( int i : id )
        {
            if (i < 0)
                throw new IllegalArgumentException( "Id may not be below zero" );
        }

        StartSet.StartNodes startNodes = new StartSet.StartNodes();
        startNodes.name = name;
        startNodes.nodes = id;
        this.query.startSets.add( startNodes );

        return this;
    }

    @Override
    public StartNext nodes( String name, String... parameters )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameters, "Parameters" );

        StartSet.StartNodesParameters startNodes = new StartSet.StartNodesParameters();
        startNodes.name = name;
        startNodes.parameters = parameters;
        this.query.startSets.add( startNodes );

        return this;
    }

    @Override
    public StartNext nodesLookup( String name, String indexName, String key, String value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        StartSet.StartNodesLookup startNodesLookup = new StartSet.StartNodesLookup();
        startNodesLookup.name = name;
        startNodesLookup.index = indexName;
        startNodesLookup.key = key;
        startNodesLookup.value = value;
        this.query.startSets.add( startNodesLookup );
        return this;
    }

    @Override
    public StartNext nodesQuery( String name, String indexName, String query )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( query, "Query" );

        StartSet.StartNodesQuery startNodesQuery = new StartSet.StartNodesQuery();
        startNodesQuery.name = name;
        startNodesQuery.index = indexName;
        startNodesQuery.query = query;
        this.query.startSets.add( startNodesQuery );
        return this;
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

        StartSet.StartRelationships startRelationships = new StartSet.StartRelationships();
        startRelationships.name = name;
        startRelationships.relationships = id;
        query.startSets.add( startRelationships );

        return this;
    }

    @Override
    public StartNext relationships( String name, String... parameters )
    {
        checkEmpty( name, "Name" );
        checkEmpty( parameters, "Parameters" );

        StartSet.StartRelationshipsParameters startRelationships = new StartSet.StartRelationshipsParameters();
        startRelationships.name = name;
        startRelationships.parameters = parameters;
        query.startSets.add( startRelationships );

        return this;
    }

    @Override
    public StartNext relationshipsLookup( String name, String indexName, String key, String value )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkEmpty( key, "Key" );
        checkEmpty( value, "Value" );

        StartSet.StartRelationshipsIndex startRelationshipsIndex = new StartSet.StartRelationshipsIndex();
        startRelationshipsIndex.name = name;
        startRelationshipsIndex.index = indexName;
        startRelationshipsIndex.key = key;
        startRelationshipsIndex.value = value;
        query.startSets.add( startRelationshipsIndex );
        return this;
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
    public ReturnNext returnNodes( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnSet.ReturnNodes returnNodes = new ReturnSet.ReturnNodes();
        returnNodes.names = names;
        query.returnSets.add( returnNodes );

        return this;
    }

    @Override
    public ReturnNext returnNodes( boolean distinct, String... names )
    {
        checkEmpty( names, "Names" );

        ReturnSet.ReturnNodes returnNodes = new ReturnSet.ReturnNodes();
        returnNodes.names = names;
        returnNodes.distinct = distinct;
        query.returnSets.add( returnNodes );

        return this;
    }

    @Override
    public ReturnNext returnRelationships( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnSet.ReturnRelationships returnRelationships = new ReturnSet.ReturnRelationships();
        returnRelationships.names = names;
        query.returnSets.add( returnRelationships );

        return this;
    }

    @Override
    public ReturnNext returnPaths( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnSet.ReturnPaths returnPaths = new ReturnSet.ReturnPaths();
        returnPaths.names = names;
        query.returnSets.add( returnPaths );

        return this;
    }

    @Override
    public ReturnNext returnProperties( String... names )
    {
        checkEmpty( names, "Names" );
        ReturnSet.ReturnProperties returnProperties = new ReturnSet.ReturnProperties();
        returnProperties.names = names;
        query.returnSets.add( returnProperties );

        return this;
    }

    @Override
    public ReturnNext returnProperties( boolean optional, String... names )
    {
        checkEmpty( names, "Names" );
        ReturnSet.ReturnProperties returnProperties = new ReturnSet.ReturnProperties();
        returnProperties.names = names;
        returnProperties.optional = optional;
        query.returnSets.add( returnProperties );

        return this;
    }

    @Override
    public ReturnNext returnLength( String name )
    {
        checkEmpty( name, "Name" );

        ReturnSet.ReturnFunction returnFunction = new ReturnSet.ReturnFunction();
        returnFunction.function = "length";
        returnFunction.name = name;
        query.returnSets.add( returnFunction );

        return this;
    }

    // OrderBy ------------------------------------------------------
    @Override
    public OrderBy orderBy( String name )
    {
        OrderBySet orderBy = new OrderBySet();
        orderBy.name = name;
        query.orderBySets.add( orderBy );

        return this;
    }

    @Override
    public OrderBy orderBy( String name, Order order )
    {
        OrderBySet orderBy = new OrderBySet();
        orderBy.name = name;
        orderBy.order = order;
        query.orderBySets.add( orderBy );

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

    private boolean isEmpty(String string)
    {
        return string == null || string.length() == 0;
    }

    private void checkEmpty(String string, String name)
    {
        if (isEmpty( string ))
            throw new IllegalArgumentException( name+" may not be null or empty string" );
    }

    private void checkEmpty(String[] strings, String name)
    {
        for( String string : strings )
        {
            if (isEmpty( string ))
                throw new IllegalArgumentException( name+" may not be null or empty string" );
        }
    }
}
