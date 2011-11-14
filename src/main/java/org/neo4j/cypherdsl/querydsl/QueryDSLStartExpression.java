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

import com.mysema.query.lucene.LuceneSerializer;
import com.mysema.query.types.EntityPath;
import com.mysema.query.types.Path;
import com.mysema.query.types.Predicate;
import org.neo4j.cypherdsl.query.Expression;
import org.neo4j.cypherdsl.query.StartExpression;

import static org.neo4j.cypherdsl.query.Query.checkEmpty;
import static org.neo4j.cypherdsl.query.Query.checkNull;

/**
 * Create QueryDSL based start expressions
 */
public class QueryDSLStartExpression
{
    private static final LuceneSerializer luceneSerializer = new LuceneSerializer(true, true);

    public static StartExpression.StartNodes node( Path<?> entity, long... id )
    {
        return StartExpression.node(entity.toString(), id);
    }

    public static StartExpression.StartNodes node( Path<?> entity, String parameter )
    {
        return StartExpression.node(entity.toString(), parameter);
    }

    public static StartExpression.StartNodesLookup lookup( Path<?> entity, String indexName, Path<?> key, String value )
    {
        return StartExpression.lookup(entity.toString(), indexName, key.getMetadata().getExpression().toString(), value);
    }

    public static StartExpression.StartNodesLookup lookup( Path<?> entity, String indexName, Expression.Identifier key, Expression.Value value )
    {
        return StartExpression.lookup(entity.toString(), indexName, key, value);
    }

    public static StartExpression.StartNodesQuery query( Path<?> entity, String indexName, Predicate query )
    {
        return query(entity.toString(), indexName, query);
    }

    public static StartExpression.StartNodesQuery query( String name, String indexName, Predicate query )
    {
        return StartExpression.query(name, indexName, luceneSerializer.toQuery(query, null).toString());
    }
}
