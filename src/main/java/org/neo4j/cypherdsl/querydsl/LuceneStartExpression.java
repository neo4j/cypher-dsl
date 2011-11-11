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
import com.mysema.query.types.Predicate;
import org.neo4j.cypherdsl.query.StartExpression;

import static org.neo4j.cypherdsl.query.Query.checkEmpty;
import static org.neo4j.cypherdsl.query.Query.checkNull;

/**
 * Create Lucene query lookup expressions using QueryDSL predicates
 */
public class LuceneStartExpression
    extends StartExpression.StartNodesQuery
{
    private static final LuceneSerializer luceneSerializer = new LuceneSerializer(true, true);

    public static StartNodesQuery query( String name, String indexName, Predicate query )
    {
        checkEmpty( name, "Name" );
        checkEmpty( indexName, "Index" );
        checkNull(query, "Query");

        StartExpression.StartNodesQuery startNodesQuery = new StartExpression.StartNodesQuery();
        startNodesQuery.name = name;
        startNodesQuery.index = indexName;

        startNodesQuery.query = luceneSerializer.toQuery(query, null).toString();
        return startNodesQuery;
    }

}
