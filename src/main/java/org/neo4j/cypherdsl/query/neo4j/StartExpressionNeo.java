/**
 * Copyright (c) 2002-2012 "Neo Technology,"
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
package org.neo4j.cypherdsl.query.neo4j;

import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.query.Query;
import org.neo4j.cypherdsl.query.StartExpression;
import org.neo4j.graphdb.Node;

import static org.neo4j.cypherdsl.query.Query.checkEmpty;

/**
 * TODO
 */
public abstract class StartExpressionNeo
{
    public static StartExpression.StartNodes node( String name, Node... nodes )
    {
        checkEmpty( name, "Name" );

        for( Node node : nodes )
        {
            Query.checkNull( node, "Node" );
        }

        long[] ids = new long[nodes.length];
        for( int i = 0; i < nodes.length; i++ )
        {
            Node node = nodes[ i ];
            ids[i] = node.getId();
        }

        StartExpression.StartNodes startNodes = new StartExpression.StartNodes();
        startNodes.name = name;
        startNodes.nodes = CypherQuery.literals( ids );
        return startNodes;
    }

}
