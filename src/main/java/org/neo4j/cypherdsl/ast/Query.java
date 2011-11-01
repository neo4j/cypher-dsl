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

package org.neo4j.cypherdsl.ast;

import java.util.ArrayList;
import java.util.List;
import org.neo4j.cypherdsl.MatchExpression;
import org.neo4j.cypherdsl.WhereExpression;

/**
 * TODO
 */
public class Query
    implements AsString
{
    public List<StartSet> startSets = new ArrayList<StartSet>();
    public List<MatchExpression> matchExpressions = new ArrayList<MatchExpression>();
    public WhereExpression whereExpression;
    public List<ReturnSet> returnSets = new ArrayList<ReturnSet>();
    public List<OrderBySet> orderBySets = new ArrayList<OrderBySet>();
    public Integer skip;
    public Integer limit;

    public void asString(StringBuilder builder)
    {
        clause( builder, "START", startSets );
        clause( builder, "MATCH", matchExpressions );

        if (whereExpression != null)
        {
            builder.append( " WHERE " );
            whereExpression.asString( builder );
        }

        clause( builder, "RETURN", returnSets );
        clause( builder, "ORDER BY", orderBySets );

        if (skip != null)
            builder.append( " SKIP " ).append( skip );

        if (limit != null)
            builder.append( " LIMIT " ).append( limit );
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
