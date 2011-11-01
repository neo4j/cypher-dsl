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

import org.neo4j.cypherdsl.ast.AsString;

/**
 * TODO
 */
public class AggregationExpression
    implements AsString
{
    public static AggregationExpression count()
    {
        return new AggregationExpression("count", null);
    }

    public static AggregationExpression count(String name)
    {
        return new AggregationExpression("count", name);
    }

    public String function;
    public String name;
    public boolean distinct;

    public AggregationExpression( String function, String name)
    {
        this.function = function;
        this.name = name;
    }

    public AggregationExpression distinct()
    {
        distinct = true;
        return this;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        builder.append( name ).append( '(' );
        
    }
}
