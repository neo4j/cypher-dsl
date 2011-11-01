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

package org.neo4j.cypherdsl.query;

import java.io.Serializable;
import org.neo4j.cypherdsl.OrderBy;

/**
 * Provides the possible expressions for the ORDER BY clause.
 */
public class OrderByExpression
    implements AsString, Serializable, Cloneable
{
    public static OrderByExpression orderBy(String name)
    {
        OrderByExpression orderBy = new OrderByExpression();
        orderBy.name = name;
        return orderBy;
    }

    public String name;
    public OrderBy.Order order;
    public boolean optional;

    public OrderByExpression optional()
    {
        optional = true;
        return this;
    }

    public void asString(StringBuilder builder)
    {
        builder.append( name );
        if (order != null)
            builder.append( ' ' ).append( order.name() );
        if (optional)
            builder.append( '?' );
    }

    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        return super.clone();
    }
}
