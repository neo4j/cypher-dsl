/**
 * Licensed to Neo Technology under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Neo Technology licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
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
        Query.checkEmpty( name, "Name" );
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

    public OrderByExpression order(OrderBy.Order order)
    {
        this.order = order;
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
