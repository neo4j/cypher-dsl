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

import org.neo4j.cypherdsl.Order;
import org.neo4j.cypherdsl.expression.Expression;

/**
 * Provides the possible expressions for the ORDER BY clause.
 */
public class OrderByExpression
        extends AbstractExpression
{
    public final Expression expression;
    public final Order order;

    public OrderByExpression( Expression expression, Order order )
    {
        this.expression = expression;
        this.order = order;
    }

    public OrderByExpression order( Order order )
    {
        return new OrderByExpression( expression, order );
    }

    public void asString( StringBuilder builder )
    {
        expression.asString( builder );
        if ( order != null )
        {
            builder.append( ' ' ).append( order.name() );
        }
    }

    @Override
    public Object clone()
            throws CloneNotSupportedException
    {
        return super.clone();
    }
}
