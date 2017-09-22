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

import static org.neo4j.cypherdsl.CypherQuery.literal;

import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.expression.BooleanExpression;
import org.neo4j.cypherdsl.expression.CollectionExpression;
import org.neo4j.cypherdsl.expression.Expression;

/**
 * Common methods for all expressions
 */
public abstract class AbstractExpression
        implements Expression
{
    @Override
    public BooleanExpression eq( Object expression )
    {
        return new Value( new Operator( this, "=" ), literal( expression ) );
    }

    @Override
    public BooleanExpression eq( Expression expression )
    {
        return new Value( new Operator( this, "=" ), expression );
    }

    @Override
    public BooleanExpression ne( Object expression )
    {
        return new Value( new Operator( this, "<>" ), literal( expression ) );
    }

    @Override
    public BooleanExpression ne( Expression expression )
    {
        return new Value( new Operator( this, "<>" ), expression );
    }

    public BooleanExpression and( BooleanExpression expression )
    {
        return new CypherQuery.And( new BooleanExpression[]{(BooleanExpression) this, expression} );
    }

    public BooleanExpression or( BooleanExpression expression )
    {
        return new CypherQuery.Or( new BooleanExpression[]{(BooleanExpression) this, expression} );
    }

    public BooleanExpression in( CollectionExpression collection )
    {
        return new Value( new Operator( this, " IN " ), collection );
    }

    public CollectionExpression union( CollectionExpression expression )
    {
        return new Value( new Operator( this, "+" ), expression );
    }

    @Override
    public Object clone()
            throws CloneNotSupportedException
    {
        return super.clone();
    }

    @Override
    public String toString()
    {
        StringBuilder builder = new StringBuilder();
        asString( builder );
        return builder.toString();
    }
}
