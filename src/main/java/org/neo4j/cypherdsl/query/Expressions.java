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

import org.neo4j.cypherdsl.expression.Expression;

/**
 * Collections many expressions into a list
 */
public class Expressions
        extends AbstractExpression
{
    public final Expression[] expressions;
    public final String separator;

    public Expressions( Expression[] expressions )
    {
        this.expressions = expressions;
        this.separator = ",";
    }

    public Expressions( Expression[] expressions, String separator )
    {
        this.expressions = expressions;
        this.separator = separator;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        for ( int i = 0; i < expressions.length; i++ )
        {
            Expression expression = expressions[i];
            if ( i > 0 )
            {
                builder.append( separator );
            }
            expression.asString( builder );
        }
    }
}
