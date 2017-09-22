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
package org.neo4j.cypherdsl.expression;

/**
 * Expression that evaluates to a number
 */
public interface NumericExpression
        extends ScalarExpression
{
    public NumericExpression add( Number expression );

    public NumericExpression add( NumericExpression expression );

    public NumericExpression subtract( Number expression );

    public NumericExpression subtract( NumericExpression expression );

    public NumericExpression times( Number expression );

    public NumericExpression times( NumericExpression expression );

    public NumericExpression divideBy( Number expression );

    public NumericExpression divideBy( NumericExpression expression );

    public NumericExpression mod( Number expression );

    public NumericExpression mod( NumericExpression expression );

    public BooleanExpression gt( Number expression );

    public BooleanExpression gt( NumericExpression expression );

    public BooleanExpression lt( Number expression );

    public BooleanExpression lt( NumericExpression expression );

    public BooleanExpression gte( Number expression );

    public BooleanExpression gte( NumericExpression expression );

    public BooleanExpression lte( Number expression );

    public BooleanExpression lte( NumericExpression expression );
}
