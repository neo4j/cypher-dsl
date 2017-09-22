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
 * Expression that evaluates to a string
 */
public interface StringExpression
        extends ScalarExpression
{
    public BooleanExpression gt( String expression );

    public BooleanExpression gt( StringExpression expression );

    public BooleanExpression gte( String expression );

    public BooleanExpression gte( StringExpression expression );

    public BooleanExpression lt( String expression );

    public BooleanExpression lt( StringExpression expression );

    public BooleanExpression lte( String expression );

    public BooleanExpression lte( StringExpression expression );

    public BooleanExpression regexp( String regexp );

    public BooleanExpression regexp( StringExpression regexp );

    public BooleanExpression regexp( String regexp, boolean caseSensitive );

    public StringExpression concat( String expression );

    public StringExpression concat( StringExpression expression );
}
