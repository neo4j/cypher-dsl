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
package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.SetExpression;

/**
 * Represents a property being assigned to a value. This is used for the SET clause,
 * as well as the MERGE ON CREATE and ON MATCH clauses.
 */
public class SetProperty
        implements AsString, SetExpression
{
    private final Property property;
    private final Expression value;

    SetProperty( Property property, Expression value )
    {
        this.property = property;
        this.value = value;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        property.asString( builder );
        builder.append( '=' );
        value.asString( builder );
    }
}
