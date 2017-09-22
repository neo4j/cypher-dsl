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

import org.neo4j.cypherdsl.AsString;
import org.neo4j.cypherdsl.expression.Expression;

/**
 * Represents the left part and the operator in an operation. E.g. 1 + 2 -> 1 and + are handled here. This is used
 * with a Value that holds an Operator and deals with the 2.
 */
public class Operator
        implements AsString, Serializable
{
    public final Expression left; // null if this is a unary operator
    public final String operator;

    public Operator( String operator )
    {
        this( null, operator );
    }

    public Operator( Expression left, String operator )
    {
        this.left = left;
        this.operator = operator;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        if ( left != null )
        {
            left.asString( builder );
        }
        builder.append( operator );
    }
}
