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

import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.expression.BooleanExpression;
import org.neo4j.cypherdsl.expression.CollectionExpression;

/**
 * Represents a filter function
 */
public class Filter
        extends AbstractExpression
{
    public final Identifier name;
    public final CollectionExpression iterable;
    public final BooleanExpression predicate;

    public Filter( Identifier name, CollectionExpression iterable, BooleanExpression predicate )
    {
        this.name = name;
        this.iterable = iterable;
        this.predicate = predicate;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        builder.append( "filter" ).append( '(' );
        name.asString( builder );
        builder.append( " IN " );
        iterable.asString( builder );
        builder.append( " WHERE " );
        predicate.asString( builder );
        builder.append( ')' );
    }
}
