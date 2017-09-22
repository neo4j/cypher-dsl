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
package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.expression.ReferenceExpression;
import org.neo4j.cypherdsl.expression.SetExpression;

/**
 * Used to create statements for the FOREACH clause. When you
 * have set all statements you want to evaluate for this FOR EACH clause,
 * use the resulting ForEachStatement as input to the {@link ForEach.forEach()} method.
 */
public interface ForEachStatements
{
    ForEachStatement create( PathExpression... paths );

    ForEachStatement create( Iterable<PathExpression> paths );

    ForEachStatement set( SetExpression... setExpressions );

    ForEachStatement set( Iterable<SetExpression> setExpressions );

    ForEachStatement delete( ReferenceExpression... expressions );

    ForEachStatement delete( Iterable<ReferenceExpression> expressions );

    ForEachStatement createUnique( PathExpression... expressions );

    ForEachStatement createUnique( Iterable<PathExpression> expressions );

    ForEachStatement forEach( ForEachStatement statement );
}
