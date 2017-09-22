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
package org.neo4j.cypherdsl.query.clause;

import java.util.ArrayList;

import org.neo4j.cypherdsl.expression.PathExpression;

/**
 * MATCH clause
 */
public class MatchClause
        extends Clause
{
    private final ArrayList<PathExpression> expressions = new ArrayList<PathExpression>();
    private boolean optional;

    public MatchClause( Iterable<PathExpression> expressions )
    {
        for ( PathExpression expression : expressions )
        {
            this.expressions.add( expression );
        }
    }

    @Override
    public void asString( StringBuilder builder )
    {
        String name = (optional ? "OPTIONAL " : "") + "MATCH";
        clauseAsString( builder, name, expressions, "," );
    }

    public void optional() {
        this.optional = true;
    }
}
