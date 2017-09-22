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

import static java.util.Arrays.asList;

import org.neo4j.cypherdsl.AsString;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.expression.ReferenceExpression;
import org.neo4j.cypherdsl.expression.SetExpression;
import org.neo4j.cypherdsl.query.clause.Clause;
import org.neo4j.cypherdsl.query.clause.CreateClause;
import org.neo4j.cypherdsl.query.clause.CreateUniqueClause;
import org.neo4j.cypherdsl.query.clause.DeleteClause;
import org.neo4j.cypherdsl.query.clause.ForEachClause;
import org.neo4j.cypherdsl.query.clause.SetClause;

/**
 * Represents a single statement to be executed with FOREACH
 */
public class ForEachStatement
        implements ForEachStatements, AsString
{
    private final ForEachClause forEachClause;

    public ForEachStatement( ForEachClause forEachClause )
    {
        this.forEachClause = forEachClause;
    }

    public ForEachStatement create( PathExpression... paths )
    {
        return new ForEachStatement( forEachClause.add( new CreateClause( asList( paths ) ) ) );
    }

    @Override
    public ForEachStatement create( Iterable<PathExpression> paths )
    {
        return new ForEachStatement( forEachClause.add( new CreateClause( paths ) ) );
    }

    @Override
    public ForEachStatement set( SetExpression... setExpressions )
    {
        return new ForEachStatement( forEachClause.add( new SetClause( asList( setExpressions ) ) ) );
    }

    @Override
    public ForEachStatement set( Iterable<SetExpression> setExpressions )
    {
        return new ForEachStatement( forEachClause.add( new SetClause( setExpressions ) ) );
    }

    @Override
    public ForEachStatement delete( ReferenceExpression... expressions )
    {
        return new ForEachStatement( forEachClause.add( new DeleteClause( asList( expressions ) ) ) );
    }

    @Override
    public ForEachStatement delete( Iterable<ReferenceExpression> expressions )
    {
        return new ForEachStatement( forEachClause.add( new DeleteClause( expressions ) ) );
    }

    @Override
    public ForEachStatement createUnique( PathExpression... expressions )
    {
        return new ForEachStatement( forEachClause.add( new CreateUniqueClause( asList( expressions ) ) ) );
    }

    @Override
    public ForEachStatement createUnique( Iterable<PathExpression> expressions )
    {
        return new ForEachStatement( forEachClause.add( new CreateUniqueClause( expressions ) ) );
    }

    @Override
    public ForEachStatement forEach( ForEachStatement statement )
    {
        return new ForEachStatement( forEachClause.add( statement.getClause() ) );
    }

    @Override
    public void asString( StringBuilder builder )
    {
        forEachClause.asString( builder );
    }

    public Clause getClause()
    {
        return forEachClause;
    }
}
