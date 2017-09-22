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
import java.util.Arrays;
import java.util.List;

import org.neo4j.cypherdsl.AsString;
import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.expression.ReferenceExpression;
import org.neo4j.cypherdsl.expression.SetExpression;
import org.neo4j.cypherdsl.grammar.ForEachStatement;
import org.neo4j.cypherdsl.grammar.ForEachStatements;

/**
 * FOR EACH clause
 */
public class ForEachClause
        extends Clause
        implements AsString, ForEachStatements
{
    private final Identifier id;
    private final Expression in;
    private final List<AsString> forEachStatements = new ArrayList<AsString>();

    public ForEachClause( Identifier id, Expression in )
    {
        this.id = id;
        this.in = in;
    }

    @Override
    public ForEachStatement create( PathExpression... paths )
    {
        return new ForEachStatement( add( new CreateClause( Arrays.asList( paths ) ) ) );
    }

    @Override
    public ForEachStatement create( Iterable<PathExpression> paths )
    {
        return new ForEachStatement( add( new CreateClause( paths ) ) );
    }

    @Override
    public ForEachStatement set( SetExpression... setExpressions )
    {
        return new ForEachStatement( add( new SetClause( Arrays.asList( setExpressions ) ) ) );
    }

    @Override
    public ForEachStatement set( Iterable<SetExpression> setExpressions )
    {
        return new ForEachStatement( add( new SetClause( setExpressions ) ) );
    }

    @Override
    public ForEachStatement delete( ReferenceExpression... expressions )
    {
        return new ForEachStatement( add( new DeleteClause( Arrays.asList( expressions ) ) ) );
    }

    @Override
    public ForEachStatement delete( Iterable<ReferenceExpression> expressions )
    {
        return new ForEachStatement( add( new DeleteClause( expressions ) ) );
    }

    @Override
    public ForEachStatement createUnique( PathExpression... expressions )
    {
        return new ForEachStatement( add( new CreateUniqueClause( Arrays.asList( expressions ) ) ) );
    }

    @Override
    public ForEachStatement createUnique( Iterable<PathExpression> expressions )
    {
        return new ForEachStatement( add( new CreateUniqueClause( expressions ) ) );
    }

    @Override
    public ForEachStatement forEach( ForEachStatement statement )
    {
        return new ForEachStatement( add( statement ) );
    }

    public ForEachClause add( AsString clause )
    {
        forEachStatements.add( clause );
        return this;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        builder.append( " FOREACH(" );
        id.asString( builder );
        builder.append( " in " );
        in.asString( builder );
        builder.append( "|" );

        String comma = "";
        for ( AsString forEachStatement : forEachStatements )
        {
            builder.append( comma );
            forEachStatement.asString( builder );
            comma = ",";
        }

        builder.append( ')' );
    }
}
