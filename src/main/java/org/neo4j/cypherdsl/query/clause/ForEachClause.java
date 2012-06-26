/**
 * Copyright (c) 2002-2012 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypherdsl.query.clause;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.neo4j.cypherdsl.Expression;
import org.neo4j.cypherdsl.ForEachStatement;
import org.neo4j.cypherdsl.ForEachStatements;
import org.neo4j.cypherdsl.PathExpression;
import org.neo4j.cypherdsl.ReferenceExpression;
import org.neo4j.cypherdsl.query.AsString;
import org.neo4j.cypherdsl.query.Identifier;
import org.neo4j.cypherdsl.query.SetProperty;

/**
 * TODO
 */
public class ForEachClause
    extends Clause
    implements AsString, ForEachStatements
{
    private final Identifier id;
    private final Expression in;
    private final List<AsString> forEachStatements = new ArrayList<AsString>(  );

    public ForEachClause( Identifier id, Expression in )
    {
        this.id = id;
        this.in = in;
    }

    public ForEachStatement create(PathExpression... paths)
    {
        return new ForEachStatement(add( new CreateClause( Arrays.asList(paths) ) ) );
    }

    @Override
    public ForEachStatement set( SetProperty... setProperties )
    {
        return new ForEachStatement(add( new SetClause( Arrays.asList(setProperties) ) ) );
    }

    @Override
    public ForEachStatement delete( ReferenceExpression... expressions )
    {
        return new ForEachStatement(add( new DeleteClause( Arrays.asList(expressions) ) ) );
    }

    @Override
    public ForEachStatement relate( PathExpression... expressions )
    {
        return new ForEachStatement(add( new RelateClause( Arrays.asList(expressions) ) ) );
    }

    @Override
    public ForEachStatement forEach( ForEachStatement statement )
    {
        return new ForEachStatement(add( statement ) );
    }

    public ForEachClause add(AsString clause)
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
        builder.append( ":" );

        String comma = "";
        for( AsString forEachStatement : forEachStatements )
        {
            builder.append( comma );
            forEachStatement.asString( builder );
            comma = ",";
        }

        builder.append( ')' );
    }
}
