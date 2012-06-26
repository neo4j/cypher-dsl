/**
 * Copyright (c) 2002-2011 "Neo Technology,"
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

package org.neo4j.cypherdsl;

import java.util.Arrays;
import org.neo4j.cypherdsl.query.AsString;
import org.neo4j.cypherdsl.query.SetProperty;
import org.neo4j.cypherdsl.query.clause.Clause;
import org.neo4j.cypherdsl.query.clause.CreateClause;
import org.neo4j.cypherdsl.query.clause.DeleteClause;
import org.neo4j.cypherdsl.query.clause.ForEachClause;
import org.neo4j.cypherdsl.query.clause.RelateClause;
import org.neo4j.cypherdsl.query.clause.SetClause;

/**
 * TODO
 */
public class ForEachStatement
    implements ForEachStatements, AsString
{
    private ForEachClause forEachClause;

    public ForEachStatement( ForEachClause forEachClause )
    {
        this.forEachClause = forEachClause;
    }

    public ForEachStatement create(PathExpression... paths)
    {
        return new ForEachStatement( forEachClause.add(new CreateClause( Arrays.asList( paths ) )));
    }

    @Override
    public ForEachStatement set( SetProperty... setProperties )
    {
        return new ForEachStatement( forEachClause.add(new SetClause( Arrays.asList( setProperties ) )));
    }

    @Override
    public ForEachStatement delete( ReferenceExpression... expressions )
    {
        return new ForEachStatement( forEachClause.add(new DeleteClause( Arrays.asList( expressions ) )));
    }

    @Override
    public ForEachStatement relate( PathExpression... expressions )
    {
        return new ForEachStatement( forEachClause.add(new RelateClause( Arrays.asList( expressions ) )));
    }

    @Override
    public ForEachStatement forEach( ForEachStatement statement )
    {
        return new ForEachStatement( forEachClause.add(statement.getClause()));
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
