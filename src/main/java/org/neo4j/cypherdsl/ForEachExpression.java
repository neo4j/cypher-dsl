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

package org.neo4j.cypherdsl;

import java.util.Arrays;
import org.neo4j.cypherdsl.query.AbstractPath;
import org.neo4j.cypherdsl.query.CreateClause;
import org.neo4j.cypherdsl.query.DeleteClause;
import org.neo4j.cypherdsl.query.Expression;
import org.neo4j.cypherdsl.query.ForEachClause;
import org.neo4j.cypherdsl.query.Identifier;
import org.neo4j.cypherdsl.query.RelateClause;
import org.neo4j.cypherdsl.query.SetClause;
import org.neo4j.cypherdsl.query.SetProperty;

/**
 * TODO
 */
public class ForEachExpression
    implements Create, Set, Delete, Relate, ForEach
{
    private UpdateNext updateNext;
    private ForEachClause clause;

    public ForEachExpression( ForEachClause clause, UpdateNext updateNext)
    {
        this.clause = clause;
        this.updateNext = updateNext;
    }

    @Override
    public UpdateNext create( AbstractPath<?>... paths )
    {
        clause.execute( new CreateClause( Arrays.asList( paths ) ) );
        return updateNext;
    }

    @Override
    public UpdateNext set( SetProperty... propertyValues )
    {
        clause.execute( new SetClause( Arrays.asList( propertyValues ) ) );
        return updateNext;
    }

    @Override
    public UpdateNext delete( Expression... expressions )
    {
        clause.execute( new DeleteClause( Arrays.asList( expressions ) ) );
        return updateNext;
    }

    @Override
    public UpdateNext relate( AbstractPath<?>... expressions )
    {
        clause.execute( new RelateClause( Arrays.asList( expressions ) ) );
        return updateNext;
    }

    @Override
    public ForEachExpression forEach( Identifier id, Expression in )
    {
        ForEachClause nextForEach = new ForEachClause( id, in );
        clause.execute( nextForEach );
        return new ForEachExpression(nextForEach, updateNext);
    }
}
