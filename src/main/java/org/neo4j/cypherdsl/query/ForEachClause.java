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

package org.neo4j.cypherdsl.query;

/**
 * TODO
 */
public class ForEachClause
    extends Clause
{
    private final Identifier id;
    private final Expression in;
    private Clause clause;

    public ForEachClause( Identifier id, Expression in )
    {
        this.id = id;
        this.in = in;
    }

    public void execute(Clause clause)
    {
        this.clause = clause;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        builder.append( " FOREACH(" );
        id.asString( builder );
        builder.append( " in " );
        in.asString( builder );
        builder.append( ":" );
        clause.asString( builder );
        builder.append( ')' );
    }
}
