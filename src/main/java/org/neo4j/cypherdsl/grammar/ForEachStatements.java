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
package org.neo4j.cypherdsl.grammar;

import org.neo4j.cypherdsl.SetProperty;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.expression.ReferenceExpression;

/**
 * Used to create statements for the FOREACH clause. When you
 * have set all statements you want to evaluate for this FOR EACH clause,
 * use the resulting ForEachStatement as input to the {@link ForEach.forEach()} method.
 */
public interface ForEachStatements
{
    ForEachStatement create( PathExpression... paths );

    ForEachStatement create( Iterable<PathExpression> paths );

    ForEachStatement set( SetProperty... setProperties );

    ForEachStatement set( Iterable<SetProperty> setProperties );

    ForEachStatement delete( ReferenceExpression... expressions );

    ForEachStatement delete( Iterable<ReferenceExpression> expressions );

    ForEachStatement relate( PathExpression... expressions );

    ForEachStatement relate( Iterable<PathExpression> expressions );

    ForEachStatement forEach( ForEachStatement statement );
}
