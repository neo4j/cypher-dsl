/**
 * Copyright (c) 2002-2013 "Neo Technology,"
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

import org.neo4j.cypherdsl.Identifier;
import org.neo4j.cypherdsl.expression.BooleanExpression;
import org.neo4j.cypherdsl.expression.CollectionExpression;

/**
 * Iterable predicates are of the form: function(name IN iterable WHERE predicate)
 */
public class IterablePredicateExpression
        extends AbstractExpression
{
    public final String function;
    public final Identifier name;
    public final CollectionExpression iterable;
    public final BooleanExpression predicate;

    public IterablePredicateExpression( String function, Identifier name, CollectionExpression iterable,
                                        BooleanExpression predicate )
    {
        this.function = function;
        this.name = name;
        this.iterable = iterable;
        this.predicate = predicate;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        builder.append( function ).append( '(' );
        name.asString( builder );
        builder.append( " IN " );
        iterable.asString( builder );
        builder.append( " WHERE " );
        predicate.asString( builder );
        builder.append( ')' );
    }
}
