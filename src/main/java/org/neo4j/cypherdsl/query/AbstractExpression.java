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

import org.neo4j.cypherdsl.BooleanExpression;
import org.neo4j.cypherdsl.CollectionExpression;
import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.Expression;

import static org.neo4j.cypherdsl.CypherQuery.literal;

/**
 * Common methods for all expressions
 */
public abstract class AbstractExpression
    implements Expression
{
    @Override
    public BooleanExpression eq( Object expression )
    {
        return new Value(new Operator( this, "=" ), literal(expression));
    }

    @Override
    public BooleanExpression eq( Expression expression )
    {
        return new Value(new Operator( this, "=" ), expression);
    }

    @Override
    public BooleanExpression ne( Object expression )
    {
        return new Value(new Operator( this, "<>" ), literal( expression));
    }

    @Override
    public BooleanExpression ne( Expression expression )
    {
        return new Value(new Operator( this, "<>" ), expression);
    }

    public BooleanExpression and( BooleanExpression expression )
    {
        return new CypherQuery.And( new BooleanExpression[]{(BooleanExpression) this, expression});
    }

    public BooleanExpression or( BooleanExpression expression )
    {
        return new CypherQuery.Or( new BooleanExpression[]{(BooleanExpression) this, expression});
    }

    public BooleanExpression in( CollectionExpression collection )
    {
        return new Value( new Operator( this, " IN " ), collection );
    }

    public CollectionExpression union( CollectionExpression expression )
    {
        return new Value( new Operator( this, "+" ), expression );
    }

    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        return super.clone();
    }
}
