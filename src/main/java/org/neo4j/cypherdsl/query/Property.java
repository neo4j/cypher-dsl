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

import java.util.regex.Pattern;
import org.neo4j.cypherdsl.CypherQuery;

/**
* TODO
*/
public abstract class Property<TYPE extends Property>
    extends Expression
{
    private static Pattern simpleName = Pattern.compile( "\\w*" );

    protected Identifier owner;
    public String name;
    protected NullHandling nullHandling = NullHandling.NULL;

    public TYPE falseIfMissing()
    {
        nullHandling = NullHandling.FALSE_IF_MISSING;
        return (TYPE) this;
    }

    public TYPE trueIfMissing()
    {
        nullHandling = NullHandling.TRUE_IF_MISSING;
        return (TYPE) this;
    }

    public TYPE optional()
    {
        nullHandling = NullHandling.TRUE_IF_MISSING;
        return (TYPE) this;
    }

    public BinaryPredicateExpression eq(Object value)
    {
        return binaryPredicate( "=", value );
    }

    public BinaryPredicateExpression gt(Object value)
    {
        return binaryPredicate( ">", value );
    }

    public BinaryPredicateExpression lt(Object value)
    {
        return binaryPredicate( "<", value );
    }

    public BinaryPredicateExpression gte(Object value)
    {
        return binaryPredicate( ">=", value );
    }

    public BinaryPredicateExpression lte(Object value)
    {
        return binaryPredicate( ">=", value );
    }

    public BinaryPredicateExpression ne(Object value)
    {
        return binaryPredicate( "<>", value );
    }

    public Has has()
    {
        Has has = new Has();
        has.expression = this;
        return has;
    }

    public IsNull isNull()
    {
        IsNull isNull = new IsNull();
        isNull.expression = this;
        return isNull;
    }

    public IsNotNull isNotNull()
    {
        IsNotNull isNotNull = new IsNotNull();
        isNotNull.expression = this;
        return isNotNull;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        if (owner != null)
        {
            owner.asString( builder );
            builder.append( '.' );
        }

        if (simpleName.matcher( name ).matches())
            builder.append( name );
        else
            builder.append( '`' ).append( name ).append( '`' );
        AsString nullHandling1 = (AsString) nullHandling;
        nullHandling1.asString( builder );
    }

    private BinaryPredicateExpression binaryPredicate( String operator, Object value )
    {
        Query.checkNull( value, "Value" );

        BinaryPredicateExpression binaryPredicateExpression = new BinaryPredicateExpression();
        binaryPredicateExpression.operator = operator;
        binaryPredicateExpression.left = this;
        binaryPredicateExpression.right = value instanceof Expression ? (Expression) value : CypherQuery.literal( value );
        return binaryPredicateExpression;
    }
}
