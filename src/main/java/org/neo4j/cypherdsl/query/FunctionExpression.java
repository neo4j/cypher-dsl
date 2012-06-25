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

import org.neo4j.cypherdsl.CypherQuery;

/**
 * Base class for all functions
 */
public class FunctionExpression
    extends Expression
{
    public String name;
    public Expression expression;

    // Operators
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

    public BinaryOperatorExpression concat(Object value)
    {
        Query.checkNull( value, "Value" );

        BinaryOperatorExpression boe = new BinaryOperatorExpression();
        boe.left = this;
        boe.operator = "+";
        boe.right = value instanceof Expression ? (Expression) value : CypherQuery.literal( value );
        return boe;
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

    @Override
    public void asString( StringBuilder builder )
    {
        builder.append( name ).append( '(' );
        expression.asString( builder );
        builder.append( ')' );
    }

    public static class Expressions
        extends Expression
    {
        public Expression[] expressions;

        @Override
        public void asString( StringBuilder builder )
        {
            for( int i = 0; i < expressions.length; i++ )
            {
                Expression expression = expressions[ i ];
                if (i>0)
                    builder.append( ',' );
                expression.asString( builder );
            }
        }
    }

    public static class Extract
        extends Expression
    {
        public Identifier name;
        public Expression iterable;
        public Expression expression;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( "extract" ).append( '(' );
            name.asString( builder );
            builder.append( " IN " );
            iterable.asString( builder );
            builder.append( ":" );
            expression.asString( builder );
            builder.append( ')' );
        }

    }

    public static class Filter
        extends Expression
    {
        public Identifier name;
        public Expression iterable;
        public PredicateExpression predicate;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( "filter" ).append( '(' );
            name.asString( builder );
            builder.append( " IN " );
            iterable.asString( builder );
            builder.append( ":" );
            predicate.asString( builder );
            builder.append( ')' );
        }
    }
}
