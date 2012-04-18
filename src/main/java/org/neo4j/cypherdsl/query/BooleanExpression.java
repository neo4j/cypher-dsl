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
* This represents the boolean expressions "and","or" and "not".
*/
public abstract class BooleanExpression
    extends PredicateExpression
{
    public static class And
        extends BooleanExpression
    {
        public PredicateExpression[] expressions;

        @Override
        public void asString( StringBuilder builder )
        {
            for( int i = 0; i < expressions.length; i++ )
            {
                PredicateExpression expression = expressions[ i ];
                if (i > 0)
                    builder.append( " and " );
                if (expression instanceof And || expression instanceof Or)
                {
                    builder.append( '(' );
                    expression.asString( builder );
                    builder.append( ')' );
                } else
                    expression.asString( builder );
            }
        }
    }

    public static class Or
        extends BooleanExpression
    {
        public PredicateExpression[] expressions;

        @Override
        public void asString( StringBuilder builder )
        {
            for( int i = 0; i < expressions.length; i++ )
            {
                PredicateExpression expression = expressions[ i ];
                if (i > 0)
                    builder.append( " or " );
                if (expression instanceof And)
                {
                    builder.append( '(' );
                    expression.asString( builder );
                    builder.append( ')' );
                } else
                    expression.asString( builder );
            }
        }
    }

    public static class Not
        extends BooleanExpression
    {
        public PredicateExpression expression;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( "not(" );
            expression.asString( builder );
            builder.append( ')' );
        }
    }
}
