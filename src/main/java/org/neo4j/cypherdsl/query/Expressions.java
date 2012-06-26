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

import org.neo4j.cypherdsl.Expression;

/**
* TODO
*/
public class Expressions
    extends AbstractExpression
{
    public Expression[] expressions;
    public String separator;

    public Expressions( Expression[] expressions )
    {
        this.expressions = expressions;
        this.separator = ",";
    }

    public Expressions( Expression[] expressions, String separator)
    {
        this.expressions = expressions;
        this.separator = separator;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        for( int i = 0; i < expressions.length; i++ )
        {
            Expression expression = expressions[ i ];
            if (i>0)
                builder.append( separator );
            expression.asString( builder );
        }
    }
}
