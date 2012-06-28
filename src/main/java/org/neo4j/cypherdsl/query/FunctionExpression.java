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
import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.NodeExpression;
import org.neo4j.cypherdsl.expression.NumericExpression;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.expression.StringExpression;

/**
 * Expresses all functions of the form "f(exp)"
 */
public class FunctionExpression
    extends AbstractExpression
{
    public String name;
    public Expression expression;

    public FunctionExpression( String name, Expression expression )
    {
        this.name = name;
        this.expression = expression;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        builder.append( name ).append( '(' );
        expression.asString( builder );
        builder.append( ')' );
    }
}
