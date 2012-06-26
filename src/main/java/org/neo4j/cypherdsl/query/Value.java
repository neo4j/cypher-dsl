/**
 * Copyright (c) 2002-2011 "Neo Technology,"
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
import org.neo4j.cypherdsl.Expression;
import org.neo4j.cypherdsl.NodeExpression;
import org.neo4j.cypherdsl.NumericExpression;
import org.neo4j.cypherdsl.PathExpression;
import org.neo4j.cypherdsl.RelationshipExpression;
import org.neo4j.cypherdsl.ScalarExpression;
import org.neo4j.cypherdsl.StringExpression;

import static org.neo4j.cypherdsl.CypherQuery.*;

/**
 * Handles a single value that corresponds to any expression. Optionally
 * can be a part of binary operation.
 */
public class Value
    extends AbstractExpression
    implements ScalarExpression, NumericExpression, StringExpression, BooleanExpression, NodeExpression, RelationshipExpression, PathExpression, CollectionExpression
{
    public Operator operator;
    public Expression value;

    public Value( Expression value )
    {
        Query.checkNull( value, "Value" );
        this.value = value;
    }

    public Value( Operator operator, Expression value )
    {
        Query.checkNull( operator, "Operator" );
        Query.checkNull( value, "Value" );
        this.operator = operator;
        this.value = value;
    }

    // NumericExpression --------------------------------------------
    public NumericExpression add(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "+" ), expression);
    }

    public NumericExpression subtract(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "-" ), expression);
    }

    public NumericExpression times(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "*" ), expression);
    }

    public NumericExpression divideBy(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "/" ), expression);
    }

    public NumericExpression mod(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "%" ), expression);
    }

    public BooleanExpression gt(Number expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), literal(expression));
    }

    public BooleanExpression lt(Number expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), literal(expression));
    }

    public BooleanExpression gte(Number expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), literal(expression));
    }

    public BooleanExpression lte(Number expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), literal(expression));
    }

    public BooleanExpression gt(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), expression);
    }

    public BooleanExpression lt(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), expression);
    }

    public BooleanExpression gte(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), expression);
    }

    public BooleanExpression lte(NumericExpression expression)
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), expression);
    }

    // String expression --------------------------------------------
    @Override
    public BooleanExpression gt( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), literal( expression));
    }

    @Override
    public BooleanExpression gt( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), expression);
    }

    @Override
    public BooleanExpression gte( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), literal( expression));
    }

    @Override
    public BooleanExpression gte( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), expression);
    }

    @Override
    public BooleanExpression lt( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), literal( expression));
    }

    @Override
    public BooleanExpression lt( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), expression);
    }

    @Override
    public BooleanExpression lte( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), literal( expression));
    }

    @Override
    public BooleanExpression lte( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), expression);
    }

    /**
     * Create a case-sensitive regular expression. Corresponds to:
     * <pre>
     *     property ~=/regex/
     * </pre>
     *
     * @param regexp
     * @return
     */
    public BooleanExpression regexp( String regexp )
    {
        return regexp( regexp, true );
    }

    public BooleanExpression regexp( StringExpression regexp )
    {
        return regexp( regexp, true );
    }

    /**
     * Create a regular expression. Corresponds to:
     * <pre>
     *     property ~=/regex/
     * </pre>
     *
     * @param regexp
     * @param caseSensitive
     * @return
     */
    public BooleanExpression regexp( String regexp, boolean caseSensitive )
    {
        return regexp(literal( regexp ), caseSensitive);
    }

    public BooleanExpression regexp( StringExpression regexp, boolean caseSensitive )
    {
        Regexp regularExpression = new Regexp();
        regularExpression.caseSensitive = caseSensitive;
        regularExpression.left = this;
        regularExpression.regexp = regexp;
        return new Value( regularExpression );
    }

    public StringExpression concat(String expression)
    {
        return new Value( new Operator( this, "+" ), literal( expression ));
    }

    public StringExpression concat(StringExpression expression)
    {
        return new Value( new Operator( this, "+" ), expression );
    }

    @Override
    public void asString( StringBuilder builder )
    {
        if (operator != null)
            operator.asString( builder );
        value.asString( builder );
    }

    @Override
    public String toString()
    {
        return value.toString();
    }
}
