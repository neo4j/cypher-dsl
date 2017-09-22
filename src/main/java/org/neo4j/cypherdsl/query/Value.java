/**
 * Licensed to Neo Technology under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Neo Technology licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.neo4j.cypherdsl.query;

import static org.neo4j.cypherdsl.CypherQuery.literal;

import org.neo4j.cypherdsl.expression.BooleanExpression;
import org.neo4j.cypherdsl.expression.CollectionExpression;
import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.NodeExpression;
import org.neo4j.cypherdsl.expression.NumericExpression;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.expression.RelationshipExpression;
import org.neo4j.cypherdsl.expression.ScalarExpression;
import org.neo4j.cypherdsl.expression.StringExpression;

/**
 * Handles a single value that corresponds to any expression. Optionally
 * can be a part of binary operation.
 */
public class Value
        extends AbstractExpression
        implements ScalarExpression, NumericExpression, StringExpression, BooleanExpression, NodeExpression,
        RelationshipExpression, PathExpression, CollectionExpression
{
    public final Operator operator;
    public final Expression value;

    public Value( Expression value )
    {
        Query.checkNull( value, "Value" );
        this.value = value;
        this.operator = null;
    }

    public Value( Operator operator, Expression value )
    {
        Query.checkNull( operator, "Operator" );
        Query.checkNull( value, "Value" );
        this.operator = operator;
        this.value = value;
    }

    // NumericExpression --------------------------------------------
    public NumericExpression add( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "+" ), literal( expression ) );
    }

    public NumericExpression add( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "+" ), expression );
    }

    public NumericExpression subtract( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "-" ), literal( expression ) );
    }

    public NumericExpression subtract( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "-" ), expression );
    }

    public NumericExpression times( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "*" ), literal( expression ) );
    }

    public NumericExpression times( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "*" ), expression );
    }

    public NumericExpression divideBy( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "/" ), literal( expression ) );
    }

    public NumericExpression divideBy( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "/" ), expression );
    }

    public NumericExpression mod( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "%" ), literal( expression ) );
    }

    public NumericExpression mod( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "%" ), expression );
    }

    public BooleanExpression gt( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), literal( expression ) );
    }

    public BooleanExpression lt( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), literal( expression ) );
    }

    public BooleanExpression gte( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), literal( expression ) );
    }

    public BooleanExpression lte( Number expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), literal( expression ) );
    }

    public BooleanExpression gt( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), expression );
    }

    public BooleanExpression lt( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), expression );
    }

    public BooleanExpression gte( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), expression );
    }

    public BooleanExpression lte( NumericExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), expression );
    }

    // String expression --------------------------------------------
    @Override
    public BooleanExpression gt( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), literal( expression ) );
    }

    @Override
    public BooleanExpression gt( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">" ), expression );
    }

    @Override
    public BooleanExpression gte( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), literal( expression ) );
    }

    @Override
    public BooleanExpression gte( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, ">=" ), expression );
    }

    @Override
    public BooleanExpression lt( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), literal( expression ) );
    }

    @Override
    public BooleanExpression lt( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<" ), expression );
    }

    @Override
    public BooleanExpression lte( String expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), literal( expression ) );
    }

    @Override
    public BooleanExpression lte( StringExpression expression )
    {
        Query.checkNull( expression, "Expression" );
        return new Value( new Operator( this, "<=" ), expression );
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
        return new Value( new Operator( this, "=~" ), literal( regexp ) );
    }

    public BooleanExpression regexp( StringExpression regexp )
    {
        return new Value( new Operator( this, "=~" ), regexp );
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
        if ( caseSensitive )
        {
            return regexp( regexp );
        }
        else
        {
            return new Value( new Operator( this, "=~" ), literal( "(?i)" + regexp ) );
        }
    }

    public StringExpression concat( String expression )
    {
        return new Value( new Operator( this, "+" ), literal( expression ) );
    }

    public StringExpression concat( StringExpression expression )
    {
        return new Value( new Operator( this, "+" ), expression );
    }

    @Override
    public void asString( StringBuilder builder )
    {
        if ( operator != null )
        {
            operator.asString( builder );
        }
        value.asString( builder );
    }

    @Override
    public String toString()
    {
        return value.toString();
    }
}
