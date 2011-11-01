/*
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

package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.ast.AsString;

/**
 * TODO
 */
public abstract class WhereExpression
    implements AsString
{
    public static And and(WhereExpression... expressions)
    {
        And and = new And();
        and.expressions = expressions;
        return and;
    }

    public static Or or(WhereExpression... expressions)
    {
        Or or = new Or();
        or.expressions = expressions;
        return or;
    }

    public static Not not(WhereExpression expression)
    {
        Not not = new Not();
        not.expression = expression;
        return not;
    }

    public static String optional(String propertyName)
    {
        return propertyName+"?";
    }

    public static Equals eq( String property, Object value )
    {
        Equals equals = new Equals();
        equals.property = property;
        equals.value = value;
        return equals;
    }

    public static GT gt( String property, Number number )
    {
        GT gt = new GT();
        gt.property = property;
        gt.number = number;
        return gt;
    }

    public static LT lt( String property, Number number )
    {
        LT lt = new LT();
        lt.property = property;
        lt.number = number;
        return lt;
    }

    public static GTE gte( String property, Number number )
    {
        GTE gte = new GTE();
        gte.property = property;
        gte.number = number;
        return gte;
    }

    public static LTE lte( String property, Number number )
    {
        LTE lte = new LTE();
        lte.property = property;
        lte.number = number;
        return lte;
    }

    public static NE ne( String property, Object value )
    {
        NE ne = new NE();
        ne.property = property;
        ne.value = value;
        return ne;
    }

    public static Regexp regexp( String property, String regexp )
    {
        Regexp regexp1 = new Regexp();
        regexp1.property = property;
        regexp1.regexp = regexp;
        return regexp1;
    }

    public static Exists exists(String property)
    {
        Exists exists = new Exists();
        exists.property = property;
        return exists;
    }

    public static IsNull isNull(String property)
    {
        IsNull isNull = new IsNull();
        isNull.property = property;
        return isNull;
    }

    public static IsNotNull isNotNull(String property)
    {
        IsNotNull isNotNull = new IsNotNull();
        isNotNull.property = property;
        return isNotNull;
    }

    public static Literal literal(String whereClause)
    {
        Literal literal = new Literal( );
        literal.literal = whereClause;
        return literal;
    }

    public static Type type(String relationShipName)
    {
        Type type = new Type();
        type.name = relationShipName;
        return type;
    }

    public And and(WhereExpression expression)
    {
        return and( this, expression );
    }

    public Or or(WhereExpression expression)
    {
        return or( this, expression );
    }

    public static class And
        extends WhereExpression
    {
        public WhereExpression[] expressions;

        @Override
        public void asString( StringBuilder builder )
        {
            for( int i = 0; i < expressions.length; i++ )
            {
                WhereExpression expression = expressions[ i ];
                if (i > 0)
                    builder.append( " and " );
                expression.asString( builder );
            }
        }
    }

    public static class Or
        extends WhereExpression
    {
        public WhereExpression[] expressions;

        @Override
        public void asString( StringBuilder builder )
        {
            for( int i = 0; i < expressions.length; i++ )
            {
                WhereExpression expression = expressions[ i ];
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
        extends WhereExpression
    {
        public WhereExpression expression;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( "not(" );
            expression.asString( builder );
            builder.append( ')' );
        }
    }

    public static class Equals
        extends WhereExpression
    {
        public String property;
        public Object value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( '=' );
            if (value instanceof String)
                builder.append( '\"' ).append( value ).append( '\"' );
            else
                builder.append( value );
        }
    }

    public static class Regexp
        extends WhereExpression
    {
        public String property;
        public String regexp;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( "=~/" ).append( regexp ).append( "/" );
        }
    }

    public static class Exists
        extends WhereExpression
    {
        public String property;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
        }
    }

    public static class IsNull
        extends WhereExpression
    {
        public String property;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( " is null" );
        }
    }

    public static class IsNotNull
        extends WhereExpression
    {
        public String property;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( " is not null" );
        }
    }

    public static class GT
        extends WhereExpression
    {
        public String property;
        public Number number;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( '>' ).append( number );
        }
    }

    public static class LT
        extends WhereExpression
    {
        public String property;
        public Number number;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( '<' ).append( number );
        }
    }

    public static class GTE
        extends WhereExpression
    {
        public String property;
        public Number number;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( ">=" ).append( number );
        }
    }

    public static class LTE
        extends WhereExpression
    {
        public String property;
        public Number number;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( "<=" ).append( number );
        }
    }

    public static class NE
        extends WhereExpression
    {
        public String property;
        public Object value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( "!=" ).append( value );
        }
    }

    public static class Literal
        extends WhereExpression
    {
        public String literal;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( literal );
        }
    }

    public static class Type
    {
        public String name;

        public Regexp regexp(String regexp)
        {
            return WhereExpression.regexp("type("+name+")",regexp);
        }

        public Equals eq(String name)
        {
            return WhereExpression.eq("type("+this.name+")",name);
        }
    }
}
