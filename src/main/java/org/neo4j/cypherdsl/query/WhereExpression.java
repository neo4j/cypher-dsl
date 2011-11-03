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

import java.io.Serializable;

/**
 * Provides the possible expressions for the WHERE clause.
 */
public abstract class WhereExpression
    implements AsString, Serializable, Cloneable
{
    public static StringType string(String name)
    {
        return new StringType(name);
    }

    public static NumberType number(String name)
    {
        return new NumberType(name);
    }

    public static NumberType length(String name)
    {
        return new NumberType( "length("+name+")" );
    }

    public static And and(BooleanExpression... expressions)
    {
        Query.checkNull( expressions, "Expressions" );

        And and = new And();
        and.expressions = expressions;
        return and;
    }

    public static Or or(BooleanExpression... expressions)
    {
        Query.checkNull( expressions, "Expressions" );

        Or or = new Or();
        or.expressions = expressions;
        return or;
    }

    public static Not not(PredicateExpression expression)
    {
        Query.checkNull( expression, "Expression" );

        Not not = new Not();
        not.expression = expression;
        return not;
    }

    public static Equals eq( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Value" );

        Equals equals = new Equals();
        equals.property = property;
        equals.value = value instanceof Expression.Value ? (Expression.Value) value : Expression.literal( value );
        return equals;
    }

    public static GT gt( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Value" );

        GT gt = new GT();
        gt.property = property;
        gt.value = value instanceof Expression.Value ? (Expression.Value) value : Expression.literal( value );
        return gt;
    }

    public static LT lt( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Value" );

        LT lt = new LT();
        lt.property = property;
        lt.value = value instanceof Expression.Value ? (Expression.Value) value : Expression.literal( value );
        return lt;
    }

    public static GTE gte( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Number" );

        GTE gte = new GTE();
        gte.property = property;
        gte.value = value instanceof Expression.Value ? (Expression.Value) value : Expression.literal( value );
        return gte;
    }

    public static LTE lte( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Number" );

        LTE lte = new LTE();
        lte.property = property;
        lte.value = value instanceof Expression.Value ? (Expression.Value) value : Expression.literal( value );
        return lte;
    }

    public static NE ne( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Value" );

        NE ne = new NE();
        ne.property = property;
        ne.value = value instanceof Expression.Value ? (Expression.Value) value : Expression.literal( value );
        return ne;
    }

    public static Regexp regexp( String property, String regexp )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkEmpty( regexp, "Regular expression" );

        Regexp regexp1 = new Regexp();
        regexp1.property = property;
        regexp1.regexp = regexp;
        return regexp1;
    }

    public static Exists exists(String property)
    {
        Query.checkEmpty( property, "Property" );

        Exists exists = new Exists();
        exists.property = property;
        return exists;
    }

    public static IsNull isNull(String property)
    {
        Query.checkEmpty( property, "Property" );

        IsNull isNull = new IsNull();
        isNull.property = property;
        return isNull;
    }

    public static IsNotNull isNotNull(String property)
    {
        Query.checkEmpty( property, "Property" );

        IsNotNull isNotNull = new IsNotNull();
        isNotNull.property = property;
        return isNotNull;
    }

    public static Literal literal(String whereClause)
    {
        Query.checkEmpty( whereClause, "Literal clause" );

        Literal literal = new Literal( );
        literal.literal = whereClause;
        return literal;
    }

    public static Type type(String relationShipName)
    {
        Query.checkEmpty( relationShipName, "Relationship" );

        Type type = new Type();
        type.name = relationShipName;
        return type;
    }

    public static IterablePredicateExpression all( String name, String iterable, PredicateExpression predicateExpression )
    {
        Query.checkEmpty( name, "Name" );
        Query.checkEmpty( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "all";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    public static IterablePredicateExpression any( String name,
                                                   String iterable,
                                                   PredicateExpression predicateExpression
    )
    {
        Query.checkEmpty( name, "Name" );
        Query.checkEmpty( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "any";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    public static IterablePredicateExpression none( String name,
                                                   String iterable,
                                                   PredicateExpression predicateExpression
    )
    {
        Query.checkEmpty( name, "Name" );
        Query.checkEmpty( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "none";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    public static IterablePredicateExpression single( String name,
                                                   String iterable,
                                                   PredicateExpression predicateExpression
    )
    {
        Query.checkEmpty( name, "Name" );
        Query.checkEmpty( iterable, "Iterable" );
        Query.checkNull( predicateExpression, "Predicate" );

        IterablePredicateExpression expression = new IterablePredicateExpression();
        expression.function = "single";
        expression.name = name;
        expression.iterable = iterable;
        expression.predicate = predicateExpression;
        return expression;
    }

    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        return super.clone();
    }

    public static class StringType
    {
        private String name;

        private StringType( String name )
        {
            this.name = name;
        }

        public String getName()
        {
            return name;
        }

        public Equals eq(String value)
        {
            return WhereExpression.eq( name, value );
        }

        public Regexp regexp(String value)
        {
            return WhereExpression.regexp( name, value );
        }
    }

    public static class NumberType
    {
        private String name;

        private NumberType( String name )
        {
            this.name = name;
        }

        public String getName()
        {
            return name;
        }

        public Equals eq(Number value)
        {
            return WhereExpression.eq( name, value );
        }

        public GT gt(Number value)
        {
            return WhereExpression.gt( name, value );
        }

        public LT lt(Number value)
        {
            return WhereExpression.lt( name, value );
        }

        public GTE gte(Number value)
        {
            return WhereExpression.gte( name, value );
        }

        public LTE lte(Number value)
        {
            return WhereExpression.lte( name, value );
        }
    }

    public static class And
        extends BooleanExpression
    {
        public BooleanExpression[] expressions;

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
        extends BooleanExpression
    {
        public BooleanExpression[] expressions;

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
        extends BooleanExpression
    {
        public BooleanExpression expression;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( "not(" );
            expression.asString( builder );
            builder.append( ')' );
        }
    }

    public static class Equals
        extends PredicateExpression
    {
        public String property;
        public Expression.Value value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( '=' );
            value.asString( builder );
        }
    }

    public static class Regexp
        extends PredicateExpression
    {
        public String property;
        public String regexp;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( "=~/" ); // TODO Should the / be include for parameters?
            builder.append( regexp );
            builder.append( '/' );
        }
    }

    public static class Exists
        extends PredicateExpression
    {
        public String property;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
        }
    }

    public static class IsNull
        extends PredicateExpression
    {
        public String property;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( " is null" );
        }
    }

    public static class IsNotNull
        extends PredicateExpression
    {
        public String property;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( " is not null" );
        }
    }

    public static class GT
        extends PredicateExpression
    {
        public String property;
        public Expression.Value value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( '>' );
            value.asString( builder );
        }
    }

    public static class LT
        extends PredicateExpression
    {
        public String property;
        public Expression.Value value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( '<' );
            value.asString( builder );
        }
    }

    public static class GTE
        extends PredicateExpression
    {
        public String property;
        public Expression.Value value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( ">=" );
            value.asString( builder );
        }
    }

    public static class LTE
        extends PredicateExpression
    {
        public String property;
        public Expression.Value value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( "<=" );
            value.asString( builder );
        }
    }

    public static class NE
        extends PredicateExpression
    {
        public String property;
        public Expression.Value value;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property );
            if (optional)
                builder.append( '?' );
            builder.append( "!=" );
            value.asString( builder );
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

    public abstract static class BooleanExpression
        extends WhereExpression
    {
        public And and(BooleanExpression expression)
        {
            return and( this, expression );
        }

        public Or or(BooleanExpression expression)
        {
            return or( this, expression );
        }
    }

    public abstract static class PredicateExpression
        extends BooleanExpression
    {
        public boolean optional;

        public PredicateExpression optional()
        {
            optional = true;
            return this;
        }
    }

    public static class IterablePredicateExpression
        extends WhereExpression
    {
        public String function;
        public String name;
        public String iterable;
        public PredicateExpression predicate;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( function ).append( '(' ).append( name ).append( " in " ).append( iterable ).append( ':' );
            predicate.asString( builder );
            builder.append( ')' );
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
            return WhereExpression.eq( "type(" + this.name + ")", name );
        }
    }
}
