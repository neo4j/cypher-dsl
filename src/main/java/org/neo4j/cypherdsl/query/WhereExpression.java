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

import java.io.Serializable;

/**
 * Provides the possible expressions for the WHERE clause.
 */
public abstract class WhereExpression
    implements AsString, Serializable, Cloneable
{
    public static And and(PredicateExpression... expressions)
    {
        Query.checkNull( expressions, "Expressions" );

        And and = new And();
        and.expressions = expressions;
        return and;
    }

    public static Or or(PredicateExpression... expressions)
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

    public static String optional(String propertyName)
    {
        return propertyName+"?";
    }

    public static Equals eq( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Value" );

        Equals equals = new Equals();
        equals.property = property;
        equals.value = value;
        return equals;
    }

    public static GT gt( String property, Number number )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( number, "Number" );

        GT gt = new GT();
        gt.property = property;
        gt.number = number;
        return gt;
    }

    public static LT lt( String property, Number number )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( number, "Number" );

        LT lt = new LT();
        lt.property = property;
        lt.number = number;
        return lt;
    }

    public static GTE gte( String property, Number number )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( number, "Number" );

        GTE gte = new GTE();
        gte.property = property;
        gte.number = number;
        return gte;
    }

    public static LTE lte( String property, Number number )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( number, "Number" );

        LTE lte = new LTE();
        lte.property = property;
        lte.number = number;
        return lte;
    }

    public static NE ne( String property, Object value )
    {
        Query.checkEmpty( property, "Property" );
        Query.checkNull( value, "Value" );

        NE ne = new NE();
        ne.property = property;
        ne.value = value;
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

    public static class And
        extends PredicateExpression
    {
        public PredicateExpression[] expressions;

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
        extends PredicateExpression
    {
        public PredicateExpression[] expressions;

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
        extends PredicateExpression
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

    public static class Equals
        extends PredicateExpression
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
        extends PredicateExpression
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
            builder.append( property ).append( " is null" );
        }
    }

    public static class IsNotNull
        extends PredicateExpression
    {
        public String property;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( property ).append( " is not null" );
        }
    }

    public static class GT
        extends PredicateExpression
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
        extends PredicateExpression
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
        extends PredicateExpression
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
        extends PredicateExpression
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
        extends PredicateExpression
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

    public abstract static class PredicateExpression
        extends WhereExpression
    {
        public boolean optional;

        public PredicateExpression optional()
        {
            optional = true;
            return this;
        }

        public And and(PredicateExpression expression)
        {
            return and( this, expression );
        }

        public Or or(PredicateExpression expression)
        {
            return or( this, expression );
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
