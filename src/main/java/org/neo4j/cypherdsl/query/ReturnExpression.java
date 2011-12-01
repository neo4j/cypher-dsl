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

import static org.neo4j.cypherdsl.query.Query.*;

/**
 * Provides the possible expressions for the RETURN clause.
 */
public abstract class ReturnExpression<T extends ReturnExpression>
    implements AsString, Serializable, Cloneable
{
    public static ReturnNodes nodes( String... names )
    {
        Query.checkEmpty( names, "Names" );

        ReturnNodes returnNode = new ReturnNodes();
        returnNode.names = names;

        return returnNode;
    }

    public static ReturnRelationships relationships( String... names )
    {
        checkEmpty(names, "Names");

        ReturnRelationships returnRelationship = new ReturnRelationships();
        returnRelationship.names = names;
        return returnRelationship;
    }

    public static ReturnProperties properties( String... names )
    {
        checkEmpty( names, "Names" );
        ReturnProperties returnProperty = new ReturnProperties();
        returnProperty.names = names;
        return returnProperty;
    }

    public static ReturnPaths paths( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnPaths returnPath = new ReturnPaths();
        returnPath.names = names;
        return returnPath;
    }

    public static ReturnAggregate count()
    {
        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "count";
        return returnAggregate;
    }

    public static ReturnAggregate count(String name)
    {
        checkEmpty( name, "Name" );

        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "count";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate sum(String name)
    {
        checkEmpty( name, "Name" );

        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "sum";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate avg(String name)
    {
        checkEmpty( name, "Name" );

        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "avg";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate max(String name)
    {
        checkEmpty( name, "Name" );

        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "max";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate min(String name)
    {
        checkEmpty( name, "Name" );

        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "min";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate collect(String name)
    {
        checkEmpty( name, "Name" );

        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "collect";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnNameFunction length( String name )
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnNameFunction returnFunction = new ReturnExpression.ReturnNameFunction();
        returnFunction.function = "length";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnNameFunction type(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnNameFunction returnFunction = new ReturnExpression.ReturnNameFunction();
        returnFunction.function = "type";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnNameFunction id(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnNameFunction returnFunction = new ReturnExpression.ReturnNameFunction();
        returnFunction.function = "id";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnExpressionsFunction coalesce(ReturnExpression... expressions)
    {
        if (expressions.length < 1)
            throw new IllegalArgumentException("At least one expression must be provided to coalesce function");

        ReturnExpression.ReturnExpressionsFunction returnFunction = new ReturnExpression.ReturnExpressionsFunction();
        returnFunction.function = "coalesce";
        returnFunction.expressions = expressions;
        return returnFunction;
    }

    public static ReturnNameFunction nodesOf(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnNameFunction returnFunction = new ReturnExpression.ReturnNameFunction();
        returnFunction.function = "nodes";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnNameFunction relationshipsOf(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnNameFunction returnFunction = new ReturnExpression.ReturnNameFunction();
        returnFunction.function = "relationships";
        returnFunction.name = name;
        return returnFunction;
    }

    public boolean distinct;
    public String[] as;

    public T distinct()
    {
        distinct = true;
        return (T) this;
    }

    @Override
    public Object clone()
        throws CloneNotSupportedException
    {
        return super.clone();
    }

    public static class ReturnNodes
        extends ReturnExpression<ReturnNodes>
    {
        public String[] names;

        public ReturnNodes as(String... names)
        {
            if (names.length != names.length)
                throw new IllegalArgumentException("Number of aliases does not match number of nodes specified");

            as = names;
            return this;
        }

        public void asString(StringBuilder builder)
        {
            for( int i = 0; i < names.length; i++ )
            {
                String name = names[ i ];
                if (i > 0)
                    builder.append( ',' );
                if (distinct)
                    builder.append( "distinct " );
                builder.append( name );

                if (as != null)
                    builder.append(" AS ").append(as[i]);
            }
        }
    }

    public static class ReturnRelationships
        extends ReturnExpression<ReturnRelationships>
    {
        public String[] names;

        public ReturnRelationships as(String... names)
        {
            if (names.length != names.length)
                throw new IllegalArgumentException("Number of aliases does not match number of relationships specified");

            as = names;
            return this;
        }

        public void asString(StringBuilder builder)
        {
            for( int i = 0; i < names.length; i++ )
            {
                String name = names[ i ];
                if (i > 0)
                    builder.append( ',' );
                if (distinct)
                    builder.append( "distinct " );
                builder.append( name );

                if (as != null)
                    builder.append(" AS ").append(as[i]);
            }
        }
    }

    public static class ReturnPaths
        extends ReturnExpression<ReturnPaths>
    {
        public String[] names;

        public ReturnPaths as(String... names)
        {
            if (names.length != names.length)
                throw new IllegalArgumentException("Number of aliases does not match number of paths specified");

            as = names;
            return this;
        }

        public void asString(StringBuilder builder)
        {
            for( int i = 0; i < names.length; i++ )
            {
                String name = names[ i ];
                if (i > 0)
                    builder.append( ',' );
                if (distinct)
                    builder.append( "distinct " );
                builder.append( name );

                if (as != null)
                    builder.append(" AS ").append(as[i]);
            }
        }
    }

    public static class ReturnProperties
        extends ReturnExpression<ReturnProperties>
    {
        public boolean optional;
        public String[] names;

        public ReturnProperties optional()
        {
            optional = true;
            return this;
        }

        public ReturnProperties as(String... names)
        {
            if (names.length != names.length)
                throw new IllegalArgumentException("Number of aliases does not match number of properties specified");

            as = names;
            return this;
        }

        public void asString(StringBuilder builder)
        {
            for( int i = 0; i < names.length; i++ )
            {
                String name = names[ i ];
                if (i > 0)
                    builder.append( ',' );
                if (distinct)
                    builder.append( "distinct " );
                builder.append( name );
                if (optional)
                    builder.append( '?' );

                if (as != null)
                    builder.append(" AS ").append(as[i]);
            }
        }
    }

    public abstract static class ReturnFunction<T extends ReturnFunction>
        extends ReturnExpression<T>
    {
        public String function;

        public T as(String name)
        {
            as = new String[]{name};
            return (T) this;
        }
    }

    public static class ReturnNameFunction
        extends ReturnFunction<ReturnNameFunction>
    {
        public String name;

        public ReturnNameFunction as(String name)
        {
            as = new String[]{name};
            return this;
        }

        public void asString(StringBuilder builder)
        {
            builder.append( function ).append( '(' ).append( name ).append( ')' );
            if (as != null)
                builder.append(" AS ").append(as[0]);
        }
    }

    public static class ReturnExpressionsFunction
        extends ReturnFunction<ReturnExpressionsFunction>
    {
        public ReturnExpression[] expressions;

        public ReturnExpressionsFunction as(String name)
        {
            as = new String[]{name};
            return this;
        }

        public void asString(StringBuilder builder)
        {
            builder.append( function ).append( '(' );

            for (int i = 0; i < expressions.length; i++)
            {
                ReturnExpression expression = expressions[i];
                if (i > 0)
                    builder.append( ',' );
                expression.asString(builder);
            }
            builder.append( ')' );
            if (as != null)
                builder.append(" AS ").append(as[0]);
        }
    }

    public static class ReturnAggregate
        extends ReturnExpression<ReturnAggregate>
    {
        public String function;
        public String name;
        public boolean optional;

        public ReturnAggregate optional()
        {
            optional = true;
            return this;
        }

        public ReturnAggregate as(String name)
        {
            as = new String[]{name};
            return this;
        }

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( function ).append( '(' );
            if (distinct)
                builder.append( "distinct " );
            if (name == null)
                builder.append( "*" );
            else
            {
                builder.append( name );
                if (optional)
                    builder.append( '?' );
            }
            builder.append( ')' );
            if (as != null)
                builder.append(" AS ").append(as[0]);
        }
    }
}
