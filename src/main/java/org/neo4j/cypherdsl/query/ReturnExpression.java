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

import static org.neo4j.cypherdsl.query.Query.*;

/**
 * Provides the possible expressions for the RETURN clause.
 */
public abstract class ReturnExpression<T extends ReturnExpression>
    implements AsString, Serializable, Cloneable
{
    public static ReturnNode node( String... names )
    {
        Query.checkEmpty( names, "Names" );

        ReturnNode returnNode = new ReturnNode();
        returnNode.names = names;

        return returnNode;
    }

    public static ReturnRelationship relationship( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnRelationship returnRelationship = new ReturnRelationship();
        returnRelationship.names = names;
        return returnRelationship;
    }

    public static ReturnProperty property( String... names )
    {
        checkEmpty( names, "Names" );
        ReturnProperty returnProperty = new ReturnProperty();
        returnProperty.names = names;
        return returnProperty;
    }

    public static ReturnPath path( String... names )
    {
        checkEmpty( names, "Names" );

        ReturnPath returnPath = new ReturnPath();
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

    public static ReturnExpression length( String name )
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnFunction returnFunction = new ReturnExpression.ReturnFunction();
        returnFunction.function = "length";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnFunction type(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnFunction returnFunction = new ReturnExpression.ReturnFunction();
        returnFunction.function = "type";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnFunction id(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnFunction returnFunction = new ReturnExpression.ReturnFunction();
        returnFunction.function = "id";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnFunction nodes(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnFunction returnFunction = new ReturnExpression.ReturnFunction();
        returnFunction.function = "nodes";
        returnFunction.name = name;
        return returnFunction;
    }

    public static ReturnFunction relationships(String name)
    {
        checkEmpty( name, "Name" );

        ReturnExpression.ReturnFunction returnFunction = new ReturnExpression.ReturnFunction();
        returnFunction.function = "relationships";
        returnFunction.name = name;
        return returnFunction;
    }

    public boolean distinct;

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

    public static class ReturnNode
        extends ReturnExpression<ReturnNode>
    {
        public String[] names;

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
            }
        }
    }

    public static class ReturnRelationship
        extends ReturnExpression<ReturnRelationship>
    {
        public String[] names;

        public void asString(StringBuilder builder)
        {
            for( int i = 0; i < names.length; i++ )
            {
                String name = names[ i ];
                if (i > 0)
                    builder.append( ',' );
                builder.append( name );
            }
        }
    }

    public static class ReturnPath
        extends ReturnExpression<ReturnPath>
    {
        public String[] names;

        public void asString(StringBuilder builder)
        {
            for( int i = 0; i < names.length; i++ )
            {
                String name = names[ i ];
                if (i > 0)
                    builder.append( ',' );
                builder.append( name );
            }
        }
    }

    public static class ReturnProperty
        extends ReturnExpression<ReturnProperty>
    {
        public boolean optional;
        public String[] names;

        public ReturnProperty optional()
        {
            optional = true;
            return this;
        }

        public void asString(StringBuilder builder)
        {
            for( int i = 0; i < names.length; i++ )
            {
                String name = names[ i ];
                if (i > 0)
                    builder.append( ',' );
                builder.append( name );
                if (optional)
                    builder.append( '?' );
            }
        }
    }

    public static class ReturnFunction
        extends ReturnExpression<ReturnFunction>
    {
        public String function;
        public String name;

        public void asString(StringBuilder builder)
        {
            builder.append( function ).append( '(' ).append( name ).append( ')' );
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
        }
    }
}
