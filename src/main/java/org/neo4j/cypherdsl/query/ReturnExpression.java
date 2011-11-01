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
        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "count";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate sum(String name)
    {
        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "sum";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate avg(String name)
    {
        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "avg";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate max(String name)
    {
        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "max";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate min(String name)
    {
        ReturnAggregate returnAggregate = new ReturnAggregate();
        returnAggregate.function = "min";
        returnAggregate.name = name;
        return returnAggregate;
    }

    public static ReturnAggregate collect(String name)
    {
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
