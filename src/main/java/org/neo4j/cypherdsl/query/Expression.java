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

/**
 * Common methods for all expressions
 */
public class Expression
{
    public static Parameter param(String name)
    {
        Parameter parameter = new Parameter();
        parameter.name = name;
        return parameter;
    }

    public static Literal literal( Object value )
    {
        Literal literal = new Literal();
        literal.value = value;
        return literal;
    }

    public static Literal[] literals( Object[] values )
    {
        Literal[] literals = new Literal[values.length];
        for( int i = 0; i < values.length; i++ )
        {
            Object value = values[ i ];
            Literal literal = new Literal();
            literal.value = value;
            literals[i] = literal;
        }
        return literals;
    }

    public static Parameter[] parameters( String[] names )
    {
        Parameter[] parameters = new Parameter[names.length];
        for( int i = 0; i < names.length; i++ )
        {
            String value = names[ i ];
            Parameter parameter = new Parameter();
            parameter.name = value;
            parameters[i] = parameter;
        }
        return parameters;
    }

    public static Literal[] literals( int[] values )
    {
        Literal[] literals = new Literal[values.length];
        for( int i = 0; i < values.length; i++ )
        {
            Object value = values[ i ];
            Literal literal = new Literal();
            literal.value = value;
            literals[i] = literal;
        }
        return literals;
    }

    public abstract static class Value
        implements AsString
    {
    }

    public static class Parameter
        extends Value
    {
        public String name;

        @Override
        public void asString( StringBuilder builder )
        {
            builder.append( '{' ).append( name ).append( '}' );
        }
    }

    public static class Literal
        extends Value
    {
        public Object value;

        @Override
        public void asString( StringBuilder builder )
        {
            if (value instanceof String)
                builder.append( "\"" ).append( value.toString() ).append( "\"" );
            else
                builder.append( value.toString() );
        }
    }

    public static class Identifier
        implements AsString
    {
        String name;

        public void asString( StringBuilder builder )
        {
            builder.append( name );
        }
    }

}
