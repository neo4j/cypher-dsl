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

import java.util.regex.Pattern;

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

    public static Identifier identifier( String name )
    {
        Identifier identifier = new Identifier();
        identifier.name = name;
        return identifier;
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

    public static Literal[] literals( long[] values )
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
