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
package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.query.AbstractExpression;
import org.neo4j.cypherdsl.query.Value;

/**
 * Represents a literal value, such as a string or number.
 */
public class Literal
        extends Value
{
    Literal( Object value )
    {
        super( new LiteralExpression( value ) );
    }

    private static class LiteralExpression
            extends AbstractExpression
    {
        public final Object value;

        private LiteralExpression( Object value )
        {
            this.value = value;
        }

        @Override
        public void asString( StringBuilder builder )
        {
            if ( value instanceof String )
            {
                builder.append( "\"" ).append( value.toString().replace( "\\", "\\\\" ).replace( "\"",
                        "\\\"" ) ).append( "\"" );
            }
            else
            {
                builder.append( value.toString() );
            }
        }

        @Override
        public String toString()
        {
            return value.toString();
        }
    }
}
