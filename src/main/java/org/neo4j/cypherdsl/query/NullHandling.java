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

import org.neo4j.cypherdsl.AsString;

/**
 * Null handling for property references
 */
public enum NullHandling
        implements AsString
{
    NULL
            {
                @Override
                public void asString( StringBuilder builder )
                {
                }
            },
    TRUE_IF_MISSING
            {
                @Override
                public void asString( StringBuilder builder )
                {
                    builder.append( "? " );
                }
            },
    FALSE_IF_MISSING
            {
                @Override
                public void asString( StringBuilder builder )
                {
                    // Add a space after the exclamation mark because otherwise cypher
                    // gives an error message saying that "!=" is not a valid inequality comparator
                    builder.append( "! " );
                }
            }
}
