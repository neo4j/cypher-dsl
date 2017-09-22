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
package org.neo4j.cypherdsl.query.clause;

import java.io.Serializable;
import java.util.List;

import org.neo4j.cypherdsl.AsString;

/**
 * Base class for all clauses
 */
public abstract class Clause
        implements AsString, Serializable, Cloneable
{
    protected void clauseAsString( StringBuilder builder, String name, List<? extends AsString> asStringList,
                                   String separator )
    {
        if ( !asStringList.isEmpty() )
        {
            if ( builder.length() > 0 )
            {
                builder.append( ' ' );
            }
            builder.append( name ).append( ' ' );

            for ( int i = 0; i < asStringList.size(); i++ )
            {
                AsString asString = asStringList.get( i );
                if ( i > 0 )
                {
                    builder.append( separator );
                }
                asString.asString( builder );
            }
        }
    }
}
