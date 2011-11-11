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

package org.neo4j.cypherdsl.result;

import java.util.Map;
import org.neo4j.graphdb.Path;

/**
 * Serializer that converts Cypher execution results to JSON.
 */
public class JSONSerializer
{
    public String toJSON(Iterable<Map<String, Object>> result)
    {
        StringBuilder builder = new StringBuilder( );

        builder.append( "[" );
        boolean firstItem = true;
        for( Map<String, Object> stringObjectMap : result )
        {
            if (!firstItem)
                builder.append( ',' );
            firstItem = false;

            builder.append( "{" );

            boolean firstValue = true;
            for( Map.Entry<String, Object> stringObjectEntry : stringObjectMap.entrySet() )
            {
                if (!firstValue)
                    builder.append( ',' );
                firstValue = false;

                builder.append( '\"' ).append( stringObjectEntry.getKey() ).append( "\":" );

                if (stringObjectEntry.getValue() instanceof String)
                {
                    builder.append( "\"" ).append( stringObjectEntry.getValue() ).append( "\"" );
                } else if (stringObjectEntry.getValue() instanceof Path)
                {
                    builder.append( "\"" ).append( stringObjectEntry.getValue() ).append( "\"" );
                }else
                {
                    builder.append( stringObjectEntry.getValue() );
                }
            }


            builder.append( "}" );
        }
        builder.append( "]" );
        return builder.toString();
    }
}
