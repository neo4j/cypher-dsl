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
package org.neo4j.cypherdsl.result;

import java.util.Iterator;
import java.util.Map;

import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.node.ArrayNode;
import org.codehaus.jackson.node.ObjectNode;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Path;

/**
 * Serializer that converts Cypher execution results to JSON.
 */
public class JSONSerializer
{
    private final ObjectMapper mapper = new ObjectMapper();

    public ArrayNode toJSON( Iterator<Map<String, Object>> result )
    {
        ArrayNode root = mapper.createArrayNode();

        while (result.hasNext())
        {
            Map<String, Object> row = result.next();
            ObjectNode entry = root.objectNode();

            for ( Map.Entry<String, Object> stringObjectEntry : row.entrySet() )
            {
                if ( stringObjectEntry.getValue() instanceof Path )
                {
                    entry.put( stringObjectEntry.getKey(), stringObjectEntry.getValue().toString() );
                }
                else if ( stringObjectEntry.getValue() instanceof Node )
                {
                    Node node = (Node) stringObjectEntry.getValue();
                    ObjectNode nodeNode = entry.objectNode();
                    nodeNode.put( "_id", node.getId() );
                    for ( String propertyName : node.getPropertyKeys() )
                    {
                        addProperty( nodeNode, propertyName, node.getProperty( propertyName ) );
                    }
                    entry.put( stringObjectEntry.getKey(), nodeNode );
                }
                else
                {
                    addProperty( entry, stringObjectEntry.getKey(), stringObjectEntry.getValue() );
                }
            }

            root.add( entry );
        }
        return root;
    }

    private void addProperty( ObjectNode node, String name, Object value )
    {
        if ( value instanceof String )
        {
            node.put( name, value.toString() );
        }
        else if ( value instanceof Long )
        {
            Long number = (Long) value;
            node.put( name, number );
        }
        else if ( value instanceof Integer )
        {
            Integer number = (Integer) value;
            node.put( name, number );
        }
        else
        {
            throw new IllegalArgumentException( "Unknown value type:" + value.getClass() );
        }
    }
}
