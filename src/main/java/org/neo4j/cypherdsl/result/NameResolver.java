/**
 * Copyright (c) 2002-2012 "Neo Technology,"
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

import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;

/**
* Helps translating names in ExecutionResult to other names more suitable for JSON output or POJO mapping.
*/
public class NameResolver
    extends HashMap<String, String>
{
    public NameResolver replace(String from, String to)
    {
        put(from, to);
        return this;
    }

    public Iterable<Map<String, Object>> map(final Iterable<Map<String, Object>> result)
    {
        return new Iterable<Map<String, Object>>()
        {
            @Override
            public Iterator<Map<String, Object>> iterator()
            {
                final Iterator<Map<String, Object>> iter = result.iterator();

                return new Iterator<Map<String,Object>>()
                {
                    Map<String, Object> newMap = new LinkedHashMap<String, Object>();

                    @Override
                    public boolean hasNext()
                    {
                        return iter.hasNext();
                    }

                    @Override
                    public Map<String, Object> next()
                    {
                        Map<String, Object> map = iter.next();

                        newMap.clear();

                        for (Map.Entry<String, Object> stringObjectEntry : map.entrySet())
                        {
                            String key = get(stringObjectEntry.getKey());
                            if (key == null)
                                key = stringObjectEntry.getKey();
                            newMap.put(key, stringObjectEntry.getValue());
                        }

                        return newMap;
                    }

                    @Override
                    public void remove()
                    {
                    }
                };
            }
        };
    }
}
