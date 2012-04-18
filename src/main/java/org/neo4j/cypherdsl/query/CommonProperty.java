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

package org.neo4j.cypherdsl.query;

import org.neo4j.cypherdsl.CypherQuery;

/**
* TODO
*/
public class CommonProperty
    extends Property<CommonProperty>
{
    public Regexp regexp(String value)
    {
        return regexp( value, true );
    }

    public Regexp regexp(String value, boolean caseSensitive)
    {
        Query.checkEmpty( value, "Regular expression" );

        Regexp regexp1 = new Regexp();
        regexp1.left = this;
        regexp1.regexp = CypherQuery.literal( value );
        regexp1.caseSensitive = caseSensitive;
        return regexp1;
    }

    public StringProperty asString()
    {
        StringProperty type = new StringProperty();
        type.owner = owner;
        type.name = name;
        return type;
    }

    public NumberProperty asNumber()
    {
        NumberProperty type = new NumberProperty();
        type.owner = owner;
        type.name = name;
        return type;
    }
}
