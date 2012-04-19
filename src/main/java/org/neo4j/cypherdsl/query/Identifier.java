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

import java.util.regex.Pattern;
import org.neo4j.cypherdsl.CypherQuery;

import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.query.Query.checkNull;

/**
* TODO
*/
public class Identifier
    extends Expression
{
    private static Pattern simpleName = Pattern.compile( "\\w*" );

    public String name;

    public CommonProperty property(String name)
    {
        return property( identifier( name ) );
    }

    public CommonProperty property(Identifier name)
    {
        checkNull( name, "Name" );
        CommonProperty property = new CommonProperty();
        property.owner = this;
        property.name = name;
        return property;
    }

    public StringProperty string(String name)
    {
        return string( identifier( name ) );
    }

    public StringProperty string(Identifier name)
    {
        checkNull( name, "Name" );
        StringProperty stringProperty = new StringProperty();
        stringProperty.owner = this;
        stringProperty.name = name;
        return stringProperty;
    }

    public NumberProperty number(String name)
    {
        return number( identifier( name ) );
    }

    public NumberProperty number(Identifier name)
    {
        checkNull( name, "Name" );
        NumberProperty numberProperty = new NumberProperty();
        numberProperty.owner = this;
        numberProperty.name = name;
        return numberProperty;
    }

    public IsNull isNull()
    {
        IsNull isNull = new IsNull();
        isNull.expression = this;
        return isNull;
    }

    public IsNotNull isNotNull()
    {
        IsNotNull isNotNull = new IsNotNull();
        isNotNull.expression = this;
        return isNotNull;
    }

    public void asString( StringBuilder builder )
    {
        if (simpleName.matcher( name ).matches())
            builder.append( name );
        else
            builder.append( '`' ).append( name ).append( '`' );
    }
}
