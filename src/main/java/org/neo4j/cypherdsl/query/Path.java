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

import java.util.Arrays;
import org.neo4j.cypherdsl.CypherQuery;

/**
* TODO
*/
public class Path<T extends Path>
    extends AbstractPath<T>
{
    public Identifier pathName;
    public Expression from;
    public Expression fromPropertyValues;

    @Override
    public void asString( StringBuilder builder )
    {
        if (pathName != null)
        {
            pathName.asString( builder );
            builder.append( '=' );
        }
        builder.append( '(' );
        if( from != null )
        {
            from.asString( builder );

            // TODO Should it be allowed to create nodes which have no names?
            if ( fromPropertyValues != null)
            {
                builder.append( ' ');
                fromPropertyValues.asString( builder );
            }
        }
        builder.append( ')' );
        super.asString( builder );
    }

    public T from(String from, PropertyValue... propertyValues)
    {
        return from( CypherQuery.identifier( from ), propertyValues);
    }

    public T from(String from, Iterable<PropertyValue> propertyValues)
    {
        return from( CypherQuery.identifier( from ), propertyValues);
    }

    public T from(Expression from, PropertyValue... propertyValues)
    {
        return from(from, Arrays.asList(propertyValues));
    }

    public T from(Expression from, Iterable<PropertyValue> propertyValues)
    {
        Query.checkNull(from, "From");
        this.from = from;

        if (propertyValues.iterator().hasNext())
            this.fromPropertyValues = new PropertyValues( propertyValues );

        return (T) this;
    }

    public T from(String from, Parameter parameterWithProperties)
    {
        return from( CypherQuery.identifier( from ), parameterWithProperties);
    }

    public T from(Identifier from, Parameter parameterWithProperties)
    {
        Query.checkNull(from, "From");
        this.from = from;
        this.fromPropertyValues = parameterWithProperties;

        return (T) this;
    }

}
