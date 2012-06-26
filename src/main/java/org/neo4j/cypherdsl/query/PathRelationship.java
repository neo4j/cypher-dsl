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

import java.io.Serializable;
import java.util.Arrays;
import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.Expression;

import static org.neo4j.cypherdsl.CypherQuery.*;

/**
 * TODO
 */
public class PathRelationship
    implements AsString, Serializable, Cloneable
{
    public Path leftNode;
    public Direction direction = null; // null indicates that the path is only a start-node
    public Identifier as;
    public Iterable<Identifier> relationships;
    public PropertyValues relationshipPropertyValues;
    public boolean optional;
    public Integer minHops;
    public Integer maxHops;

    public PathRelationship( Path leftNode, Direction direction, Iterable<Identifier> relationships )
    {
        this.leftNode = leftNode;
        this.direction = direction;
        this.relationships = relationships;
    }

    public PathRelationship values(PropertyValue... propertyValues)
    {
        relationshipPropertyValues = new PropertyValues( Arrays.asList( propertyValues ) );
        return this;
    }

    public PathRelationship values(Iterable<PropertyValue> propertyValues)
    {
        relationshipPropertyValues = new PropertyValues( propertyValues );
        return this;
    }

    public PathRelationship as( String name )
    {
        return as( CypherQuery.identifier( name ) );
    }

    public PathRelationship as( Identifier name )
    {
        Query.checkNull( name, "Name" );
        this.as = name;
        return this;
    }

    public PathRelationship optional()
    {
        this.optional = true;
        return this;
    }

    public PathRelationship hops( Integer minHops, Integer maxHops )
    {
        if( minHops != null && minHops < 0 )
        {
            throw new IllegalArgumentException( "Minimum number of hops must be over zero" );
        }

        if( maxHops != null && maxHops < 0 )
        {
            throw new IllegalArgumentException( "Maximum number of hops must be over zero" );
        }

        this.minHops = minHops;
        this.maxHops = maxHops;
        return this;
    }

    public Path node()
    {
        return new Path(null, this);
    }

    public Path node(String id)
    {
        return node( identifier( id ));
    }

    public Path node(Expression id)
    {
        return new Path(id, this);
    }

    @Override
    public void asString( StringBuilder builder )
    {
        leftNode.asString( builder );

        builder.append( direction.equals( Direction.IN ) ? "<-" : "-" );

        boolean hasRelationships = relationships.iterator().hasNext();
        if( as != null || hasRelationships || optional || minHops != null || maxHops != null )
        {
            builder.append( '[' );
            if( as != null )
            {
                as.asString( builder );
            }
            if( optional )
            {
                builder.append( '?' );
            }
            if( hasRelationships )
            {
                builder.append( ':' );
                String or = "";
                for( Identifier relationship : relationships )
                {
                    builder.append( or );
                    relationship.asString( builder );
                    or = "|";
                }

                if (relationshipPropertyValues != null)
                {
                    builder.append( ' ' );
                    relationshipPropertyValues.asString( builder );
                }
            }

            if( minHops != null || maxHops != null )
            {
                builder.append( '*' );
                if( minHops != null )
                {
                    builder.append( minHops );
                }
                builder.append( ".." );
                if( maxHops != null )
                {
                    builder.append( maxHops );
                }
            }

            builder.append( ']' );
        }

        builder.append( direction.equals( Direction.OUT ) ? "->" : "-" );
    }
}
