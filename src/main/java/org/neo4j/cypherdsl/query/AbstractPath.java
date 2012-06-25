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
public class AbstractPath<T extends AbstractPath>
    extends MatchExpression
{
    public Identifier to;
    public Expression toPropertyValues;
    public Direction direction = null; // null indicates that the path is only a start-node
    public Identifier as;
    public Expression[] relationships = new Expression[0];
    public PropertyValues relationshipPropertyValues;
    public boolean optional;
    public Integer minHops;
    public Integer maxHops;

    @Override
    public void asString( StringBuilder builder )
    {
        if (direction == null)
            return;

        builder.append( direction.equals( Direction.IN ) ? "<-" : "-" );

        if( as != null || relationships.length > 0 || optional || minHops != null || maxHops != null )
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
            if( relationships.length > 0 )
            {
                builder.append( ':' );
                for( int i = 0; i < relationships.length; i++ )
                {
                    if( i > 0 )
                    {
                        builder.append( "|" );
                    }
                    relationships[ i ].asString( builder );
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

        builder.append( '(' );
        if( to != null )
        {
            to.asString( builder );

            if (toPropertyValues != null)
            {
                builder.append( ' ' );
                toPropertyValues.asString( builder );
            }
        }
        builder.append( ')' );
    }

    public T out(String... relationship)
    {
        this.direction = Direction.OUT;
        this.relationships = CypherQuery.identifiers( relationship );
        return (T) this;
    }

    public T out(Identifier relationship, PropertyValue... propertyValues)
    {
        this.direction = Direction.OUT;
        this.relationships = CypherQuery.identifiers( relationship.name );
        if (propertyValues.length > 0)
            relationshipPropertyValues = new PropertyValues( Arrays.asList( propertyValues ) );
        return (T) this;
    }

    public T in(String... relationship)
    {
        this.direction = Direction.IN;
        this.relationships = CypherQuery.identifiers(relationship);
        return (T) this;
    }

    public T in(Identifier relationship, PropertyValue... propertyValues)
    {
        this.direction = Direction.IN;
        this.relationships = CypherQuery.identifiers(relationship.name);
        if (propertyValues.length > 0)
            relationshipPropertyValues = new PropertyValues( Arrays.asList( propertyValues ) );
        return (T) this;
    }

    public T both( String... relationship)
    {
        this.direction = Direction.BOTH;
        this.relationships = CypherQuery.identifiers(relationship);
        return (T) this;
    }

    public T both( Identifier relationship, PropertyValue... propertyValues )
    {
        this.direction = Direction.BOTH;
        this.relationships = CypherQuery.identifiers(relationship.name);
        if (propertyValues.length > 0)
            relationshipPropertyValues = new PropertyValues( Arrays.asList( propertyValues ) );
        return (T) this;
    }

    public T as( String name )
    {
        return as( CypherQuery.identifier( name ) );
    }

    public T as( Identifier name )
    {
        Query.checkNull( name, "Name" );
        this.as = name;
        return (T) this;
    }

    public T optional()
    {
        this.optional = true;
        return (T) this;
    }

    public T hops( Integer minHops, Integer maxHops )
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
        return (T) this;
    }

    public T to(String to, PropertyValue... propertyValues)
    {
        return to(CypherQuery.identifier( to ), propertyValues);
    }

    public T to(String to, Iterable<PropertyValue> propertyValues)
    {
        return to(CypherQuery.identifier( to ), propertyValues);
    }

    public T to(Identifier to, PropertyValue... propertyValues)
    {
        return to(to, Arrays.asList(propertyValues));
    }

    public T to(Identifier to, Iterable<PropertyValue> propertyValues)
    {
        Query.checkNull( to, "To" );
        this.to = to;

        if (propertyValues.iterator().hasNext())
            this.toPropertyValues = new PropertyValues( propertyValues );

        return (T) this;
    }

    public T to(String to, Parameter parameterWithProperties)
    {
        return to( CypherQuery.identifier( to ), parameterWithProperties );
    }

    public T to(Identifier to, Parameter parameterWithProperties)
    {
        Query.checkNull(to, "To");
        this.to = to;
        this.toPropertyValues = parameterWithProperties;

        return (T) this;
    }

    public Link link()
    {
        Link link = new Link();
        link.leftPath = this;

        return link;
    }
}
