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
package org.neo4j.cypherdsl;

import static java.util.Arrays.asList;
import static org.neo4j.cypherdsl.CypherQuery.identifier;

import java.io.Serializable;

import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.query.Direction;
import org.neo4j.cypherdsl.query.PropertyValue;
import org.neo4j.cypherdsl.query.PropertyValues;
import org.neo4j.cypherdsl.query.Query;

/**
 * Represents a relationship in a path.
 */
public class PathRelationship
        implements AsString, Serializable, Cloneable
{
    public final Path leftNode;
    public final Direction direction; // null indicates that the path is only a start-node
    public final Identifier as;
    public final Iterable<Identifier> relationships;
    public final PropertyValues relationshipPropertyValues;
    public final Integer minHops;
    public final Integer maxHops;

    PathRelationship(Path leftNode, Direction direction, Identifier as, Iterable<Identifier> relationships,
                     PropertyValues relationshipPropertyValues, Integer minHops, Integer maxHops)
    {
        this.leftNode = leftNode;
        this.direction = direction;
        this.as = as;
        this.relationships = relationships;
        this.relationshipPropertyValues = relationshipPropertyValues;
        this.minHops = minHops;
        this.maxHops = maxHops;
    }

    /**
     * If this relationship is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values.
     * Use e.g. {@link CypherQuery.value( String,Object )} to create
     * the individual values to be passed in here.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n)-[:relationship {prop1:value1,prop2:value2})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public PathRelationship values( PropertyValue... propertyValues )
    {
        return new PathRelationship( leftNode, direction, as, relationships,
                new PropertyValues( asList( propertyValues ) ), minHops, maxHops );
    }

    /**
     * If this relationship is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values.
     * Use e.g. {@link CypherQuery.value( String,Object )} to create
     * the individual values to be passed in here.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n)-[:relationship {prop1:value1,prop2:value2})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public PathRelationship values( Iterable<PropertyValue> propertyValues )
    {
        return new PathRelationship( leftNode, direction, as, relationships, new PropertyValues( propertyValues ),
                minHops, maxHops );
    }

    /**
     * Use this method to name a relationship
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[name]-(m)
     * </pre>
     *
     * @param name
     * @return
     */
    public PathRelationship as( String name )
    {
        return as( CypherQuery.identifier( name ) );
    }

    /**
     * Use this method to name a relationship
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[name]-(m)
     * </pre>
     *
     * @param name
     * @return
     */
    public PathRelationship as( Identifier name )
    {
        Query.checkNull( name, "Name" );
        return new PathRelationship( leftNode, direction, name, relationships, relationshipPropertyValues,
                minHops, maxHops );
    }

    /**
     * Use this method to declare how many hops are allowed. You can either specify
     * min, max or both.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:*minHops,maxHops]-(m)
     * </pre>
     *
     * @param minHops
     * @param maxHops
     * @return
     */
    public PathRelationship hops( Integer minHops, Integer maxHops )
    {
        if ( minHops != null && minHops < 0 )
        {
            throw new IllegalArgumentException( "Minimum number of hops must be over zero" );
        }

        if ( maxHops != null && maxHops < 0 )
        {
            throw new IllegalArgumentException( "Maximum number of hops must be over zero" );
        }

        return new PathRelationship( leftNode, direction, as, relationships, relationshipPropertyValues,
                minHops, maxHops );
    }

    /**
     * Declare the end node of this path. This must be called before using
     * this expression in any clause, as otherwise you would not have a complete
     * path.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)--()
     * </pre>
     *
     * @return
     */
    public Path node()
    {
        return new Path( null, this, null, null );
    }

    /**
     * Declare the end node of this path. This must be called before using
     * this expression in any clause, as otherwise you would not have a complete
     * path.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)--(id)
     * </pre>
     *
     * @return
     */
    public Path node( String id )
    {
        return node( identifier( id ) );
    }

    /**
     * Declare the end node of this path. This must be called before using
     * this expression in any clause, as otherwise you would not have a complete
     * path.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)--(id)
     * </pre>
     *
     * @return
     */
    public Path node( Expression id )
    {
        return new Path( id, this, null, null );
    }

    @Override
    public void asString( StringBuilder builder )
    {
        leftNode.asString( builder );

        builder.append( direction.equals( Direction.IN ) ? "<-" : "-" );

        boolean hasRelationships = relationships.iterator().hasNext();
        if ( as != null || hasRelationships || minHops != null || maxHops != null || relationshipPropertyValues!= null )
        {
            builder.append( '[' );
            if ( as != null )
            {
                as.asString(builder);
            }
            if ( hasRelationships )
            {
                builder.append( ':' );
                String or = "";
                for ( Identifier relationship : relationships )
                {
                    builder.append( or );
                    relationship.asString( builder );
                    or = "|";
                }
            }

            if ( minHops != null || maxHops != null )
            {
                builder.append( '*' );
                if ( minHops != null )
                {
                    builder.append( minHops );
                }
                builder.append( ".." );
                if ( maxHops != null )
                {
                    builder.append( maxHops );
                }
            }

            if ( relationshipPropertyValues != null )
            {
                builder.append( ' ' );
                relationshipPropertyValues.asString( builder );
            }

            builder.append( ']' );
        }

        builder.append( direction.equals( Direction.OUT ) ? "->" : "-" );
    }
}
