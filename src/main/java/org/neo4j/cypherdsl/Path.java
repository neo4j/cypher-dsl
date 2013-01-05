/**
 * Copyright (c) 2002-2013 "Neo Technology,"
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

package org.neo4j.cypherdsl;

import static java.util.Arrays.asList;
import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.CypherQuery.identifiers;
import static org.neo4j.cypherdsl.query.Direction.BOTH;
import static org.neo4j.cypherdsl.query.Direction.IN;
import static org.neo4j.cypherdsl.query.Direction.OUT;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.neo4j.cypherdsl.expression.Expression;
import org.neo4j.cypherdsl.expression.PathExpression;
import org.neo4j.cypherdsl.query.AbstractExpression;
import org.neo4j.cypherdsl.query.PropertyValue;
import org.neo4j.cypherdsl.query.PropertyValues;

/**
 * Represents either a single node or a path from one node to another.
 */
public class Path
        extends AbstractExpression
        implements PathExpression
{
    private final Expression node;
    private final Expression nodePropertyValues;
    private final PathRelationship relationship;

    Path( Expression node, PathRelationship relationship, Expression nodePropertyValues )
    {
        this.node = node;
        this.relationship = relationship;
        this.nodePropertyValues = nodePropertyValues;
    }

    /**
     * If this node is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values.
     * Use e.g. {@link CypherQuery.value( String,Object )} to create
     * the individual values to be passed in here.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n {prop1:value1,prop2:value2})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public Path values( PropertyValue... propertyValues )
    {
        return new Path( node, relationship, new PropertyValues( asList( propertyValues ) ) );
    }

    /**
     * If this node is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values.
     * Use e.g. {@link CypherQuery.value( String,Object )} to create
     * the individual values to be passed in here.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n {prop1:value1,prop2:value2})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public Path values( Iterable<PropertyValue> propertyValues )
    {
        return new Path( node, relationship, new PropertyValues( propertyValues ) );
    }

    /**
     * If this node is used in a CREATE or CREATE UNIQUE clause,
     * then you can use this method to specify property values which
     * should be taken from a map parameter.
     * <p/>
     * Corresponds to:
     * <pre>
     *     (n {propertyValues})
     * </pre>
     *
     * @param propertyValues
     * @return
     */
    public Path values( Parameter propertyValues )
    {
        return new Path( node, relationship, propertyValues );
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out()
    {
        return new PathRelationship( this, OUT, null, Collections.<Identifier>emptyList(), null, false, null, null );
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out( String... relationships )
    {
        return new PathRelationship( this, OUT, null, asList( identifiers( relationships ) ), null, false, null, null );
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out( Identifier... relationships )
    {
        return new PathRelationship( this, OUT, null, asList( relationships ), null, false, null, null );
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]->(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship out( Enum<?>... relationships )
    {
        List<Identifier> relationshipNames = new ArrayList<Identifier>();
        for ( Enum<?> relationship : relationships )
        {
            relationshipNames.add( identifier( relationship.name() ) );
        }

        return new PathRelationship( this, OUT, null, relationshipNames, null, false, null, null );
    }

    /**
     * Declare a new outgoing relationship from this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<--(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in()
    {
        return new PathRelationship( this, IN, null, Collections.<Identifier>emptyList(), null, false, null, null );
    }

    /**
     * Declare a new incoming relationship to this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in( String... relationships )
    {
        return new PathRelationship( this, IN, null, asList( identifiers( relationships ) ), null, false, null, null );
    }

    /**
     * Declare a new incoming relationship to this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in( Identifier... relationships )
    {
        return new PathRelationship( this, IN, null, asList( relationships ), null, false, null, null );
    }

    /**
     * Declare a new incoming relationship to this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)<-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship in( Enum<?>... relationships )
    {
        List<Identifier> relationshipNames = new ArrayList<Identifier>();
        for ( Enum<?> relationship : relationships )
        {
            relationshipNames.add( identifier( relationship.name() ) );
        }

        return new PathRelationship( this, IN, null, relationshipNames, null, false, null, null );
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)--(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both()
    {
        return new PathRelationship( this, BOTH, null, Collections.<Identifier>emptyList(), null, false, null, null );
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both( String... relationships )
    {
        return new PathRelationship( this, BOTH, null, asList( identifiers( relationships ) ), null, false, null,
                null );
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both( Identifier... relationships )
    {
        return new PathRelationship( this, BOTH, null, asList( relationships ), null, false, null, null );
    }

    /**
     * Declare a new relationship on this node.
     * <p/>
     * Corresponds to:
     * <pre>
     * (n)-[:relationship1|relationship2]-(m)
     * </pre>
     *
     * @return
     */
    public PathRelationship both( Enum<?>... relationships )
    {
        List<Identifier> relationshipNames = new ArrayList<Identifier>();
        for ( Enum<?> relationship : relationships )
        {
            relationshipNames.add( identifier( relationship.name() ) );
        }

        return new PathRelationship( this, BOTH, null, relationshipNames, null, false, null, null );
    }

    @Override
    public void asString( StringBuilder builder )
    {
        if ( relationship != null )
        {
            relationship.asString( builder );
        }

        builder.append( '(' );
        if ( node != null )
        {
            node.asString( builder );

            if ( nodePropertyValues != null )
            {
                builder.append( ' ' );
                nodePropertyValues.asString( builder );
            }
        }
        else
        {
            if ( nodePropertyValues != null )
            {
                nodePropertyValues.asString( builder );
            }
        }
        builder.append( ')' );
    }
}
