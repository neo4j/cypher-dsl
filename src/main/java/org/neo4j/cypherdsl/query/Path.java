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
import java.util.Collections;
import org.neo4j.cypherdsl.BooleanExpression;
import org.neo4j.cypherdsl.CollectionExpression;
import org.neo4j.cypherdsl.CypherQuery;
import org.neo4j.cypherdsl.Expression;
import org.neo4j.cypherdsl.PathExpression;

/**
* TODO
*/
public class Path
    extends AbstractExpression
    implements PathExpression
{
    private Expression node;
    private Expression nodePropertyValues;
    private PathRelationship relationship;

    public Path( Expression node, PathRelationship relationship )
    {
        this.node = node;
        this.relationship = relationship;
    }

    public Path values(PropertyValue... propertyValues)
    {
        nodePropertyValues = new PropertyValues( Arrays.asList( propertyValues ) );
        return this;
    }

    public Path values(Iterable<PropertyValue> propertyValues)
    {
        nodePropertyValues = new PropertyValues( propertyValues );
        return this;
    }

    public Path values(Parameter propertyValues)
    {
        nodePropertyValues = propertyValues;
        return this;
    }

    public PathRelationship out()
    {
        return new PathRelationship( this, Direction.OUT, Collections.<Identifier>emptyList());
    }

    public PathRelationship out(String... relationships)
    {
        return new PathRelationship( this, Direction.OUT, Arrays.asList( CypherQuery.identifiers( relationships ) ));
    }

    public PathRelationship out(Identifier... relationships)
    {
        return new PathRelationship( this, Direction.OUT, Arrays.asList( relationships ) );
    }

    public PathRelationship in()
    {
        return new PathRelationship( this, Direction.IN, Collections.<Identifier>emptyList());
    }

    public PathRelationship in(String... relationships)
    {
        return new PathRelationship( this, Direction.IN, Arrays.asList( CypherQuery.identifiers( relationships ) ) );
    }

    public PathRelationship in(Identifier... relationships)
    {
        return new PathRelationship( this, Direction.IN, Arrays.asList( relationships ));
    }

    public PathRelationship both()
    {
        return new PathRelationship( this, Direction.BOTH, Collections.<Identifier>emptyList());
    }

    public PathRelationship both( String... relationships)
    {
        return new PathRelationship( this, Direction.BOTH, Arrays.asList( CypherQuery.identifiers( relationships ) ) );
    }

    public PathRelationship both( Identifier... relationships)
    {
        return new PathRelationship( this, Direction.BOTH, Arrays.asList( relationships));
    }

    @Override
    public void asString( StringBuilder builder )
    {
        if( relationship != null )
        {
            relationship.asString( builder );
        }

        builder.append( '(' );
        if( node != null )
        {
            node.asString( builder );

            if ( nodePropertyValues != null)
            {
                builder.append( ' ');
                nodePropertyValues.asString( builder );
            }
        } else
        {
            if ( nodePropertyValues != null)
            {
                nodePropertyValues.asString( builder );
            }
        }
        builder.append( ')' );
    }
}
