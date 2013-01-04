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

import org.neo4j.cypherdsl.expression.NumericExpression;
import org.neo4j.cypherdsl.expression.ReferenceExpression;
import org.neo4j.cypherdsl.expression.StringExpression;
import org.neo4j.cypherdsl.query.AbstractExpression;
import org.neo4j.cypherdsl.query.Value;

import java.util.regex.Pattern;

import static org.neo4j.cypherdsl.CypherQuery.identifier;
import static org.neo4j.cypherdsl.query.Query.checkNull;

/**
 * Represents an identifier. If the identifier is a node or relationship,
 * then you can use the property() method to get a representation of individual properties.
 */
public class Identifier
        extends Value
        implements ReferenceExpression
{
    private static Pattern simpleName = Pattern.compile( "\\p{Alpha}\\w*" );

    Identifier( String name )
    {
        super( new IdentifierExpression( name ) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public Property property( String name )
    {
        return property( identifier( name ) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public Property property( Identifier name )
    {
        checkNull( name, "Name" );
        return new Property( this, name );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a string property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public StringExpression string( String name )
    {
        return string( identifier( name ) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a string property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public StringExpression string( Identifier name )
    {
        checkNull( name, "Name" );
        return new Property( this, name );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a numeric property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public NumericExpression number( String name )
    {
        return number( identifier( name ) );
    }

    /**
     * If this identifier represents a node or relationship,
     * then you can use this method to denote a numeric property.
     * <p/>
     * Corresponds to:
     * <pre>
     * id.name
     * </pre>
     *
     * @param name
     * @return
     */
    public NumericExpression number( Identifier name )
    {
        checkNull( name, "Name" );
        return new Property( this, name );
    }

    private static class IdentifierExpression
            extends AbstractExpression
    {
        private String name;

        private IdentifierExpression( String name )
        {
            this.name = name;
        }

        public void asString( StringBuilder builder )
        {
            if ( simpleName.matcher( name ).matches() )
            {
                builder.append( name );
            }
            else
            {
                builder.append( '`' ).append( name ).append( '`' );
            }
        }
    }
}
