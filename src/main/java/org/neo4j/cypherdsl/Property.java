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

package org.neo4j.cypherdsl;

import org.neo4j.cypherdsl.expression.ReferenceExpression;
import org.neo4j.cypherdsl.query.NullHandling;
import org.neo4j.cypherdsl.query.Operator;
import org.neo4j.cypherdsl.query.Value;

/**
 * Represents a property reference.
 */
public class Property
        extends Value
        implements ReferenceExpression
{
    protected NullHandling nullHandling = NullHandling.NULL;

    Property( Identifier owner, Identifier name )
    {
        super( new Operator( owner, "." ), name );
    }

    /**
     * Use this method to declare that this property reference
     * should be treated as false if the property is missing.
     * <p/>
     * Corresponds to:
     * <pre>
     * n.prop!
     * </pre>
     *
     * @return
     */
    public Property falseIfMissing()
    {
        nullHandling = NullHandling.FALSE_IF_MISSING;
        return (Property) this;
    }

    /**
     * Use this method to declare that this property reference
     * should be treated as true if the property is missing.
     * <p/>
     * Corresponds to:
     * <pre>
     * n.prop?
     * </pre>
     *
     * @return
     */
    public Property trueIfMissing()
    {
        nullHandling = NullHandling.TRUE_IF_MISSING;
        return (Property) this;
    }

    /**
     * Use this method to declare that this property reference
     * should be treated as true if the property is missing.
     * <p/>
     * Corresponds to:
     * <pre>
     * n.prop?
     * </pre>
     *
     * @return
     */
    public Property optional()
    {
        nullHandling = NullHandling.TRUE_IF_MISSING;
        return (Property) this;
    }

    @Override
    public void asString( StringBuilder builder )
    {
        super.asString( builder );
        AsString nullHandling1 = (AsString) nullHandling;
        nullHandling1.asString( builder );
    }
}
