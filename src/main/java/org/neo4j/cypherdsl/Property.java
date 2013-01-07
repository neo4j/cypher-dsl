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

import static org.neo4j.cypherdsl.query.NullHandling.FALSE_IF_MISSING;
import static org.neo4j.cypherdsl.query.NullHandling.TRUE_IF_MISSING;

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
    protected final NullHandling nullHandling;
    private final Identifier owner;
    private final Identifier name;

    Property( Identifier owner, Identifier name, NullHandling nullHandling )
    {
        super( new Operator( owner, "." ), name );
        this.nullHandling = nullHandling;
        this.owner = owner;
        this.name = name;
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
        return new Property( owner, name, FALSE_IF_MISSING );
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
        return new Property( owner, name, TRUE_IF_MISSING );
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
        return trueIfMissing();
    }

    @Override
    public void asString( StringBuilder builder )
    {
        super.asString( builder );
        AsString nullHandling1 = (AsString) nullHandling;
        nullHandling1.asString( builder );
    }
}
