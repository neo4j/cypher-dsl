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
import org.neo4j.cypherdsl.Expression;
import org.neo4j.cypherdsl.ReferenceExpression;
import org.neo4j.cypherdsl.ScalarExpression;

/**
* TODO
*/
public class Property
    extends Value
    implements ReferenceExpression
{
    protected NullHandling nullHandling = NullHandling.NULL;

    protected Property( Identifier owner,
                        Identifier name
    )
    {
        super( new Operator(owner, "."), name );
    }

    public Property falseIfMissing()
    {
        nullHandling = NullHandling.FALSE_IF_MISSING;
        return (Property) this;
    }

    public Property trueIfMissing()
    {
        nullHandling = NullHandling.TRUE_IF_MISSING;
        return (Property) this;
    }

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
