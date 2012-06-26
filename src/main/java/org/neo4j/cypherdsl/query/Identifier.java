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
import org.neo4j.cypherdsl.BooleanExpression;
import org.neo4j.cypherdsl.Expression;
import org.neo4j.cypherdsl.NumericExpression;
import org.neo4j.cypherdsl.ReferenceExpression;
import org.neo4j.cypherdsl.StringExpression;

import static org.neo4j.cypherdsl.CypherQuery.*;
import static org.neo4j.cypherdsl.query.Query.checkNull;

/**
* TODO
*/
public class Identifier
    extends Value
    implements ReferenceExpression
{
    private static Pattern simpleName = Pattern.compile( "\\w*" );

    public Identifier( String name )
    {
        super( new IdentifierExpression(name) );
    }

    public Property property(String name)
    {
        return property( identifier( name ) );
    }

    public Property property(Identifier name)
    {
        checkNull( name, "Name" );
        return new Property(this, name);
    }

    public StringExpression string(String name)
    {
        return string( identifier( name ) );
    }

    public StringExpression string(Identifier name)
    {
        checkNull( name, "Name" );
        return new Property(this, name);
    }

    public NumericExpression number(String name)
    {
        return number( identifier( name ) );
    }

    public NumericExpression number(Identifier name)
    {
        checkNull( name, "Name" );
        return new Property(this, name);
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
            if (simpleName.matcher( name ).matches())
                builder.append( name );
            else
                builder.append( '`' ).append( name ).append( '`' );
        }
    }
}
