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
package org.neo4j.cypherdsl.expression;

/**
 * Expression that evaluates to a string
 */
public interface StringExpression
        extends ScalarExpression
{
    public BooleanExpression gt( String expression );

    public BooleanExpression gt( StringExpression expression );

    public BooleanExpression gte( String expression );

    public BooleanExpression gte( StringExpression expression );

    public BooleanExpression lt( String expression );

    public BooleanExpression lt( StringExpression expression );

    public BooleanExpression lte( String expression );

    public BooleanExpression lte( StringExpression expression );

    public BooleanExpression regexp( String regexp );

    public BooleanExpression regexp( StringExpression regexp );

    public BooleanExpression regexp( String regexp, boolean caseSensitive );

    public StringExpression concat( String expression );

    public StringExpression concat( StringExpression expression );
}
