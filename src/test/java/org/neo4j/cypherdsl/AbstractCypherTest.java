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

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.neo4j.cypher.MissingIndexException;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.test.ImpermanentGraphDatabase;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public abstract class AbstractCypherTest {

    public static final String CYPHER = "CYPHER 1.8 ";
    private static ImpermanentGraphDatabase graphdb;
    protected static ExecutionEngine engine;

    @BeforeClass
    public static void classSetup() throws IOException {
        graphdb = new ImpermanentGraphDatabase();
        graphdb.cleanContent(true);
    
        engine = new ExecutionEngine( graphdb );
    }

    @After
    public void cleanContent() {
        graphdb.cleanContent(true);
    }

    @AfterClass
    public static void teardown() {
        graphdb.shutdown();
    }

    public AbstractCypherTest() {
	super();
    }

    protected void assertQueryEquals(String expected, String query) {
        assertEquals(expected, query);
        // Make sure the generated query is actually executable
        try {
            engine.execute(query);
        } catch (MissingIndexException mie) {
        } catch (NotFoundException nfe) {
        }
    }

}