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

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.neo4j.cypher.EntityNotFoundException;
import org.neo4j.cypher.MissingIndexException;
import org.neo4j.cypher.ParameterNotFoundException;
import org.neo4j.graphdb.*;
import org.neo4j.test.TestGraphDatabaseFactory;

public abstract class AbstractCypherTest
{

    public static final String CYPHER = "CYPHER " + "2.3" + " ";
    protected GraphDatabaseService graphdb;
    private Transaction tx;

    @BeforeClass
    public static void classSetup() throws IOException
    {
    }

    @Before
    public void setUp() throws Exception {
        graphdb = new TestGraphDatabaseFactory().newImpermanentDatabase();
        tx = graphdb.beginTx();
    }

    @After
    public void cleanContent()
    {
        if (tx!=null) {
            tx.failure();
            tx.close();
            tx = null;
        }
        graphdb.shutdown();
    }

    @AfterClass
    public static void teardown()
    {
    }

    public AbstractCypherTest()
    {
        super();
    }

    protected void assertQueryEquals( String expected, String query )
    {
        assertEquals( expected, query );
        // Make sure the generated query is actually executable
        try (Result result = graphdb.execute("EXPLAIN "+ query))
        {
            result.hasNext();
            result.close();
        } catch (QueryExecutionException qee) {
            if (!qee.getMessage().matches("Index `.+` does not exist")) throw qee;
        }
    }

}
