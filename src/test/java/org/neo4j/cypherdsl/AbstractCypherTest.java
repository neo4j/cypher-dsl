/**
 * Licensed to Neo Technology under one or more contributor
 * license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright
 * ownership. Neo Technology licenses this file to you under
 * the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.neo4j.cypherdsl;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.neo4j.cypher.EntityNotFoundException;
import org.neo4j.cypher.MissingIndexException;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.graphdb.QueryExecutionException;
import org.neo4j.graphdb.Transaction;
import org.neo4j.test.TestGraphDatabaseFactory;

import java.io.IOException;

import static org.junit.Assert.assertEquals;

public abstract class AbstractCypherTest
{

    public static final String CYPHER = "CYPHER " + "3.0" + " ";
    static GraphDatabaseService graphdb;
    private Transaction tx;

    @BeforeClass
    public static void classSetup() throws IOException
    {
        graphdb = new TestGraphDatabaseFactory().newImpermanentDatabase();
    }

    @Before
    public void setUp() throws Exception {
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
        graphdb.execute( "MATCH (n) DETACH DELETE n" );
    }

    @AfterClass
    public static void teardown()
    {
        graphdb.shutdown();
    }

    void assertQueryEquals( String expected, String query )
    {
        assertEquals( expected, query );
        // Make sure the generated query is actually executable
        try {
            graphdb.execute( "EXPLAIN "+ query ).close();
        } catch ( QueryExecutionException qee) {
            if (!qee.getMessage().matches("Index `.*` does not exist") ) throw qee;
        } /*| ParameterNotFoundException | MissingIndexException | EntityNotFoundException | NotFoundException ignored*/
    }

}
