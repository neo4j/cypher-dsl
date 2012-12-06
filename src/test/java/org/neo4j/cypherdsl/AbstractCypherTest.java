package org.neo4j.cypherdsl;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.neo4j.cypher.MissingIndexException;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.graphdb.NotFoundException;
import org.neo4j.test.ImpermanentGraphDatabase;

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