package org.neo4j.cypherdsl.parser;

import java.util.Optional;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.neo4j.driver.AuthTokens;
import org.neo4j.driver.Driver;
import org.neo4j.driver.GraphDatabase;
import org.testcontainers.containers.Neo4jContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

@Testcontainers(disabledWithoutDocker = true)
public class QPPPrimerIT {

	private static final Neo4jContainer<?> neo4j = new Neo4jContainer<>(System.getProperty("default-neo4j-image"))
		.withEnv("NEO4J_ACCEPT_LICENSE_AGREEMENT", "yes")
		.withReuse(true);

	private static Driver driver;

	@BeforeAll
	static void startNeo4j() {
		neo4j.start();
		driver = GraphDatabase.driver(neo4j.getBoltUrl(), AuthTokens.basic("neo4j", neo4j.getAdminPassword()));
		driver.verifyConnectivity();
	}

	@AfterAll
	static void closeDriver() {
		driver.close();
	}

	@Test
	void f() {

	}
}
