/*
 * Copyright (c) 2019-2022 "Neo4j,"
 * Neo4j Sweden AB [https://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.URI;

import org.junit.jupiter.api.Test;

/**
 * @author Michael J. Simons
 */
class LoadCSVIT {

	@Test
	void loadAndCreateShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher.loadCSV(URI.create("file:///artists.csv"))
			.as(l)
			.create(Cypher.node("Artist")
				.withProperties("name", Cypher.valueAt(l, 1), "year", Functions.toInteger(Cypher.valueAt(l, 2))))
			.build();

		assertThat(statement.getCypher()).isEqualTo("LOAD CSV FROM 'file:///artists.csv' AS line "
													+ "CREATE (:`Artist` {name: line[1], year: toInteger(line[2])})");
	}

	@Test
	void loadAndCreateWithHeadersShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher.loadCSV(URI.create("file:///artists.csv"), true)
			.as(l)
			.create(Cypher.node("Artist")
				.withProperties("name", Cypher.valueAt(l, 1), "year", Functions.toInteger(Cypher.valueAt(l, 2))))
			.build();

		assertThat(statement.getCypher()).isEqualTo("LOAD CSV WITH HEADERS FROM 'file:///artists.csv' AS line "
													+ "CREATE (:`Artist` {name: line[1], year: toInteger(line[2])})");
	}

	@Test
	void loadAndCreateWithFieldTerminatorShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher.loadCSV(URI.create("file:///artists.csv"))
			.as(l)
			.withFieldTerminator(";")
			.create(Cypher.node("Artist")
				.withProperties("name", Cypher.valueAt(l, 1), "year", Functions.toInteger(Cypher.valueAt(l, 2))))
			.build();

		assertThat(statement.getCypher()).isEqualTo(
			"LOAD CSV FROM 'file:///artists.csv' AS line FIELDTERMINATOR ';' "
			+ "CREATE (:`Artist` {name: line[1], year: toInteger(line[2])})");
	}

	@Test
	void usingPeriodCommitShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher
			.usingPeriodicCommit()
			.loadCSV(URI.create("file:///artists.csv"))
			.as(l)
			.create(Cypher.node("Artist")
				.withProperties("name", Cypher.valueAt(l, 1), "year", Functions.toInteger(Cypher.valueAt(l, 2))))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo(
				"USING PERIODIC COMMIT LOAD CSV FROM 'file:///artists.csv' AS line "
				+ "CREATE (:`Artist` {name: line[1], year: toInteger(line[2])})");
	}

	@Test
	void usingPeriodCommitWithRateShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher
			.usingPeriodicCommit(500)
			.loadCSV(URI.create("file:///artists.csv"))
			.as(l)
			.create(Cypher.node("Artist")
				.withProperties("name", Cypher.valueAt(l, 1), "year", Functions.toInteger(Cypher.valueAt(l, 2))))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo(
				"USING PERIODIC COMMIT 500 LOAD CSV FROM 'file:///artists.csv' AS line "
				+ "CREATE (:`Artist` {name: line[1], year: toInteger(line[2])})");
	}

	@Test
	void usingLinenumberShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher
			.loadCSV(URI.create("file:///artists.csv"))
			.as(l)
			.returning(Functions.linenumber().as("number"), l)
			.build();

		assertThat(statement.getCypher())
			.isEqualTo("LOAD CSV FROM 'file:///artists.csv' AS line RETURN linenumber() AS number, line");
	}

	@Test
	void usingFileShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher
			.loadCSV(URI.create("file:///artists.csv"))
			.as(l)
			.returning(Functions.file().as("path"))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo("LOAD CSV FROM 'file:///artists.csv' AS line RETURN file() AS path");
	}

	@Test
	void allOptionsCombinedShouldWork() {

		SymbolicName l = SymbolicName.of("line");
		Statement statement = Cypher
			.usingPeriodicCommit(42)
			.loadCSV(URI.create("file:///artists.csv"), true)
			.as(l)
			.withFieldTerminator(";")
			.create(Cypher.node("Artist")
				.withProperties(
					"name", Cypher.valueAt(l, 1), "year", Functions.toInteger(Cypher.valueAt(l, 2)),
					"source", Functions.file().concat(Cypher.literalOf("@")).concat(Functions.linenumber())
				))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo(
				"USING PERIODIC COMMIT 42 LOAD CSV WITH HEADERS FROM 'file:///artists.csv' AS line FIELDTERMINATOR ';' "
				+ "CREATE (:`Artist` {name: line[1], year: toInteger(line[2]), source: ((file() + '@') + linenumber())})");
	}

	@Test
	void devGuideExample1() {

		SymbolicName row = SymbolicName.of("row");
		Property id = row.property("Id");
		Statement statement = Cypher
			.loadCSV(URI.create("file:///companies.csv"), true)
			.as(row)
			.with(row).where(id.isNotNull())
			.merge(Cypher.node("Company").named("c").withProperties("companyId", id))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo(
				"LOAD CSV WITH HEADERS FROM 'file:///companies.csv' AS row "
				+ "WITH row WHERE row.Id IS NOT NULL "
				+ "MERGE (c:`Company` {companyId: row.Id})");
	}

	@Test
	void devGuideExample2() {

		SymbolicName row = SymbolicName.of("row");
		Property id = row.property("Id");
		Statement statement = Cypher
			.loadCSV(URI.create("file:///companies.csv"), true)
			.as(row)
			.merge(Cypher.node("Company").named("c").withProperties("companyId", id, "hqLocation",
				Functions.coalesce(row.property("Location"), Cypher.literalOf("Unknown"))))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo(
				"LOAD CSV WITH HEADERS FROM 'file:///companies.csv' AS row "
				+ "MERGE (c:`Company` {companyId: row.Id, hqLocation: coalesce(row.Location, 'Unknown')})");
	}

	@Test
	void devGuideExample3() {

		SymbolicName row = SymbolicName.of("row");
		Property id = row.property("Id");
		Property email = row.property("Email");

		Node node = Cypher.node("Company").named("c").withProperties("companyId", id);

		Statement statement = Cypher
			.loadCSV(URI.create("file:///companies.csv"), true)
			.as(row)
			.merge(node)
			.set(node.property("emailAddress")
				.to(Cypher.caseExpression(Functions.trim(email)).when(Cypher.literalOf("")).then(NullLiteral.INSTANCE)
					.elseDefault(
						email)))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo(
				"LOAD CSV WITH HEADERS FROM 'file:///companies.csv' AS row "
				+ "MERGE (c:`Company` {companyId: row.Id}) "
				+ "SET c.emailAddress = CASE trim(row.Email) WHEN '' THEN NULL ELSE row.Email END");
	}

	@Test
	void devGuideExample4() {

		SymbolicName row = SymbolicName.of("row");
		Property id = row.property("Id");
		Property email = row.property("Email");

		SymbolicName skill = Cypher.name("skill");

		Node e = Cypher.node("Employee").named("e").withProperties("employeeId", id, "email", email);
		Node s = Cypher.node("Skill").named("s").withProperties("name", skill);

		Statement statement = Cypher
			.loadCSV(URI.create("file:///employees.csv"), true)
			.as(row)
			.merge(e)
			.with(e, row)
			.unwind(Functions.split(row.property("Skills"), Cypher.literalOf(":"))).as(skill)
			.merge(s)
			.merge(e.relationshipTo(s, "HAS_EXPERIENCE").named("r"))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo(
				"LOAD CSV WITH HEADERS FROM 'file:///employees.csv' AS row "
				+ "MERGE (e:`Employee` {employeeId: row.Id, email: row.Email}) "
				+ "WITH e, row "
				+ "UNWIND split(row.Skills, ':') AS skill "
				+ "MERGE (s:`Skill` {name: skill}) "
				+ "MERGE (e)-[r:`HAS_EXPERIENCE`]->(s)");
	}

	@Test
	void inQuery() {

		SymbolicName row = SymbolicName.of("row");
		Node userNode = Cypher.node("User").named("u").withProperties("name", Cypher.literalOf("Michael"));

		Statement statement = Cypher.match(userNode)
			.with(userNode)
			.orderBy(userNode.property("name")).ascending()
			.loadCSV(URI.create("file:///bikes.csv"))
			.as(row)
			.merge(userNode.relationshipTo(Cypher.node("Bike").withProperties("name", Cypher.valueAt(row, 0)), "OWNS"))
			.build();

		assertThat(statement.getCypher())
			.isEqualTo("MATCH (u:`User` {name: 'Michael'}) WITH u ORDER BY u.name ASC LOAD CSV FROM 'file:///bikes.csv' AS row MERGE (u)-[:`OWNS`]->(:`Bike` {name: row[0]})");
	}
}
