/*
 * Copyright (c) 2019-2024 "Neo4j,"
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
package org.neo4j.cypherdsl.examples.core;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.Cypher;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 * @soundtrack Rage Against The Machine - Rage Against The Machine
 */
class ArbitraryProceduresAndFunctionsTest {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Test
	void standaloneCalls() {
		// tag::standalone-call[]
		var call = Cypher.call("db", "labels").build(); // <.>
		assertThat(cypherRenderer.render(call)).isEqualTo("CALL db.labels()");

		call = Cypher.call("db.labels").build();  // <.>
		assertThat(cypherRenderer.render(call)).isEqualTo("CALL db.labels()");
		// end::standalone-call[]

	}

	@Test
	void standaloneCallWithArgs() {
		// tag::standalone-call-with-args[]
		var call = Cypher
			.call("dbms.security.createUser")
			.withArgs(Cypher.literalOf("johnsmith"), Cypher.literalOf("h6u4%kr"), Cypher.literalFalse())
			.build();
		assertThat(cypherRenderer.render(call))
			.isEqualTo("CALL dbms.security.createUser('johnsmith', 'h6u4%kr', false)");
		// end::standalone-call-with-args[]
	}

	@Test
	void standaloneCallYielding() {
		// tag::standalone-call-yielding[]
		var call = Cypher.call("dbms.procedures").yield("name", "signature").build(); // <.>
		assertThat(cypherRenderer.render(call)).isEqualTo("CALL dbms.procedures() YIELD name, signature");

		call = Cypher.call("dbms.procedures").yield(Cypher.name("name"), Cypher.name("signature")).build(); // <.>
		assertThat(cypherRenderer.render(call)).isEqualTo("CALL dbms.procedures() YIELD name, signature");
		// end::standalone-call-yielding[]
	}

	@Test
	void standaloneCallWhere() {
		// tag::standalone-call-where[]
		var name = Cypher.name("name");
		var call = Cypher
			.call("dbms.listConfig")
			.withArgs(Cypher.literalOf("browser"))
			.yield(name)
			.where(name.matches("browser\\.allow.*"))
			.returning(Cypher.asterisk())
			.build();
		assertThat(cypherRenderer.render(call)).isEqualTo(
			"CALL dbms.listConfig('browser') YIELD name WHERE name =~ 'browser\\\\.allow.*' RETURN *");
		// end::standalone-call-where[]
	}

	@Test
	void inQueryCalls() {

		// tag::in-query-calls[]
		var label = Cypher.name("label");
		var statement = Cypher
			.match(Cypher.anyNode().named("n")).with("n")
			.call("db.labels").yield(label).with(label)
			.returning(Cypher.count(label).as("numLabels"))
			.build();
		assertThat(cypherRenderer.render(statement)).isEqualTo(
			"MATCH (n) WITH n CALL db.labels() YIELD label WITH label RETURN count(label) AS numLabels");
		// end::in-query-calls[]
	}

	@Test
	void asFunction() {

		// tag::as-function[]
		var p = Cypher.node("Person").named("p");
		var createUuid = Cypher.call("apoc.create.uuid").asFunction(); // <.>
		var statement = Cypher.merge(p.withProperties(Cypher.mapOf("id", createUuid))) // <.>
			.set(
				p.property("firstName").to(Cypher.literalOf("Michael")),
				p.property("surname").to(Cypher.literalOf("Hunger"))
			)
			.returning(p)
			.build();
		assertThat(cypherRenderer.render(statement)).isEqualTo(
			"MERGE (p:`Person` {id: apoc.create.uuid()}) "
			+ "SET p.firstName = 'Michael', p.surname = 'Hunger' "
			+ "RETURN p");
		// end::as-function[]
	}

	@Test
	void asFunctionWithArgs() {

		// tag::as-function-with-args[]
		var p = Cypher.node("Person").named("p");
		var createUuid = Cypher.call("apoc.create.uuid").asFunction(); // <.>
		var toCamelCase = Cypher.call("apoc.text.camelCase")
			.withArgs(Cypher.literalOf("first name")) // <.>
			.asFunction();
		var statement = Cypher.merge(p.withProperties(Cypher.mapOf("id",
			createUuid)))
			.set(p.property("surname").to(Cypher.literalOf("Simons")))
			.with(p)
			.call("apoc.create.setProperty").withArgs(
				p.getRequiredSymbolicName(),
				toCamelCase,
				Cypher.parameter("nameParam") // <.>
			).yield("node")
			.returning("node")
			.build();
		assertThat(cypherRenderer.render(statement)).isEqualTo(
			"MERGE (p:`Person` {id: apoc.create.uuid()}) SET p.surname = 'Simons' "
			+ "WITH p CALL apoc.create.setProperty(p, apoc.text.camelCase('first name'), $nameParam) "
			+ "YIELD node RETURN node");
		// end::as-function-with-args[]
	}
}
