/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.renderer.Renderer;

import java.util.Arrays;

/**
 * @author Michael J. Simons
 */
class ProcedureCallsIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Test
	void simple() {

		String expected = "CALL db.labels()";

		Statement call = Cypher.call("db", "labels").build();
		assertThat(cypherRenderer.render(call)).isEqualTo(expected);

		call = Cypher.call("db.labels").build();
		assertThat(cypherRenderer.render(call)).isEqualTo(expected);
	}

	@Test
	void simpleBasedOnStringCollection() {

		String expected = "CALL db.labels()";

		String[] prodecureCallPart = {"db", "labels"};
		Statement call = Cypher.call(Arrays.asList(prodecureCallPart)).build();
		assertThat(cypherRenderer.render(call)).isEqualTo(expected);
	}

	@Test
	void shouldGenerateStatementsOfCorrectType() {

		Statement call = Cypher.call("db", "labels").build();
		assertThat(call).isInstanceOf(ProcedureCall.class);
		assertThat(call).isInstanceOf(Statement.class);
		assertThat(call).isNotInstanceOf(ResultStatement.class);

		call = Cypher.call("db", "labels").yield("label").build();
		assertThat(call).isInstanceOf(ProcedureCall.class);
		assertThat(call).isInstanceOf(Statement.class);
		assertThat(call).isInstanceOf(ResultStatement.class);

		call = Cypher.call("db", "labels").yield("label").returning("label").build();
		assertThat(call).isInstanceOf(Statement.SingleQuery.class);
		assertThat(call).isInstanceOf(Statement.class);
		assertThat(call).isInstanceOf(ResultStatement.class);
	}

	@Test
	void withArgs() {

		Statement call = Cypher
			.call("dbms.security.createUser")
			.withArgs(Cypher.literalOf("johnsmith"), Cypher.literalOf("h6u4%kr"), BooleanLiteral.FALSE)
			.build();
		assertThat(cypherRenderer.render(call))
			.isEqualTo("CALL dbms.security.createUser('johnsmith', 'h6u4%kr', false)");
	}

	@Test
	void yieldItems() {

		String expected = "CALL dbms.procedures() YIELD name, signature";

		Statement call = Cypher.call("dbms.procedures").yield("name", "signature").build();
		assertThat(cypherRenderer.render(call)).isEqualTo(expected);

		call = Cypher.call("dbms.procedures").yield(Cypher.name("name"), Cypher.name("signature"))
			.build();
		assertThat(cypherRenderer.render(call)).isEqualTo(expected);
	}

	@Test
	void yieldItemsRenamed() {

		Statement call = Cypher
			.call("db.propertyKeys")
			.yield(Cypher.name("propertyKey").as("prop"))
			.build();
		assertThat(cypherRenderer.render(call)).isEqualTo("CALL db.propertyKeys() YIELD propertyKey AS prop");
	}

	@Test
	void withArgsAndYield() {

		Statement call = Cypher
			.call("dbms.listConfig")
			.withArgs(Cypher.literalOf("browser"))
			.yield("name")
			.build();
		assertThat(cypherRenderer.render(call)).isEqualTo("CALL dbms.listConfig('browser') YIELD name");
	}

	@Test
	void where() {

		SymbolicName name = Cypher.name("name");
		Statement call = Cypher
			.call("dbms.listConfig")
			.withArgs(Cypher.literalOf("browser"))
			.yield(name)
			.where(name.matches("browser\\.allow.*"))
			.returning(Cypher.asterisk())
			.build();
		assertThat(cypherRenderer.render(call))
			.isEqualTo("CALL dbms.listConfig('browser') YIELD name WHERE name =~ 'browser\\\\.allow.*' RETURN *");
	}

	@Test
	void returning() {

		SymbolicName label = Cypher.name("label");
		Statement call = Cypher
			.call("db.labels")
			.yield(label)
			.returning(Functions.count(label).as("numLabels"))
			.build();
		assertThat(cypherRenderer.render(call))
			.isEqualTo("CALL db.labels() YIELD label RETURN count(label) AS numLabels");
	}

	@Test
	void withThanReturning() {

		SymbolicName label = Cypher.name("label");
		Statement call = Cypher
			.call("db.labels")
			.yield(label)
			.with(label)
			.returning(Functions.count(label).as("numLabels"))
			.build();
		assertThat(cypherRenderer.render(call))
			.isEqualTo("CALL db.labels() YIELD label WITH label RETURN count(label) AS numLabels");
	}

	@Test
	void withThanReturningInQuery() {

		SymbolicName label = Cypher.name("label");
		Statement call = Cypher
			.match(Cypher.anyNode().named("n"))
			.with("n")
			.call("db.labels")
			.yield(label)
			.with(label)
			.returning(Functions.count(label).as("numLabels"))
			.build();
		assertThat(cypherRenderer.render(call))
			.isEqualTo("MATCH (n) WITH n CALL db.labels() YIELD label WITH label RETURN count(label) AS numLabels");
	}

	@Test // GH-101
	void shouldBeUsableAsExpression() {

		Node p = Cypher.node("Person").named("p");
		Statement stmt = Cypher.merge(p.withProperties(Cypher.mapOf("id", Cypher.call("apoc.create.uuid").asFunction())))
			.set(
				p.property("firstName").to(Cypher.literalOf("Michael")),
				p.property("surname").to(Cypher.literalOf("Hunger"))
			)
			.returning(p)
			.build();
		assertThat(cypherRenderer.render(stmt))
			.isEqualTo("MERGE (p:`Person` {id: apoc.create.uuid()}) SET p.firstName = 'Michael', p.surname = 'Hunger' RETURN p");
	}

	@Test
	void dynamicDistinct() {

		assertThat(cypherRenderer.render(Cypher.returning(Cypher.call("aVg").withArgs(Cypher.literalOf(1)).asFunction(true)).build()))
			.isEqualTo("RETURN aVg(DISTINCT 1)");
	}

	@Test
	void dynamicDistinctUnsupported() {

		assertThatIllegalArgumentException().isThrownBy(() -> Cypher.call("foobar").withArgs(Cypher.literalOf(1)).asFunction(true))
			.withMessage("The distinct operator can only be applied within aggregate functions.");
	}

	@Test // GH-101
	void shouldBeUsableWithParametersAsExpression() {

		Node p = Cypher.node("Person").named("p");
		Statement stmt = Cypher.merge(p.withProperties(Cypher.mapOf("id", Cypher.call("apoc.create.uuid").asFunction())))
			.set(p.property("surname").to(Cypher.literalOf("Simons")))
			.with(p)
			.call("apoc.create.setProperty").withArgs(p.getRequiredSymbolicName(), Cypher.call("apoc.text.camelCase").withArgs(Cypher.literalOf("first name")).asFunction(), Cypher.literalOf("Michael")).yield("node")
			.returning("node")
			.build();
		assertThat(cypherRenderer.render(stmt))
			.isEqualTo("MERGE (p:`Person` {id: apoc.create.uuid()}) SET p.surname = 'Simons' WITH p CALL apoc.create.setProperty(p, apoc.text.camelCase('first name'), 'Michael') YIELD node RETURN node");
	}

	@Nested
	class MultipartQueries {

		@Test
		void unrelated() {

			SymbolicName name = Cypher.name("name");
			Statement call = Cypher
				.call("dbms.listConfig")
				.withArgs(Cypher.literalOf("browser"))
				.yield(name)
				.where(name.matches("browser\\.allow.*"))
				.match(Cypher.anyNode("n"))
				.with(name)
				.returning(Cypher.asterisk())
				.build();
			assertThat(cypherRenderer.render(call))
				.isEqualTo(
					"CALL dbms.listConfig('browser') YIELD name WHERE name =~ 'browser\\\\.allow.*' MATCH (n) WITH name RETURN *");
		}

		@Test
		void related() {

			SymbolicName name = Cypher.name("name");
			SymbolicName description = Cypher.name("description");
			Statement call = Cypher
				.call("dbms.listConfig")
				.withArgs(Cypher.literalOf("browser"))
				.yield(name, description)
				.where(name.matches("browser\\.allow.*"))
				.with(Cypher.asterisk())
				.create(Cypher.node("Config").withProperties("name", name, "description", description).named("n"))
				.returning(Cypher.name("n"))
				.build();
			assertThat(cypherRenderer.render(call))
				.isEqualTo(
					"CALL dbms.listConfig('browser') YIELD name, description WHERE name =~ 'browser\\\\.allow.*' WITH * CREATE (n:`Config` {name: name, description: description}) RETURN n");
		}

		@Test
		void relatedInner() {

			SymbolicName name = Cypher.name("name");
			AliasedExpression parameters = Cypher.listOf(Cypher.literalOf("browser"), Cypher.literalOf("causal_clustering"))
				.as("parameters");
			Statement call = Cypher.with(parameters)
				.unwind(parameters).as("p")
				.call("dbms.listConfig").withArgs(Cypher.name("p"))
				.yield(name)
				.where(name.matches(".*allow.*"))
				.returning(name)
				.build();

			assertThat(cypherRenderer.render(call))
				.isEqualTo(
					"WITH ['browser', 'causal_clustering'] AS parameters UNWIND parameters AS p CALL dbms.listConfig(p) YIELD name WHERE name =~ '.*allow.*' RETURN name");
		}
	}
}
