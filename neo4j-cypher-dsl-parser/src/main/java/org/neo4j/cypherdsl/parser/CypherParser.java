/*
 * Copyright (c) 2019-2025 "Neo4j,"
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
package org.neo4j.cypherdsl.parser;

import static org.apiguardian.api.API.Status.STABLE;

import java.util.List;

import org.apiguardian.api.API;
import org.neo4j.cypher.internal.parser.javacc.CharStream;
import org.neo4j.cypher.internal.parser.javacc.Cypher;
import org.neo4j.cypher.internal.parser.javacc.CypherCharStream;
import org.neo4j.cypher.internal.parser.javacc.ParseException;
import org.neo4j.cypherdsl.core.Clause;
import org.neo4j.cypherdsl.core.Expression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.RelationshipPattern;
import org.neo4j.cypherdsl.core.Statement;

/**
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class CypherParser {

	/**
	 * @param input A Cypher fragment
	 * @return A node
	 * @see #parseNode(String, Options)
	 */
	public static Node parseNode(String input) {
		return parseNode(input, Options.defaultOptions());
	}

	/**
	 * Parses a Cypher fragment describing a Node-pattern into a {@link Node} instance.
	 *
	 * @param input   A Cypher fragment
	 * @param options Options for the parser
	 * @return A node
	 */
	public static Node parseNode(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
			CypherDslASTExceptionFactory.INSTANCE,
			getCharStream(input)).NodePattern()).value();
	}

	/**
	 * @param input A Cypher fragment
	 * @return A relationship pattern or chain of relationship pattern
	 * @see #parseNode(String, Options)
	 */
	public static RelationshipPattern parseRelationship(String input) {
		return parseRelationship(input, Options.defaultOptions());
	}

	/**
	 * Parses a Cypher fragment describing a relationship into a {@link RelationshipPattern} instance.
	 *
	 * @param input   A Cypher fragment
	 * @param options Options for the parser
	 * @return A relationship pattern or chain of relationship pattern
	 */
	public static RelationshipPattern parseRelationship(String input, Options options) {

		return handle(input, () -> (RelationshipPattern) new Cypher<>(CypherDslASTFactory.getInstance(options),
			CypherDslASTExceptionFactory.INSTANCE,
			getCharStream(input)).Pattern());
	}

	/**
	 * @param input A Cypher fragment of an expression
	 * @return A valid Cypher-DSL expression instance
	 * @see #parseExpression(String, Options)
	 */
	public static Expression parseExpression(String input) {
		return parseExpression(input, Options.defaultOptions());
	}

	/**
	 * Parses a Cypher expression into an {@link Expression}.
	 *
	 * @param input   A Cypher fragment of an expression
	 * @param options Options for the parser
	 * @return A valid Cypher-DSL expression instance
	 */
	public static Expression parseExpression(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
			CypherDslASTExceptionFactory.INSTANCE,
			getCharStream(input)).Expression());
	}

	/**
	 * @param input A Cypher fragment containing a valid clause
	 * @return A {@link Clause} instance
	 * @see #parseClause(String, Options)
	 */
	public static Clause parseClause(String input) {
		return parseClause(input, Options.defaultOptions());
	}

	/**
	 * Parses a fragment into a {@link Clause} that can be put together into a whole statement via {@link Statement#of(List)}.
	 *
	 * @param input   A Cypher fragment containing a valid clause
	 * @param options Options for the parser
	 * @return A {@link Clause} instance
	 */
	public static Clause parseClause(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
			CypherDslASTExceptionFactory.INSTANCE,
			getCharStream(input)).Clause());
	}

	/**
	 * @param input String representing a statement
	 * @return A {@link Statement} statement.
	 * @see #parseStatement(String, Options)
	 */
	public static Statement parseStatement(String input) {
		return parseStatement(input, Options.defaultOptions());
	}

	/**
	 * Parses a whole statement into a renderable Cypher-DSL {@link Statement}. The statement might be used in a subquery,
	 * with a union or maybe just rewritten.
	 *
	 * @param input   String representing a statement
	 * @param options Options for the parser
	 * @return A {@link Statement} statement.
	 */
	public static Statement parseStatement(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
			CypherDslASTExceptionFactory.INSTANCE,
			getCharStream(input)).Statement());
	}

	/**
	 * Parses a {@link String} into a {@link Statement}.
	 *
	 * @param input String representing a statement
	 * @return A {@link Statement} statement.
	 * @see #parseStatement(String)
	 */
	public static Statement parse(String input) {
		return parse(input, Options.defaultOptions());
	}

	/**
	 * Parses a {@link String} into a {@link Statement}.
	 *
	 * @param input   String representing a statement
	 * @param options Options for the parser
	 * @return A {@link Statement} statement.
	 * @see #parseStatement(String, Options)
	 */
	public static Statement parse(String input, Options options) {
		return parseStatement(input, options);
	}

	private static <T> T handle(String input, ThrowingParser<T> parser) {
		try {
			return parser.parse();
		} catch (ParseException e) {
			throw new CyperDslParseException(e);
		} catch (IllegalArgumentException e) {
			throw e;
		} catch (UnsupportedOperationException e) {
			throw new UnsupportedCypherException(input, e);
		} catch (Exception e) {
			throw new RuntimeException("Unexpected exception", e);
		}
	}

	@FunctionalInterface
	private interface ThrowingParser<T> {

		T parse() throws Exception;
	}

	private static CharStream getCharStream(String input) {
		return new CypherCharStream(input);
	}

	/**
	 * Not to be instantiated.
	 */
	private CypherParser() {
	}
}
