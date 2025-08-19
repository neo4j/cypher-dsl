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

import static org.apiguardian.api.API.Status.STABLE;

/**
 * Main entrypoint to a Cypher parser that produces Cypher-DSL statements.
 *
 * @author Michael J. Simons
 * @since 2021.3.0
 */
@API(status = STABLE, since = "2021.3.0")
public final class CypherParser {

	/**
	 * Not to be instantiated.
	 */
	private CypherParser() {
	}

	/**
	 * Parses a single node.
	 * @param input a Cypher fragment
	 * @return a node
	 * @see #parseNode(String, Options)
	 */
	public static Node parseNode(String input) {
		return parseNode(input, Options.defaultOptions());
	}

	/**
	 * Parses a Cypher fragment describing a Node-pattern into a {@link Node} instance.
	 * @param input a Cypher fragment
	 * @param options options for the parser
	 * @return a node
	 */
	public static Node parseNode(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
				CypherDslASTExceptionFactory.INSTANCE, getCharStream(input))
			.NodePattern()).value();
	}

	/**
	 * Parses a single relationship.
	 * @param input a Cypher fragment
	 * @return a relationship pattern or chain of relationship pattern
	 * @see #parseNode(String, Options)
	 */
	public static RelationshipPattern parseRelationship(String input) {
		return parseRelationship(input, Options.defaultOptions());
	}

	/**
	 * Parses a Cypher fragment describing a relationship into a
	 * {@link RelationshipPattern} instance.
	 * @param input a Cypher fragment
	 * @param options options for the parser
	 * @return a relationship pattern or chain of relationship pattern
	 */
	public static RelationshipPattern parseRelationship(String input, Options options) {

		return handle(input, () -> (RelationshipPattern) new Cypher<>(CypherDslASTFactory.getInstance(options),
				CypherDslASTExceptionFactory.INSTANCE, getCharStream(input))
			.Pattern());
	}

	/**
	 * Parses a full expression.
	 * @param input a Cypher fragment of an expression
	 * @return a valid Cypher-DSL expression instance
	 * @see #parseExpression(String, Options)
	 */
	public static Expression parseExpression(String input) {
		return parseExpression(input, Options.defaultOptions());
	}

	/**
	 * Parses a Cypher expression into an {@link Expression}.
	 * @param input a Cypher fragment of an expression
	 * @param options options for the parser
	 * @return a valid Cypher-DSL expression instance
	 */
	public static Expression parseExpression(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
				CypherDslASTExceptionFactory.INSTANCE, getCharStream(input))
			.Expression());
	}

	/**
	 * Parses a single clause.
	 * @param input a Cypher fragment containing a valid clause
	 * @return a {@link Clause} instance
	 * @see #parseClause(String, Options)
	 */
	public static Clause parseClause(String input) {
		return parseClause(input, Options.defaultOptions());
	}

	/**
	 * Parses a fragment into a {@link Clause} that can be put together into a whole
	 * statement via {@link Statement#of(List)}.
	 * @param input a Cypher fragment containing a valid clause
	 * @param options options for the parser
	 * @return a {@link Clause} instance
	 */
	public static Clause parseClause(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
				CypherDslASTExceptionFactory.INSTANCE, getCharStream(input))
			.Clause());
	}

	/**
	 * Parses a full statement.
	 * @param input string representing a statement
	 * @return a {@link Statement} statement.
	 * @see #parseStatement(String, Options)
	 */
	public static Statement parseStatement(String input) {
		return parseStatement(input, Options.defaultOptions());
	}

	/**
	 * Parses a whole statement into a renderable Cypher-DSL {@link Statement}. The
	 * statement might be used in a subquery, with a union or maybe just rewritten.
	 * @param input string representing a statement
	 * @param options options for the parser
	 * @return a {@link Statement} statement.
	 */
	public static Statement parseStatement(String input, Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
				CypherDslASTExceptionFactory.INSTANCE, getCharStream(input))
			.Statement());
	}

	/**
	 * Parses a {@link String} into a {@link Statement}.
	 * @param input string representing a statement
	 * @return a {@link Statement} statement.
	 * @see #parseStatement(String)
	 */
	public static Statement parse(String input) {
		return parse(input, Options.defaultOptions());
	}

	/**
	 * Parses a {@link String} into a {@link Statement}.
	 * @param input string representing a statement
	 * @param options options for the parser
	 * @return a {@link Statement} statement.
	 * @see #parseStatement(String, Options)
	 */
	public static Statement parse(String input, Options options) {
		return parseStatement(input, options);
	}

	private static <T> T handle(String input, ThrowingParser<T> parser) {
		try {
			return parser.parse();
		}
		catch (ParseException ex) {
			throw new CyperDslParseException(ex);
		}
		catch (IllegalArgumentException ex) {
			throw ex;
		}
		catch (UnsupportedOperationException ex) {
			throw new UnsupportedCypherException(input, ex);
		}
		catch (Exception ex) {
			throw new RuntimeException("Unexpected exception", ex);
		}
	}

	private static CharStream getCharStream(String input) {
		return new CypherCharStream(input);
	}

	@FunctionalInterface
	private interface ThrowingParser<T> {

		T parse() throws Exception;

	}

}
