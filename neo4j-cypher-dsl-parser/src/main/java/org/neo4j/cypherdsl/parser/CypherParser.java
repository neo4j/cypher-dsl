/*
 * Copyright (c) "Neo4j"
 * Neo4j Sweden AB [http://neo4j.com]
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
package org.neo4j.cypherdsl.parser;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.util.List;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Nullable;
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
@API(status = EXPERIMENTAL, since = "2021.3.0")
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
	public static Node parseNode(String input, @Nullable Options options) {

		return handle(input, () -> new Cypher<>(CypherDslASTFactory.getInstance(options),
			CypherDslASTExceptionFactory.INSTANCE,
			getCharStream(input)).NodePattern());
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
	public static RelationshipPattern parseRelationship(String input, @Nullable Options options) {

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
	public static Expression parseExpression(String input, @Nullable Options options) {

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
	public static Clause parseClause(String input, @Nullable Options options) {

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
	public static Statement parseStatement(String input, @Nullable Options options) {

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
	public static Statement parse(String input, @Nullable Options options) {
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
			throw new UnsupportedCypherException(input);
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
