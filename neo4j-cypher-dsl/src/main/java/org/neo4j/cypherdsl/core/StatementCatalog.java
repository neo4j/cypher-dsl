/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * The statement catalog gives an  overview about relevant items in a statement. These  items include tokens (labels and
 * relationship  types), the  resolved properties  for given  sets  of these  tokens (those  sets reassembling  concrete
 * entities, think Nodes with one  or more labels or relationships with a concrete type) as  well as conditions in which
 * the properties have  been used. This structural analysis can  be used to predict how graph  elements are accessed, or
 * they can be used to make sure certain conditions are contained within a statement.
 * <p>
 * Finally, the list of identifiable elements at the end of a statement (aka after a {@code RETURN} clause) is contained
 * in the catalog.
 * <p>
 * In addition, this interface provides the namespace for all  elements that are represented as non-AST elements as part
 * of a catalog, such as the {@link Property properties} and {@link Condition property conditions}.
 * <p>
 * Any instance of a {@link StatementCatalog catalog} and its contents can be safely assumed to be immutable.
 *
 * @author Michael J. Simons
 * @soundtrack Guns n' Roses - Use Your Illusion II
 * @since 2023.1.0
 */
public sealed interface StatementCatalog permits StatementCatalogBuildingVisitor.DefaultStatementCatalog {

	/**
	 * Returns a collection of all tokens used in the analyzed statement.
	 *
	 * @return A collection of all tokens used
	 */
	Collection<Token> getAllTokens();

	/**
	 * Returns a collection of all node labels used in the analyzed statement.
	 *
	 * @return A collection of all labels used
	 */
	default Collection<Token> getNodeLabels() {
		return getAllTokens().stream().filter(token -> token.type() == Token.Type.NODE_LABEL)
			.collect(Collectors.toUnmodifiableSet());
	}

	/**
	 * Returns a collection of all relationship types used in the analyzed statement.
	 *
	 * @return A collection of all types used
	 */
	default Collection<Token> getRelationshipTypes() {
		return getAllTokens().stream().filter(token -> token.type() == Token.Type.RELATIONSHIP_TYPE)
			.collect(Collectors.toUnmodifiableSet());
	}

	/**
	 * Returns a collection of all properties resolved in the analyzed statement.
	 *
	 * @return A collection of all properties resolved
	 */
	Collection<Property> getProperties();

	/**
	 * Returns a map that contains all properties that have been used in a comparing condition as keys and a set of all
	 * comparisons they appeared in.
	 *
	 * @return A map of all properties used in comparisons
	 */
	Map<Property, Collection<Condition>> getAllConditions();

	/**
	 * Returns a collection of all property based conditions in the statement involving the resolved properties.
	 *
	 * @return A collection of all conditions involving properties resolved in the statement
	 */
	default Collection<Condition> getConditions(Property property) {
		return getAllConditions().get(property);
	}

	/**
	 * Returns a collection of all expressions that are identifiable expression in a non-void (or non-unit) Cypher statement.
	 * These expressions might refer to properties, but can be of course function calls, existential subqueries and the
	 * like.
	 *
	 * @return A collection of identifiable expressions.
	 */
	Collection<Expression> getIdentifiableExpressions();

	/**
	 * A token can either describe a node label or a relationship type.
	 *
	 * @param type  The type of this token
	 * @param value The concrete value
	 */
	record Token(Type type, String value) implements Comparable<Token> {

		/**
		 * Turns a specific {@link NodeLabel label} into a more abstract token.
		 *
		 * @param label A label, must not be {@literal null}.
		 * @return A token
		 */
		public static Token label(NodeLabel label) {
			return new Token(Token.Type.NODE_LABEL, Objects.requireNonNull(label, "Label must not be null.").getValue());
		}

		/**
		 * Turns a specific node label into a more abstract token.
		 *
		 * @param label A label, must not be {@literal null}.
		 * @return A token
		 */
		public static Token label(String label) {
			return new Token(Token.Type.NODE_LABEL, Objects.requireNonNull(label, "Label must not be null."));
		}

		/**
		 * Turns a specific relationship type into a more abstract token
		 *
		 * @param type A string representing a type, must not be {@literal null}.
		 * @return A token
		 */
		public static Token type(String type) {
			return new Token(Token.Type.RELATIONSHIP_TYPE, Objects.requireNonNull(type, "Type must not be null."));
		}

		@Override
		public int compareTo(Token o) {
			int result = this.type().compareTo(o.type());
			if (result == 0) {
				result = this.value().compareTo(o.value());
			}
			return result;
		}

		/**
		 * The specific token type.
		 */
		public enum Type {
			/**
			 * Represents a node label.
			 */
			NODE_LABEL,
			/**
			 * Represents a relationship type.
			 */
			RELATIONSHIP_TYPE
		}
	}

	/**
	 * A property that has been resolved. In case this property  has been resolved for an entity, the entity itself will
	 * be defined by its set of tokens. Tokens are guaranteed to be sorted and will be of the same type.
	 *
	 * @param name        The name of the resolved property
	 * @param owningToken Zero or many owning tokens for a property
	 */
	record Property(Set<Token> owningToken, String name) {

		/**
		 * Creates a new property without an owner.
		 *
		 * @param name The name of the resolved property
		 */
		public Property(String name) {
			this(Set.of(), name);
		}

		/**
		 * Creates a new property with a single owning token.
		 *
		 * @param owningToken The owning token
		 * @param name        The name of the resolved property
		 */
		public Property(Token owningToken, String name) {
			this(Set.of(owningToken), name);
		}

		/**
		 * The constructor enforces the use of unmodifiable sets and unique types accross tokens.
		 *
		 * @param name        The name of the resolved property
		 * @param owningToken Zero or many owning tokens for a property
		 */
		public Property {
			if (owningToken.stream().map(Token::type).distinct().count() > 1) {
				throw new IllegalArgumentException("Owning tokens are of multiple types");
			}
			owningToken = Collections.unmodifiableSet(new TreeSet<>(owningToken));
		}

		/**
		 * @return An optional, owning type.
		 */
		public Optional<Token.Type> owningType() {
			return owningToken.stream().map(Token::type).distinct().findFirst();
		}
	}

	/**
	 * This type  encapsulates a comparison in  which a property  of a node or  relationship was used. The  property may
	 * appear on the  left hand side or right hand  side or even on both  side of the comparison (think being  used in a
	 * function on both sides with different arguments). The  {@code clause} attribute will specify the context in which
	 * the comparison was made.
	 * <p>
	 * The expressions used in the comparison are provided as  Cypher-DSL AST expressions. They can be freely visited or
	 * rendered into Cypher via the {@link org.neo4j.cypherdsl.core.renderer.GeneralizedRenderer} like this:
	 *
	 * <pre>{@code
	 *     var cypher = Renderer.getRenderer(Configuration.prettyPrinting(), GeneralizedRenderer.class)
	 *          .render(comparison.left());
	 * }</pre>
	 *
	 * @param clause         The clause in which the comparison was used
	 * @param left           The left hand side of the comparison
	 * @param operator       The operator used
	 * @param right          The right hand side of the comparison
	 * @param parameterNames The parameter names used in this comparison
	 *                       (analoge to {@link Statement#getParameterNames()}
	 * @param parameters     Parameters with defined values used in this comparison
	 *                       (analoge to {@link Statement#getParameters()}
	 */
	record Condition(Clause clause, Expression left, Operator operator, Expression right, Set<String> parameterNames,
		Map<String, Object> parameters
	) {

		/**
		 * The constructor enforces the use of unmodifiable sets.
		 *
		 * @param clause         The clause in which the comparison was used
		 * @param left           The left hand side of the comparison
		 * @param operator       The operator used
		 * @param right          The right hand side of the comparison
		 * @param parameterNames The parameter names used in this comparison
		 *                       (analoge to {@link Statement#getParameterNames()}
		 * @param parameters     Parameters with defined values used in this comparison
		 *                       (analoge to {@link Statement#getParameters()}
		 */
		public Condition {
			parameterNames = Set.copyOf(parameterNames);
			parameters = Map.copyOf(parameters);
		}

		/**
		 * Enum for the clause in which a comparison was made.
		 */
		public enum Clause {
			/**
			 * The comparison was used in a {@code MATCH} clause.
			 */
			MATCH,
			/**
			 * The comparison was used in a {@code CREATE} clause.
			 */
			CREATE,
			/**
			 * The comparison was used in a {@code MERGE} clause.
			 */
			MERGE,
			/**
			 * The comparison was used in a {@code DELETE} clause.
			 */
			DELETE,
			/**
			 * The comparison was used in a {@code WITH} clause. The {@code WITH} clause is different to the {@code WHERE}
			 * clause as here "WHERE simply filters the results."
			 * (quoting from <a href="https://neo4j.com/docs/cypher-manual/current/clauses/where/">where</a>).
			 */
			WITH,
			/**
			 * Used in case the analysis could not determine a clause.
			 */
			UNKNOWN
		}
	}
}
