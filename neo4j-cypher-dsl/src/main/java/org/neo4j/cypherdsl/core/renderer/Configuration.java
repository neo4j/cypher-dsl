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
package org.neo4j.cypherdsl.core.renderer;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;
import static org.apiguardian.api.API.Status.STABLE;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.apiguardian.api.API;

/**
 * This class provides some configuration settings for the Cypher-DSL, mainly around rendering of a statement. Instances
 * of {@link Configuration} are threadsafe and can be reused. Please use the {@link Configuration.Builder associated builder}
 * via {@link Configuration#newConfig()} to create new variants.
 *
 * @author Michael J. Simons
 * @soundtrack Slayer - Undisputed Attitude
 * @since 2021.0.1
 */
@API(status = STABLE, since = "2021.0.1")
public final class Configuration {

	private static final Configuration DEFAULT_CONFIG = newConfig().build();
	private static final Configuration PRETTY_PRINTING = newConfig()
		.withPrettyPrint(true)
		.alwaysEscapeNames(false)
		.build();

	/**
	 * Enum for the available indent styles.
	 */
	public enum IndentStyle {
		/** Use tabs for indentation. */
		TAB,
		/** Use a configurable number of spaces for indentation. */
		SPACE
	}

	/**
	 * Enum for configuring name rewriting.
	 */
	public enum GeneratedNames {
		/**
		 * Entity names are rewritten
		 */
		ENTITY_NAMES,
		/**
		 * Parameter names are rewritten
		 */
		PARAMETER_NAMES,
		/**
		 * All aliases are rewritten
		 */
		ALL_ALIASES,
		/**
		 * All aliases except the ones used in the last {@literal RETURN} clause.
		 */
		INTERNAL_ALIASES_ONLY
	}

	/**
	 * Simple definition of a known / schema relationship.
	 *
	 * @param sourceLabel The source label
	 * @param type The type of the relationship
	 * @param targetLabel The target label
	 */
	public record RelationshipDefinition(String sourceLabel, String type, String targetLabel) {

		/**
		 * Will make sure all parameters are trimmed.
		 *
		 * @param sourceLabel The source label
		 * @param type        The type of the relationship
		 * @param targetLabel The target label
		 */
		public RelationshipDefinition {
			sourceLabel = sourceLabel.trim();
			type = type.trim();
			targetLabel = targetLabel.trim();
		}

		private static RelationshipDefinition of(String definition) {
			var tuple = Objects.requireNonNull(definition)
				.replace("(", "").replace(")", "")
				.split(",");

			if (tuple.length != 3) {
				throw new IllegalArgumentException("Invalid relationship definition " + definition);
			}
			return new RelationshipDefinition(tuple[0], tuple[1], tuple[2]);
		}

		boolean selfReferential() {
			return this.sourceLabel.equals(this.targetLabel);
		}
	}

	/**
	 * Creates a new relationship definition from a string in the form {@code (sourceLabel, TYPE, targetLabel)}.
	 *
	 * @param definition The literal definition of the relationship
	 * @return A new relationship definition
	 */
	public static RelationshipDefinition relationshipDefinition(String definition) {
		return RelationshipDefinition.of(definition);
	}

	/**
	 * Set to {@literal true} to enable pretty printing.
	 */
	private final boolean prettyPrint;

	/**
	 * Configure your favorite indentation style.
	 */
	private final IndentStyle indentStyle;

	/**
	 * The indentation sizes. Only applicable when using {@link IndentStyle#SPACE}. Defaults to {@literal 2}.
	 */
	private final int indentSize;

	/**
	 * A flag if all names (node labels and relationship types) should be escaped all the time. Defaults to {@literal true}.
	 * When pretty printing, this defaults to {@literal false}.
	 */
	private final boolean alwaysEscapeNames;

	/**
	 * Configure  if and  which names  (identifiers of  entities, parameter  names and  aliases) will  be replaced  with
	 * generated names during  rendering. The names will stay constant  for the lifetime of a statement.  It defaults to
	 * the empty set. This setting can be useful if you want to normalize your statements: Imagine two statements coming
	 * from two different  sources who may have used  different variable or parameter names. Using  generated names will
	 * make those statements produce the same rendering, given they have the same semantics.
	 *
	 */
	private final Set<GeneratedNames> generatedNames;

	/**
	 * The dialect to use when rendering a statement. The default dialect works well with Neo4j 4.4 and prior.
	 */
	private final Dialect dialect;

	/**
	 * A flag of the renderer should be instructed to enforce a schema.
	 */
	private final boolean enforceSchema;

	/**
	 * The map of known relationship definitions. The key is the relationship type.
	 */
	private final Map<String, List<RelationshipDefinition>> relationshipDefinitions;

	/**
	 * Cypher is not pretty printed by default. No indentation settings apply.
	 *
	 * @return the default config
	 */
	public static Configuration defaultConfig() {
		return DEFAULT_CONFIG;
	}

	/**
	 * Pretty printing with default indentation settings.
	 *
	 * @return a configuration enabling pretty printing.
	 */
	public static Configuration prettyPrinting() {
		return PRETTY_PRINTING;
	}

	/**
	 * {@return a new builder} for creating a new configuration from scratch
	 */
	public static Builder newConfig() {
		return Builder.newConfig();
	}

	/**
	 * Use this builder to create new {@link Configuration} instances.
	 */
	@SuppressWarnings("HiddenField")
	public static final class Builder {

		private boolean prettyPrint = false;
		private IndentStyle indentStyle = IndentStyle.SPACE;
		private int indentSize = 2;
		private boolean alwaysEscapeNames = true;
		private Dialect dialect = Dialect.NEO4J_4;
		private Set<GeneratedNames> generatedNames = EnumSet.noneOf(GeneratedNames.class);
		private boolean enforceSchema = false;
		private Map<String, List<RelationshipDefinition>> relationshipDefinitions = new HashMap<>();

		private Builder() {
		}

		static Builder newConfig() {
			return new Builder();
		}

		/**
		 * Enables or disables pretty printing. Enabling pretty printing will disable unnecessary escaping of labels and types.
		 *
		 * @param prettyPrint use {@literal true} for enabling pretty printing
		 * @return this builder
		 */
		public Builder withPrettyPrint(boolean prettyPrint) {
			this.prettyPrint = prettyPrint;
			if (this.prettyPrint) {
				return this.alwaysEscapeNames(false);
			}
			return this;
		}

		/**
		 * @param indentStyle The new indentation style
		 * @return this builder
		 */
		public Builder withIndentStyle(IndentStyle indentStyle) {

			if (indentStyle == null) {
				throw new IllegalArgumentException("Indent style is required.");
			}
			this.indentStyle = indentStyle;
			return this;
		}

		/**
		 * @param indentSize The new indentation size
		 * @return this builder
		 */
		public Builder withIndentSize(int indentSize) {
			this.indentSize = indentSize;
			return this;
		}

		/**
		 * Configure whether names should be always escaped.
		 *
		 * @param alwaysEscapeNames use {@literal true} to always escape names
		 * @return this builder
		 */
		public Builder alwaysEscapeNames(boolean alwaysEscapeNames) {
			this.alwaysEscapeNames = alwaysEscapeNames;
			return this;
		}

		/**
		 * Configure whether variable names should be always generated.
		 *
		 * @param useGeneratedNames Set to {@literal true} to use generated symbolic names, parameter names and aliases
		 * @return this builder
		 */
		public Builder withGeneratedNames(boolean useGeneratedNames) {
			if (useGeneratedNames) {
				this.generatedNames = EnumSet.allOf(GeneratedNames.class);
			} else {
				this.generatedNames = EnumSet.noneOf(GeneratedNames.class);
			}
			return this;
		}

		/**
		 * Configure for which type of object generated names should be used.
		 *
		 * @param useGeneratedNames The set of objects for which generated names should be used
		 * @return this builder
		 */
		public Builder withGeneratedNames(Set<GeneratedNames> useGeneratedNames) {
			this.generatedNames = Objects.requireNonNullElseGet(useGeneratedNames, () -> EnumSet.noneOf(GeneratedNames.class));
			return this;
		}

		/**
		 * Use a configuration with a dialect fitting your target database if the default dialect for
		 * {@link Dialect#NEO4J_4 Neo4j 4.x and earlier} leads to incompatible results with your version of Neo4j.
		 *
		 * @param dialect The new dialect
		 * @return This builder. You can both use the original or this instance.
		 * @since 2022.3.0
		 */
		public Builder withDialect(Dialect dialect) {
			this.dialect = dialect;
			return this;
		}

		/**
		 * Adds a new relationship definition to the current schema.
		 *
		 * @param relationshipDefinition A new relationship definition
		 * @return this builder
		 * @since 2023.7.0
		 */
		@API(status = EXPERIMENTAL, since = "2023.7.0")
		public Builder withRelationshipDefinition(RelationshipDefinition relationshipDefinition) {
			if (relationshipDefinition == null) {
				return this;
			}
			var relationships = this.relationshipDefinitions.computeIfAbsent(relationshipDefinition.type,
				k -> new ArrayList<>());
			relationships.add(relationshipDefinition);
			return this;
		}

		/**
		 * Configure whether to enforce a schema or not.
		 *
		 * @param enforceSchema Set to {@literal true} to enforce the schema defined by known {@link #withRelationshipDefinition(RelationshipDefinition) relationship definitions}.
		 * @return this builder
		 * @since 2023.7.0
		 */
		@API(status = EXPERIMENTAL, since = "2023.7.0")
		public Builder withEnforceSchema(boolean enforceSchema) {
			this.enforceSchema = enforceSchema;
			return this;
		}

		/**
		 * @return a new immutable configuration
		 */
		public Configuration build() {
			return new Configuration(this);
		}
	}

	private Configuration(Builder builder) {
		this.prettyPrint = builder.prettyPrint;
		this.alwaysEscapeNames = builder.alwaysEscapeNames;
		this.indentStyle = builder.indentStyle;
		this.indentSize = builder.indentSize;
		this.dialect = builder.dialect == null ? Dialect.NEO4J_4 : builder.dialect;
		this.generatedNames = builder.generatedNames;
		this.enforceSchema = builder.enforceSchema;

		Map<String, List<RelationshipDefinition>> mutableRelationshipDefinitions = new HashMap<>();
		builder.relationshipDefinitions.forEach((k, v) -> mutableRelationshipDefinitions.put(k, List.copyOf(v)));
		this.relationshipDefinitions = Map.copyOf(mutableRelationshipDefinitions);
	}

	/**
	 * @return {@literal true} if this configuration uses pretty printing
	 */
	public boolean isPrettyPrint() {
		return prettyPrint;
	}

	/**
	 * {@return the indentation style} whether to use tabs or spaces to indent things
	 */
	public IndentStyle getIndentStyle() {
		return indentStyle;
	}

	/**
	 * @return width of one indentation
	 */
	public int getIndentSize() {
		return indentSize;
	}

	/**
	 * @return {@literal true} when names should be always escaped
	 */
	public boolean isAlwaysEscapeNames() {
		return alwaysEscapeNames;
	}

	/**
	 * @return The set of object types for which generated names should be used
	 * @since 2023.2.0
	 */
	public Set<GeneratedNames> getGeneratedNames() {
		return generatedNames;
	}

	/**
	 * @return {@literal true} when symbolic and parameter names should be replaced with generated names
	 * @since 2023.2.0
	 */
	public boolean isUseGeneratedNames() {
		return !this.generatedNames.isEmpty();
	}

	/**
	 * @return the target dialect
	 */
	public Dialect getDialect() {
		return dialect;
	}

	/**
	 * @return {@literal true} if a schema should be enforced
	 * @since 2023.7.0
	 */
	@API(status = EXPERIMENTAL, since = "2023.7.0")
	public boolean isEnforceSchema() {
		return enforceSchema;
	}

	/**
	 * @return A map of predefined relationships
	 * @since 2023.7.0
	 */
	@API(status = EXPERIMENTAL, since = "2023.7.0")
	public Map<String, List<RelationshipDefinition>> getRelationshipDefinitions() {
		return relationshipDefinitions;
	}

	@Override
	public boolean equals(Object o) {
		if (this == o) {
			return true;
		}
		if (o == null || getClass() != o.getClass()) {
			return false;
		}
		Configuration that = (Configuration) o;
		return prettyPrint == that.prettyPrint && indentSize == that.indentSize && indentStyle == that.indentStyle &&
			alwaysEscapeNames == that.alwaysEscapeNames && dialect == that.dialect &&
			generatedNames.equals(that.generatedNames) && enforceSchema == that.enforceSchema &&
			relationshipDefinitions.equals(that.relationshipDefinitions);
	}

	@Override
	public int hashCode() {
		return Objects.hash(prettyPrint, indentStyle, indentSize, alwaysEscapeNames, dialect, generatedNames,
			enforceSchema, relationshipDefinitions);
	}

	@Override
	public String toString() {
		return "Configuration{" +
			"prettyPrint=" + prettyPrint +
			", indentStyle=" + indentStyle +
			", indentSize=" + indentSize +
			'}';
	}
}
