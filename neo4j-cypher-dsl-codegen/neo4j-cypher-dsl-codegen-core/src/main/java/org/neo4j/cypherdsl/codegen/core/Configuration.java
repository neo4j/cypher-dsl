/*
 * Copyright (c) 2019-2026 "Neo4j,"
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
package org.neo4j.cypherdsl.codegen.core;

import java.nio.file.Path;
import java.time.Clock;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.apiguardian.api.API;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

/**
 * Main configuration objects for all aspects of code generation.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public final class Configuration {

	/**
	 * Lookup key to be used in an annotations processors environment options for prefixes
	 * to be used.
	 */
	public static final String PROPERTY_PREFIX = "org.neo4j.cypherdsl.codegen.prefix";

	/**
	 * Lookup key to be used in an annotations processors environment options for suffixes
	 * to be used.
	 */
	public static final String PROPERTY_SUFFIX = "org.neo4j.cypherdsl.codegen.suffix";

	/**
	 * Lookup key to be used in an annotations processors environment options for the
	 * indent style to be used.
	 */
	public static final String PROPERTY_INDENT_STYLE = "org.neo4j.cypherdsl.codegen.indent_style";

	/**
	 * Lookup key to be used in an annotations processors environment options for the
	 * indent size to be used.
	 */
	public static final String PROPERTY_INDENT_SIZE = "org.neo4j.cypherdsl.codegen.indent_size";

	/**
	 * Lookup key to be used in an annotations processors environment options for the
	 * timestamp to be used.
	 */
	public static final String PROPERTY_TIMESTAMP = "org.neo4j.cypherdsl.codegen.timestamp";

	/**
	 * Lookup key to be used in an annotations processors environment options for the flag
	 * whether generated code should be marked.
	 */
	public static final String PROPERTY_ADD_AT_GENERATED = "org.neo4j.cypherdsl.codegen.add_at_generated";

	/**
	 * Lookup key to be used in an annotations processors environment options for a comma
	 * separated list of types that should be excluded from being processed.
	 */
	public static final String PROPERTY_EXCLUDES = "org.neo4j.cypherdsl.codegen.excludes";

	private static final Configuration DEFAULT_CONFIG = newConfig().build();

	/**
	 * Defines decoration for generated type names, applies to both nodes and
	 * relationships.
	 */
	private final UnaryOperator<String> typeNameDecorator;

	/**
	 * Defines how classes representing nodes should be named.
	 */
	private final ClassNameGenerator nodeNameGenerator;

	/**
	 * Defines how classes representing relationships should be named.
	 */
	private final ClassNameGenerator relationshipNameGenerator;

	private final FieldNameGenerator fieldNameGenerator;

	/**
	 * On which Java version should the generated classes be compilable? Defaults to Java
	 * Release 8.
	 */
	private final JavaVersion target;

	/**
	 * The fully qualified name of the Java package into which the classes should be
	 * generated.
	 */
	private final String defaultPackage;

	/**
	 * The path into which the Java classes should be generated. A package structure
	 * matching {@link #defaultPackage} will be created.
	 */
	private final Optional<Path> path;

	/**
	 * The indention string used in the generated source files.
	 */
	private final String indent;

	/**
	 * Optional clock to use while generation things.
	 */
	private final Optional<Clock> clock;

	/**
	 * Flag if the {@code @Generated}-annotation should be added. On JDK9+ on the module
	 * path it would require jdk.compiler. If you don't want it, disable it with this
	 * flag.
	 */
	private final boolean addAtGenerated;

	/**
	 * Set of qualified names to exclude from processing.
	 */
	private final Set<String> excludes;

	private Configuration(UnaryOperator<String> typeNameDecorator, ClassNameGenerator nodeNameGenerator,
			ClassNameGenerator relationshipNameGenerator, FieldNameGenerator fieldNameGenerator, JavaVersion target,
			String defaultPackage, Path path, String indent, Clock clock, boolean addAtGenerated,
			Set<String> excludes) {
		this.typeNameDecorator = typeNameDecorator;
		this.nodeNameGenerator = nodeNameGenerator;
		this.relationshipNameGenerator = relationshipNameGenerator;
		this.fieldNameGenerator = fieldNameGenerator;
		this.target = target;
		this.defaultPackage = defaultPackage;
		this.path = Optional.ofNullable(path);
		this.indent = indent;
		this.clock = Optional.ofNullable(clock);
		this.addAtGenerated = addAtGenerated;
		this.excludes = excludes;
	}

	/**
	 * {@return an instance of the default configuration}
	 */
	public static Configuration defaultConfig() {
		return DEFAULT_CONFIG;
	}

	/**
	 * Starts building new configuration.
	 * @return a new builder
	 */
	public static Builder newConfig() {
		return Builder.newConfig();
	}

	/**
	 * Starts building a new configuration for the given path.
	 * @param path the path into which code should be generated
	 * @return a new builder
	 */
	public static Builder newConfig(final Path path) {
		return Builder.newConfig(path);
	}

	/**
	 * {@return the generator for node names}
	 */
	public ClassNameGenerator getNodeNameGenerator() {
		return this.nodeNameGenerator;
	}

	/**
	 * {@return the generator for class names}
	 */
	public ClassNameGenerator getRelationshipNameGenerator() {
		return this.relationshipNameGenerator;
	}

	/**
	 * {@return the generator for constant field names}
	 */
	public FieldNameGenerator getConstantFieldNameGenerator() {
		return this.fieldNameGenerator;
	}

	/**
	 * {@return the target java version of the generated code}
	 */
	public JavaVersion getTarget() {
		return this.target;
	}

	/**
	 * {@return the default package name to use}
	 */
	public String getDefaultPackage() {
		return this.defaultPackage;
	}

	/**
	 * {@return the path to generate code into}
	 */
	public Optional<Path> getPath() {
		return this.path;
	}

	/**
	 * {@return the decorator that applies pre- and suffixes to typenames}
	 */
	public UnaryOperator<String> getTypeNameDecorator() {
		return this.typeNameDecorator;
	}

	/**
	 * {@return the string to be used as indent}
	 */
	public String getIndent() {
		return this.indent;
	}

	/**
	 * {@return an optional clock, different from the system.}
	 */
	public Optional<Clock> getClock() {
		return this.clock;
	}

	/**
	 * {@return literal <code>true</code> if generated code is marked as such}
	 */
	public boolean isAddAtGenerated() {
		return this.addAtGenerated;
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
		return this.typeNameDecorator.equals(that.typeNameDecorator)
				&& this.nodeNameGenerator.equals(that.nodeNameGenerator)
				&& this.relationshipNameGenerator.equals(that.relationshipNameGenerator)
				&& this.fieldNameGenerator.equals(that.fieldNameGenerator) && this.target == that.target
				&& this.defaultPackage.equals(that.defaultPackage) && this.path.equals(that.path)
				&& this.indent.equals(that.indent) && this.clock.equals(that.clock)
				&& this.addAtGenerated == that.addAtGenerated;
	}

	@Override
	public int hashCode() {
		return Objects.hash(this.typeNameDecorator, this.nodeNameGenerator, this.relationshipNameGenerator,
				this.fieldNameGenerator, this.target, this.defaultPackage, this.path, this.indent, this.clock,
				this.addAtGenerated);
	}

	/**
	 * Returns {@literal true} if the type with the given name should be excluded.
	 * @param qualifiedName the type name to be checked
	 * @return {@literal true} if the type with the given name should be excluded
	 */
	boolean exclude(String qualifiedName) {
		return this.excludes.contains(qualifiedName);
	}

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
	 * The target Java baseline.
	 */
	public enum JavaVersion {

		/**
		 * Generated code should compile on Java 8.
		 */
		RELEASE_8,
		/**
		 * Generated code should compile on Java 11 or higher.
		 */
		RELEASE_11

	}

	/**
	 * Use this builder to create new
	 * {@link org.neo4j.cypherdsl.core.renderer.Configuration} instances.
	 */
	@SuppressWarnings("HiddenField")
	public static final class Builder {

		private ClassNameGenerator nodeNameGenerator = new NodeNameGenerator();

		private ClassNameGenerator relationshipNameGenerator = new RelationshipNameGenerator();

		private FieldNameGenerator fieldNameGenerator = FieldNameGenerator.Default.INSTANCE;

		private JavaVersion target = JavaVersion.RELEASE_11;

		private String defaultPackage = "";

		private Path path;

		private String prefix;

		private String suffix = "_";

		private IndentStyle indentStyle = IndentStyle.TAB;

		private int indentSize = 2;

		private String timestamp;

		private boolean addAtGenerated = false;

		private Set<String> excludes = Set.of();

		private Builder() {
		}

		static Builder newConfig() {
			return new Builder();
		}

		static Builder newConfig(final Path path) {
			return new Builder().withPath(path);
		}

		/**
		 * Changes the node name generator.
		 * @param nodeNameGenerator a new generator
		 * @return this builder
		 */
		public Builder withNodeNameGenerator(ClassNameGenerator nodeNameGenerator) {

			if (nodeNameGenerator == null) {
				throw new IllegalArgumentException("A class name generator for nodes is required.");
			}
			this.nodeNameGenerator = nodeNameGenerator;
			return this;
		}

		/**
		 * Changes the relationship name generator.
		 * @param relationshipNameGenerator a new generator
		 * @return this builder
		 */
		public Builder withRelationshipNameGenerator(ClassNameGenerator relationshipNameGenerator) {

			if (relationshipNameGenerator == null) {
				throw new IllegalArgumentException("A class name generator for relationships is required.");
			}
			this.relationshipNameGenerator = relationshipNameGenerator;
			return this;
		}

		/**
		 * Changes the field name generator.
		 * @param fieldNameGenerator a new generator
		 * @return this builder
		 * @since 2023.8.0
		 */
		public Builder withFieldNameGenerator(FieldNameGenerator fieldNameGenerator) {

			if (fieldNameGenerator == null) {
				throw new IllegalArgumentException("A field name generator is required.");
			}
			this.fieldNameGenerator = fieldNameGenerator;
			return this;
		}

		/**
		 * Configures the targeted Java version.
		 * @param target the new target version
		 * @return this builder
		 */
		public Builder withTarget(JavaVersion target) {

			if (target == null) {
				throw new IllegalArgumentException("A java version is required.");
			}
			this.target = target;
			return this;
		}

		/**
		 * Configures the default target package.
		 * @param defaultPackage the target package
		 * @return this builder
		 */
		public Builder withDefaultPackage(String defaultPackage) {

			if (defaultPackage == null) {
				throw new IllegalArgumentException("A default package is required.");
			}
			this.defaultPackage = defaultPackage;
			return this;
		}

		/**
		 * Configures a static timestamp for the builder to be used.
		 * @param timestamp timestamp to write into the generated classes. Uses the
		 * current time when no timestamp is given. Expected format is
		 * {@link DateTimeFormatter#ISO_OFFSET_DATE_TIME}.
		 * @return this builder
		 */
		public Builder withTimestamp(String timestamp) {

			this.timestamp = timestamp;
			return this;
		}

		/**
		 * A path is not always necessary, for example in an annotation processor.
		 * @param path a path into which the files should be written
		 * @return this builder
		 */
		public Builder withPath(Path path) {

			this.path = path;
			return this;
		}

		/**
		 * Configure a prefix for the generated classes. Will only be used when no other
		 * naming generator is configured.
		 * @param prefix prepended to the names of generated classes.
		 * @return this builder
		 */
		public Builder withPrefix(String prefix) {

			this.prefix = prefix;
			return this;
		}

		/**
		 * Configure a suffix for the generated classes. Will only be used when no other
		 * naming generator is configured.
		 * @param suffix appended to the names of generated classes.
		 * @return this builder
		 */
		public Builder withSuffix(String suffix) {

			this.suffix = suffix;
			return this;
		}

		/**
		 * Should generated sources be marked as such.
		 * @param addAtGenerated set to {@literal true} to mark generated sources as
		 * generated
		 * @return this builder
		 */
		public Builder withAddAtGenerated(boolean addAtGenerated) {

			this.addAtGenerated = addAtGenerated;
			return this;
		}

		/**
		 * Configures the indentation style, aka Tabs vs. Spaces, I'll be watching.
		 * @param indentStyle the style to use
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
		 * Configures the indent size.
		 * @param indentSize the number of indents to use
		 * @return this builder
		 */
		public Builder withIndentSize(int indentSize) {
			this.indentSize = indentSize;
			return this;
		}

		/**
		 * Configures the types to exclude.
		 * @param excludes if {@literal null} or empty, no types will be excluded
		 * @return this builder
		 */
		public Builder withExcludes(String excludes) {
			if (excludes == null || excludes.isBlank()) {
				this.excludes = Set.of();
			}
			else {
				this.excludes = Arrays.stream(excludes.split(","))
					.map(String::trim)
					.filter(Predicate.not(String::isBlank))
					.collect(Collectors.toUnmodifiableSet());
			}
			return this;
		}

		/**
		 * {@return a new, immutable configuration}
		 */
		public Configuration build() {

			UnaryOperator<String> typeNameDecorator;
			if (this.prefix == null && this.suffix == null) {
				typeNameDecorator = UnaryOperator.identity();
			}
			else {
				typeNameDecorator = s -> ((this.prefix != null) ? this.prefix.trim() : "") + s
						+ ((this.suffix != null) ? this.suffix.trim() : "");
			}

			String indent;
			if (this.indentStyle == IndentStyle.TAB) {
				indent = "\t";
			}
			else {
				indent = " ".repeat(Math.max(0, this.indentSize));
			}

			Clock clock = null;
			if (this.timestamp != null && !this.timestamp.isEmpty()) {
				ZonedDateTime z = ZonedDateTime.from(DateTimeFormatter.ISO_OFFSET_DATE_TIME.parse(this.timestamp));
				clock = Clock.fixed(z.toInstant(), z.getZone());
			}
			return new Configuration(typeNameDecorator, this.nodeNameGenerator, this.relationshipNameGenerator,
					this.fieldNameGenerator, this.target, this.defaultPackage, this.path, indent, clock,
					this.addAtGenerated, this.excludes);
		}

	}

}
