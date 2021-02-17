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
package org.neo4j.cypherdsl.core.renderer;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.util.Objects;

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
@API(status = EXPERIMENTAL, since = "2021.0.1")
public final class Configuration {

	private static final Configuration DEFAULT_CONFIG = newConfig().build();
	private static final Configuration PRETTY_PRINTING = newConfig().withPrettyPrint(true).build();

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
	 * Set to {@literal true} to enable pretty printing.
	 */
	private final boolean prettyPrint;

	/**
	 * Configure your favorite indentation style.
	 */
	private final IndentStyle indentStyle;

	/**
	 * The indentation sizes. Only applicable when using {@link IndentStyle#SPACE}. Defaults to {@literal 4}.
	 */
	private final int indentSize;

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

		private Builder() {
		}

		private static Builder newConfig() {
			return new Builder();
		}

		public Builder withPrettyPrint(boolean prettyPrint) {
			this.prettyPrint = prettyPrint;
			return this;
		}

		public Builder withIndentStyle(IndentStyle indentStyle) {
			this.indentStyle = indentStyle;
			return this;
		}

		public Builder withIndentSize(int indentSize) {
			this.indentSize = indentSize;
			return this;
		}

		public Builder but() {
			return newConfig().withPrettyPrint(prettyPrint).withIndentStyle(indentStyle)
				.withIndentSize(indentSize);
		}

		public Configuration build() {
			return new Configuration(prettyPrint, indentStyle, indentSize);
		}
	}

	private Configuration(boolean prettyPrint, IndentStyle indentStyle, int indentSize) {
		this.prettyPrint = prettyPrint;
		this.indentStyle = indentStyle == null ? IndentStyle.TAB : indentStyle;
		this.indentSize = indentSize;
	}

	public boolean isPrettyPrint() {
		return prettyPrint;
	}

	public IndentStyle getIndentStyle() {
		return indentStyle;
	}

	public int getIndentSize() {
		return indentSize;
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
		return prettyPrint == that.prettyPrint && indentSize == that.indentSize && indentStyle == that.indentStyle;
	}

	@Override
	public int hashCode() {
		return Objects.hash(prettyPrint, indentStyle, indentSize);
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
