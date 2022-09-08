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
package org.neo4j.cypherdsl.support.schema_name;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A <a href="https://s3.amazonaws.com/artifacts.opencypher.org/M18/railroad/SchemaName.html">SchemaName</a> can appear
 * according to the OpenCypher-Spec all the following places:
 *
 * <ul>
 *     <li>NodePattern aka Label</li>
 *     <li>RelationshipPattern aka Type</li>
 *     <li>PropertyLookup</li>
 * </ul>
 *
 * Some papers refer to a schema name as symbolic names, too. A symbolic name in the context of the Cypher-DSL is usually
 * used to describe variables inside a statement, so we stick with the term schema name here.<p>
 *
 * A schema name must be escaped in statements with backticks - also known as grave accents - ({@code `}) when it contains
 * content that is not a valid identifier or if the name is a reserved word.<p>
 *
 * Backticks themselves must be escaped with a backtick itself: {@code ``}.
 * <strong>We always treat two consecutive backticks as escaped backticks.</strong> An odd number of backticks will
 * always lead to another backtick being inserted so that the single one will be escaped proper. As a concrete example
 * this means an input like {@code ```} will be sanitised and quoted as {@code ``````}. These are one leading and one
 * closing backtick as this schema name needs to be quoted plus one more to escape the outlier. When used as a label or
 * type, the resulting label will be {@code ``}.<p>
 *
 * This utility can be used standalone, the distributed module does not have any dependencies. Shading is ok as well,
 * there is no encapsulation to break.
 *
 * @author Michael J. Simons
 * @since 2022.8.0
 */
public final class SchemaNames {

	private static final String ESCAPED_UNICODE_BACKTICK = "\\u0060";

	private static final Pattern PATTERN_ESCAPED_4DIGIT_UNICODE = Pattern.compile("\\\\u+(\\p{XDigit}{4})");
	private static final Pattern PATTERN_LABEL_AND_TYPE_QUOTATION = Pattern.compile("(?<!`)`(?:`{2})*(?!`)");

	private static final List<String[]> SUPPORTED_ESCAPE_CHARS = Collections.unmodifiableList(Arrays.asList(
		new String[] { "\\b", "\b" },
		new String[] { "\\f", "\f" },
		new String[] { "\\n", "\n" },
		new String[] { "\\r", "\r" },
		new String[] { "\\t", "\t" },
		new String[] { "\\`", "``" }
	));

	private static final int CACHE_SIZE = 128;

	private static class CacheKey {

		private final String value;

		private final int major;

		private final int minor;

		CacheKey(String value, int major, int minor) {
			this.value = value;
			this.major = major;
			this.minor = minor;
		}

		@Override public boolean equals(Object o) {
			if (this == o) {
				return true;
			}
			if (o == null || getClass() != o.getClass()) {
				return false;
			}
			CacheKey cacheKey = (CacheKey) o;
			return major == cacheKey.major && minor == cacheKey.minor && value.equals(cacheKey.value);
		}

		@Override public int hashCode() {
			return Objects.hash(value, major, minor);
		}
	}

	private static class SchemaName {

		private final String value;

		private final boolean needsQuotation;

		SchemaName(String value, boolean needsQuotation) {
			this.value = value;
			this.needsQuotation = needsQuotation;
		}
	}

	/**
	 * Cypher-DSL has a concrete implementation of such a simple cache, but it should be in this module itself.
	 */
	private static final Map<CacheKey, SchemaName> CACHE = Collections.synchronizedMap(
		new LinkedHashMap<CacheKey, SchemaName>(CACHE_SIZE / 4, 0.75f, true) {
			private static final long serialVersionUID = -8109893585632797360L;

			@Override
			protected boolean removeEldestEntry(Map.Entry<CacheKey, SchemaName> eldest) {
				return size() >= CACHE_SIZE;
			}
		});

	/**
	 * Sanitizes the given input to be used as a valid schema name, adds quotes if necessary
	 *
	 * @param value The value to sanitize
	 * @return A value that is safe to be used in string concatenation, an empty optional indicates a value that cannot be safely quoted
	 */
	public static Optional<String> sanitize(String value) {
		return sanitize(value, false);
	}

	/**
	 * Sanitizes the given input to be used as a valid schema name
	 *
	 * @param value The value to sanitize
	 * @param enforceQuotes If quotation should be enforced, even when not necessary
	 * @return A value that is safe to be used in string concatenation, an empty optional indicates a value that cannot be safely quoted
	 */
	public static Optional<String> sanitize(String value, boolean enforceQuotes) {
		return sanitize(value, enforceQuotes, -1, -1);
	}

	/**
	 * Sanitizes the given input to be used as a valid schema name
	 *
	 * @param value The value to sanitize
	 * @param enforceQuotes If quotation should be enforced, even when not necessary
	 * @param major Neo4j major version, use a value &lt; 0 to assume the latest major version
	 * @param minor Neo4j minor version, use a value &lt; to assume any minor version
	 * @return A value that is safe to be used in string concatenation, an empty optional indicates a value that cannot be safely quoted
	 */
	public static Optional<String> sanitize(String value, boolean enforceQuotes, int major, int minor) {

		if (major >= 0 && (major < 3 || major > 6)) {
			throw new IllegalArgumentException("Unsupported major version: " + major);
		}

		if (value == null || value.isEmpty()) {
			return Optional.empty();
		}

		CacheKey cacheKey = new CacheKey(value, major < 0 ? -1 : major, minor < 0 ? -1 : minor);
		SchemaName escapedValue = CACHE.computeIfAbsent(cacheKey, SchemaNames::sanitze);

		if (!(enforceQuotes || escapedValue.needsQuotation)) {
			return Optional.of(escapedValue.value);
		}

		return Optional.of(String.format(Locale.ENGLISH, "`%s`", escapedValue.value));
	}

	private static SchemaName sanitze(CacheKey key) {

		String workingValue = key.value;

		// Replace current and future escaped chars
		for (String[] pair : SUPPORTED_ESCAPE_CHARS) {
			workingValue = workingValue.replace(pair[0], pair[1]);
		}
		workingValue = workingValue.replace(ESCAPED_UNICODE_BACKTICK, "`");

		// Replace escaped octal hex
		// Excluding the support for 6 digit literals, as this contradicts the overall example in CIP-59r
		Matcher matcher = PATTERN_ESCAPED_4DIGIT_UNICODE.matcher(workingValue);
		StringBuffer sb = new StringBuffer();
		while (matcher.find()) {
			String replacement = Character.toString((char) Integer.parseInt(matcher.group(1), 16));
			matcher.appendReplacement(sb, Matcher.quoteReplacement(replacement));
		}
		matcher.appendTail(sb);
		workingValue = sb.toString();

		if (substituteRemainingEscapedUnicodeLiteral(key.major, key.minor)) {
			workingValue = workingValue.replace("\\u", "\\u005C\\u0075");
		}

		matcher = PATTERN_LABEL_AND_TYPE_QUOTATION.matcher(workingValue);
		workingValue = matcher.replaceAll("`$0");

		if (unescapeEscapedBackslashes(key.major)) {
			workingValue = workingValue.replace("\\\\", "\\");
		}

		return new SchemaName(workingValue, !isIdentifier(workingValue));
	}

	/**
	 * True if the start of an escaped unicode literal {@code \\u} should be obsfuscated by two escaped literals for {@code \} and {@code u}.
	 *
	 * @param major neo4j Major version
	 * @param minor Neo4j minor version
	 * @return {@literal true} if the beginning of an escaped unicude literal needs special treatment
	 */
	private static boolean substituteRemainingEscapedUnicodeLiteral(int major, int minor) {
		if (major == -1) {
			return true;
		}
		return major >= 4 && major <= 5 && (minor == -1 || minor >= 2);
	}

	/**
	 * @param major neo4j Major version
	 * @return {@literal true} if escaped backslashes aren't supported and must be unescaped
	 */
	private static boolean unescapeEscapedBackslashes(int major) {
		return major <= 5;
	}

	/**
	 * This is a literal copy of {@code javax.lang.model.SourceVersion#isIdentifier(CharSequence)} included here to
	 * be not dependent on the compiler module.
	 *
	 * @param name A possible Java identifier
	 * @return True, if {@code name} represents an identifier.
	 */
	private static boolean isIdentifier(CharSequence name) {

		String id = name.toString();
		int cp = id.codePointAt(0);
		if (!Character.isJavaIdentifierStart(cp)) {
			return false;
		}
		for (int i = Character.charCount(cp); i < id.length(); i += Character.charCount(cp)) {
			cp = id.codePointAt(i);
			if (!Character.isJavaIdentifierPart(cp)) {
				return false;
			}
		}
		return true;
	}

	private SchemaNames() {
	}
}
