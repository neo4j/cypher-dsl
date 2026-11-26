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
package org.neo4j.cypherdsl.support.schema_name;

import java.io.Serial;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A <a href=
 * "https://s3.amazonaws.com/artifacts.opencypher.org/M18/railroad/SchemaName.html">SchemaName</a>
 * can appear according to the OpenCypher-Spec all the following places:
 *
 * <ul>
 * <li>NodePattern aka Label</li>
 * <li>RelationshipPattern aka Type</li>
 * <li>PropertyLookup</li>
 * </ul>
 *
 * Some papers refer to a schema name as symbolic names, too. A symbolic name in the
 * context of the Cypher-DSL is usually used to describe variables inside a statement, so
 * we stick with the term schema name here.
 * <p>
 *
 * A schema name must be escaped in statements with backticks - also known as grave
 * accents - ({@code `}) when it contains content that is not a valid identifier or if the
 * name is a reserved word.
 * <p>
 *
 * Backticks themselves must be escaped with a backtick itself: {@code ``}. <strong>We
 * always treat two consecutive backticks as escaped backticks.</strong> An odd number of
 * backticks will always lead to another backtick being inserted so that the single one
 * will be escaped proper. As a concrete example this means an input like {@code ```} will
 * be sanitised and quoted as {@code ``````}. These are one leading and one closing
 * backtick as this schema name needs to be quoted plus one more to escape the outlier.
 * When used as a label or type, the resulting label will be {@code ``}.
 * <p>
 *
 * This utility can be used standalone, the distributed module does not have any
 * dependencies. Shading is ok as well, there is no encapsulation to break.
 *
 * @author Michael J. Simons
 * @since 2022.8.0
 */
public final class SchemaNames {

	private static final String ESCAPED_BACKSLASH = "\\u005C";

	private static final String ESCAPED_U = "\\u0075";

	private static final Pattern PATTERN_BACKSLASHES_BEFORE_U = Pattern.compile("(?<!\\\\)(\\\\++)u");

	private static final Pattern PATTERN_LABEL_AND_TYPE_QUOTATION = Pattern.compile("(?<!`)`(?:`{2})*(?!`)");

	private static final int CACHE_SIZE = 128;

	/**
	 * Cypher-DSL has a concrete implementation of such a simple cache, but it should be
	 * in this module itself.
	 */
	private static final Map<CacheKey, SchemaName> CACHE = Collections
		.synchronizedMap(new LinkedHashMap<>(CACHE_SIZE / 4, 0.75f, true) {
			@Serial
			private static final long serialVersionUID = -8109893585632797360L;

			@Override
			protected boolean removeEldestEntry(Map.Entry<CacheKey, SchemaName> eldest) {
				return size() >= CACHE_SIZE;
			}
		});

	private SchemaNames() {
	}

	/**
	 * Sanitizes the given input to be used as a valid schema name, adds quotes if
	 * necessary.
	 * @param value the value to sanitize
	 * @return a value that is safe to be used in string concatenation, an empty optional
	 * indicates a value that cannot be safely quoted
	 */
	public static Optional<String> sanitize(String value) {
		return sanitize(value, false);
	}

	/**
	 * Sanitizes the given input to be used as a valid schema name.
	 * @param value the value to sanitize
	 * @param enforceQuotes if quotation should be enforced, even when not necessary
	 * @return a value that is safe to be used in string concatenation, an empty optional
	 * indicates a value that cannot be safely quoted
	 */
	public static Optional<String> sanitize(String value, boolean enforceQuotes) {
		return sanitize(value, enforceQuotes, -1, -1);
	}

	/**
	 * Sanitizes the given input to be used as a valid schema name.
	 * @param value the value to sanitize
	 * @param enforceQuotes if quotation should be enforced, even when not necessary
	 * @param major the Neo4j major version, use a value &lt; 0 to assume the latest major
	 * version
	 * @param minor the Neo4j minor version, use a value &lt; 0 to assume any minor
	 * version
	 * @return a value that is safe to be used in string concatenation, an empty optional
	 * indicates a value that cannot be safely quoted
	 */
	public static Optional<String> sanitize(String value, boolean enforceQuotes, int major, int minor) {

		if (major >= 0 && (major < 3 || (major > 5 && major < 2025))) {
			throw new IllegalArgumentException("Unsupported major version: " + major);
		}

		if (value == null || value.isEmpty()) {
			return Optional.empty();
		}

		CacheKey cacheKey = new CacheKey(value, (major < 0) ? -1 : major, (minor < 0) ? -1 : minor);
		SchemaName escapedValue = CACHE.computeIfAbsent(cacheKey, SchemaNames::sanitize);

		if (!(enforceQuotes || escapedValue.needsQuotation)) {
			return Optional.of(escapedValue.value);
		}

		return Optional.of(String.format(Locale.ENGLISH, "`%s`", escapedValue.value));
	}

	private static SchemaName sanitize(CacheKey key) {

		String workingValue = decodeEscapes(key.value);

		if (substituteRemainingEscapedUnicodeLiteral(key.major, key.minor)) {
			workingValue = obfuscateUnicodeEscapeStarts(workingValue);
		}

		Matcher matcher = PATTERN_LABEL_AND_TYPE_QUOTATION.matcher(workingValue);
		workingValue = matcher.replaceAll("`$0");

		return new SchemaName(workingValue, !isIdentifier(workingValue));
	}

	/**
	 * Decodes Cypher escape sequences in a single pass from left to right, so that an
	 * escaped backslash is never treated as the start of another escape sequence:
	 * {@code \\u0060} is a literal backslash followed by the text {@code u0060}. Decoded
	 * characters are not scanned again. Supported are {@code \\}, {@code \b}, {@code \f},
	 * {@code \n}, {@code \r}, {@code \t}, {@code \`} (an escaped backtick) and 4-digit
	 * Unicode escapes with one or more {@code u}. Excluding the support for 6 digit
	 * literals, as this contradicts the overall example in CIP-59r. A backslash not
	 * starting a supported escape sequence is taken literally.
	 * @param value the value to decode
	 * @return the decoded value
	 */
	private static String decodeEscapes(String value) {

		int length = value.length();
		StringBuilder sb = new StringBuilder(length);
		int i = 0;
		while (i < length) {
			char c = value.charAt(i);
			if (c != '\\' || i + 1 == length) {
				sb.append(c);
				++i;
				continue;
			}

			char next = value.charAt(i + 1);
			String replacement = switch (next) {
				case '\\' -> "\\";
				case 'b' -> "\b";
				case 'f' -> "\f";
				case 'n' -> "\n";
				case 'r' -> "\r";
				case 't' -> "\t";
				case '`' -> "``";
				default -> null;
			};
			if (replacement != null) {
				sb.append(replacement);
				i += 2;
				continue;
			}

			if (next == 'u') {
				int start = i + 1;
				while (start < length && value.charAt(start) == 'u') {
					++start;
				}
				if (isHex4(value, start)) {
					sb.append((char) Integer.parseInt(value.substring(start, start + 4), 16));
					i = start + 4;
					continue;
				}
			}

			sb.append(c);
			++i;
		}
		return sb.toString();
	}

	private static boolean isHex4(String value, int start) {
		if (start + 4 > value.length()) {
			return false;
		}
		for (int i = start; i < start + 4; ++i) {
			char c = value.charAt(i);
			if (!((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))) {
				return false;
			}
		}
		return true;
	}

	/**
	 * Obfuscates every run of backslashes followed by a {@code u} with escaped literals,
	 * so that Neo4j neither decodes nor rejects it as a Unicode escape. The whole run is
	 * replaced, as Neo4j does not consider a backslash preceded by an odd number of
	 * backslashes to be the start of a Unicode escape.
	 * @param value the decoded value
	 * @return the obfuscated value
	 */
	private static String obfuscateUnicodeEscapeStarts(String value) {
		return PATTERN_BACKSLASHES_BEFORE_U.matcher(value)
			.replaceAll(r -> Matcher.quoteReplacement(ESCAPED_BACKSLASH.repeat(r.group(1).length()) + ESCAPED_U));
	}

	/**
	 * True if the start of an escaped Unicode literal {@code \\u} should be obfuscated by
	 * two escaped literals for {@code \} and {@code u}.
	 * @param major the Neo4j Major version
	 * @param minor the Neo4j minor version
	 * @return {@literal true} if the beginning of an escaped Unicode literal needs
	 * special treatment
	 */
	private static boolean substituteRemainingEscapedUnicodeLiteral(int major, int minor) {
		if (major == -1) {
			return true;
		}
		return major >= 4 && major <= 5 && (minor == -1 || minor >= 2) || major >= 2025;
	}

	/**
	 * This is a literal copy of
	 * {@code javax.lang.model.SourceVersion#isIdentifier(CharSequence)} included here to
	 * be not dependent on the compiler module.
	 * @param name a possible Java identifier
	 * @return true, if {@code name} represents an identifier.
	 */
	private static boolean isIdentifier(CharSequence name) {

		String id = name.toString();
		int cp = id.codePointAt(0);
		if (!Character.isJavaIdentifierStart(cp) || '$' == cp) {
			return false;
		}
		for (int i = Character.charCount(cp); i < id.length(); i += Character.charCount(cp)) {
			cp = id.codePointAt(i);
			if (!Character.isJavaIdentifierPart(cp) || '$' == cp) {
				return false;
			}
		}

		return true;
	}

	private record CacheKey(String value, int major, int minor) {
	}

	private record SchemaName(String value, boolean needsQuotation) {
	}

}
