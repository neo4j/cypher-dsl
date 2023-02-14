package org.neo4j.cypherdsl.core.fump;

import java.util.Objects;

import org.neo4j.cypherdsl.core.NodeLabel;

/**
 * @author Michael J. Simons
 * @param type  The type of this token
 * @param value The concrete value
 * @soundtrack Avenger - Prayers Of Steel
 * @since TBA
 */
public record Token(Type type, String value) {

	/**
	 * Turns a specific {@link NodeLabel label} into a more abstract token.
	 * @param label A label, must not be {@literal null}.
	 * @return A token
	 */
	static Token of(NodeLabel label) {
		return new Token(Type.LABEL, Objects.requireNonNull(label, "Label must not be null.").getValue());
	}

	enum Type {
		LABEL,
		RELATIONSHIP
	}
}
