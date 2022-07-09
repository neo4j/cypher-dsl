package org.neo4j.cypherdsl.core;

import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.core.internal.LiteralBase;

import java.util.Optional;

/**
 * The string will be added without single quotes in the Cypher statement
 *
 * @author AakashSorathiya
*/
public final class EscapeQuoteStringLiteral extends LiteralBase<CharSequence> {

    EscapeQuoteStringLiteral(CharSequence content) {
        super(content);
    }

    @NotNull
    @Override
    public String asString() {
        final Optional<String> escapedContent = StringLiteral.escapeString(getContent());
        return escapedContent.orElse("");
    }
}