package org.neo4j.cypherdsl.core.fump;

import java.util.List;

public class Things {

	private final List<Token> tokens;

	// TODO make stuff immutable
	public Things(List<Token> tokens) {
		this.tokens = tokens;
	}
}
