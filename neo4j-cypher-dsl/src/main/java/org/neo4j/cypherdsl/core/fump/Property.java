package org.neo4j.cypherdsl.core.fump;

import java.util.Properties;
import java.util.Set;

/**
 * @param name The name of the resolved property
 * @param owningToken Zero or many owning tokens for a property
 */
record Property(String name, Set<Token> owningToken) {

	Property {
		owningToken = Set.copyOf(owningToken);
	}
}