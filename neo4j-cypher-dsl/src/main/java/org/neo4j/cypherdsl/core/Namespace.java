package org.neo4j.cypherdsl.core;


import static org.apiguardian.api.API.Status.INTERNAL;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.support.Visitable;
import org.neo4j.cypherdsl.core.support.Visitor;

/**
 * @author Michael J. Simons
 * @since 2020.1
 * @soundtrack Apocalyptica - Cell-0
 */
@API(status = INTERNAL, since = "2020.1")
public final class Namespace implements Visitable {

	private final SymbolicName value;

	Namespace(SymbolicName name) {

		this.value = name;
	}

	@Override
	public void accept(Visitor visitor) {

		visitor.enter(this);
		this.value.accept(visitor);
		visitor.leave(this);
	}
}
