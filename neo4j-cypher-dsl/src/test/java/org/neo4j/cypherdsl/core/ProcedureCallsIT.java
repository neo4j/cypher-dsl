package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;
import static org.neo4j.cypherdsl.core.Cypher.literalOf;

import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.neo4j.cypherdsl.core.renderer.Renderer;

/**
 * @author Michael J. Simons
 */
class ProcedureCallsIT {

	private static final Renderer cypherRenderer = Renderer.getDefaultRenderer();

	@Nested
	class StandaloneCalls {

		@Test
		void simple() {

			String expected = "CALL db.labels";

			StandaloneCall call = Cypher.call("db", "labels");
			assertThat(cypherRenderer.render(call)).isEqualTo(expected);

			call = Cypher.call("db.labels");
			assertThat(cypherRenderer.render(call)).isEqualTo(expected);
		}

		@Test
		void withArgs() {

			StandaloneCall call = Cypher.call("dbms.security.createUser").withArgs(literalOf("johnsmith"), literalOf("h6u4%kr"), BooleanLiteral.FALSE);
			assertThat(cypherRenderer.render(call)).isEqualTo("CALL dbms.security.createUser('johnsmith', 'h6u4%kr', false)");
		}
	}
}
