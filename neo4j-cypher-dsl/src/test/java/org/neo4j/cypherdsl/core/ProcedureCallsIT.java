package org.neo4j.cypherdsl.core;

import static org.assertj.core.api.Assertions.assertThat;

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

			StandaloneCall call = Cypher.call("db", "labels");
			assertThat(cypherRenderer.render(call)).isEqualTo("CALL db.labels");

			call = Cypher.call("db.labels");
			assertThat(cypherRenderer.render(call)).isEqualTo("CALL db.labels");
		}
	}
}
