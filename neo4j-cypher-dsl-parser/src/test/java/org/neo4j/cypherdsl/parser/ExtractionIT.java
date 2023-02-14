package org.neo4j.cypherdsl.parser;

import java.util.List;
import java.util.stream.Stream;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.neo4j.cypherdsl.core.fump.Thing;

/**
 * @author Michael J. Simons
 * @author Christophe Willemsen
 */
class ExtractionIT {

	static Stream<org.junit.jupiter.params.provider.Arguments> extractionShouldWork() {
		return TEST_DATA.stream().map(Arguments::of);
	}

	@SuppressWarnings("removal")
	@ParameterizedTest
	@MethodSource
	void extractionShouldWork(TestData testData) {
		var statement = CypherParser.parse(testData.query());
		Thing.getThings(statement);
		System.out.println("-");
	}

	record TestData(String query) {
	}

	static final List<TestData> TEST_DATA = List.of(
		/**
		 * labels matched [Person]
		 * Matched property `name` on label `Person`
		 */
		new TestData("""
			MATCH (n:Person {name: $name})
			RETURN n, COUNT { (n)--() } AS degree
			"""
		),
		new TestData("""
			MATCH (n:Person) WHERE n.name = $name
			RETURN n, COUNT { (n)--() } AS degree
			"""
		)
	);
}
