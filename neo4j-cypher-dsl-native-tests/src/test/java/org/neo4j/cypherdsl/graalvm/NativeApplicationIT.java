/*
 * Copyright (c) 2019-2021 "Neo4j,"
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
package org.neo4j.cypherdsl.graalvm;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Paths;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * This test is run via fail safe after GraalVM native image has build the application.
 *
 * @author Michael J. Simons
 * @soundtrack Bad Religion - Faith Alone 2020
 */
class NativeApplicationIT {

	@Test
	public void f() throws IOException, ExecutionException, InterruptedException {

		var statements = List.of(
			"MATCH (m:`Movie`) RETURN m",
			"MATCH (person:`Person`) RETURN person{livesIn: [(person)-[:`LIVES_IN`]->(personLivesIn:`Location`) | personLivesIn{.name}][$personLivedInOffset..($personLivedInOffset + $personLivedInFirst)]}"
		);

		var p = new ProcessBuilder(Paths.get(".", "target", "application").toAbsolutePath().normalize().toString())
			.start();

		p.onExit().thenAccept(done -> {
			try (var in = new BufferedReader(new InputStreamReader(done.getInputStream()))) {
				var generatedStatements = in.lines().collect(Collectors.toSet());
				Assertions.assertTrue(generatedStatements.containsAll(statements));
			} catch (IOException e) {
			}
		}).get();
	}
}
