/*
 * Copyright (c) 2019-2022 "Neo4j,"
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
package org.neo4j.cypherdsl.build.processor;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Objects;
import java.util.stream.IntStream;

import javax.tools.JavaFileObject;
import javax.tools.StandardLocation;

import org.junit.jupiter.api.Test;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;

/**
 * @author Michael J. Simons
 */
class RegisterForReflectionProcessorTest {

	static Compiler getCompiler(Object... options) {

		Object[] defaultOptions = new Object[] { "--release", "17" };

		Object[] finalOptions = new Object[options.length + defaultOptions.length];
		System.arraycopy(defaultOptions, 0, finalOptions, 0, defaultOptions.length);
		System.arraycopy(options, 0, finalOptions, defaultOptions.length, options.length);

		return Compiler.javac().withOptions(finalOptions);
	}

	@Test
	void shouldOnlyGenerateReflectionConfigWithContentWhenClassesArePresent() {
		JavaFileObject[] resources = IntStream.range(0, 4)
			.mapToObj(this::getTestClassN)
			.toArray(JavaFileObject[]::new);

		Compilation compilation = getCompiler(
			"-Aorg.neo4j.cypherdsl.build.native_config_dir=foo/bar")
			.withProcessors(new RegisterForReflectionProcessor())
			.compile(resources);

		assertThat(compilation.status()).isEqualTo(Compilation.Status.SUCCESS);
		assertThat(compilation.warnings()).isEmpty();
		assertThat(compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
			"META-INF/native-image/foo/bar/reflection-config.json"))
			.map(f -> {
				try {
					return f.getCharContent(true);
				} catch (IOException e) {
					throw new UncheckedIOException(e);
				}
			})
			.hasValue("""
				[ {
				  "name" : "org.neo4j.cypherdsl.build.test.Class0",
				  "allDeclaredMethods" : true
				}, {
				  "name" : "org.neo4j.cypherdsl.build.test.Class3",
				  "allDeclaredConstructors" : true
				} ]"""
			);
	}

	@Test
	void shouldHandleEmptyDir() {
		Compilation compilation = getCompiler()
			.withProcessors(new RegisterForReflectionProcessor())
			.compile(getTestClassN(0));

		assertThat(compilation.status()).isEqualTo(Compilation.Status.SUCCESS);
		assertThat(compilation.warnings()).isEmpty();
		assertThat(compilation.generatedFile(StandardLocation.CLASS_OUTPUT, "META-INF/native-image/reflection-config.json"))
			.isPresent();
	}

	@Test
	void shouldHandleSlash() {
		Compilation compilation = getCompiler("-Aorg.neo4j.cypherdsl.build.native_config_dir=foo/")
			.withProcessors(new RegisterForReflectionProcessor())
			.compile(getTestClassN(0));

		assertThat(compilation.status()).isEqualTo(Compilation.Status.SUCCESS);
		assertThat(compilation.warnings()).isEmpty();
		assertThat(compilation.generatedFile(StandardLocation.CLASS_OUTPUT, "META-INF/native-image/foo/reflection-config.json"))
			.isPresent();
	}

	@Test
	void shouldNotGenerateEmpty() {
		JavaFileObject[] resources = IntStream.range(1, 3)
			.mapToObj(this::getTestClassN)
			.toArray(JavaFileObject[]::new);

		Compilation compilation = getCompiler(
			"-Aorg.neo4j.cypherdsl.build.native_config_dir=foo/bar")
			.withProcessors(new RegisterForReflectionProcessor())
			.compile(resources);

		assertThat(compilation.status()).isEqualTo(Compilation.Status.SUCCESS);
		assertThat(compilation.warnings()).isEmpty();
		assertThat(compilation.generatedFile(StandardLocation.CLASS_OUTPUT,
			"META-INF/native-image/foo/bar/reflection-config.json")).isEmpty();
	}

	private JavaFileObject getTestClassN(int i) {
		return JavaFileObjects.forResource(
			Objects.requireNonNull(this.getClass().getResource(String.format("/test_classes/Class%d.java", i))));
	}
}
