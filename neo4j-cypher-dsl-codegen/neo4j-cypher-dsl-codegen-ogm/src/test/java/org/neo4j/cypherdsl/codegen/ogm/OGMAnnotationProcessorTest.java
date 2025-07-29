/*
 * Copyright (c) 2019-2025 "Neo4j,"
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
package org.neo4j.cypherdsl.codegen.ogm;

import io.github.classgraph.ClassGraph;
import io.github.classgraph.ScanResult;

import java.util.Arrays;

import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.converter.ArgumentConversionException;
import org.junit.jupiter.params.converter.ConvertWith;
import org.junit.jupiter.params.converter.SimpleArgumentConverter;
import org.junit.jupiter.params.provider.CsvSource;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.CompilationSubject;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;

/**
 * @author Michael J. Simons
 * @author Shinigami
 */
class OGMAnnotationProcessorTest {

	static Compiler getCompiler(Object... options) {

		String ts = "-Aorg.neo4j.cypherdsl.codegen.timestamp=2025-09-21T21:21:00+01:00";
		Object[] defaultOptions;

		if (ToolProvider.getSystemJavaCompiler().isSupportedOption("--release") >= 0) {
			// release 8 deprecated since Java 21, suppressed with -options
			defaultOptions = new Object[] { "-Xlint:-options", ts, "--release", "8" };
		} else {
			defaultOptions = new Object[] { ts, "-source", "8", "-target", "8" };
		}

		Object[] finalOptions = new Object[options.length + defaultOptions.length];
		System.arraycopy(defaultOptions, 0, finalOptions, 0, defaultOptions.length);
		System.arraycopy(options, 0, finalOptions, defaultOptions.length, options.length);

		return Compiler.javac().withOptions(finalOptions);
	}

	JavaFileObject[] getJavaResources(String base) {
		try (ScanResult scanResult = new ClassGraph().acceptPaths(base).scan()) {
			return scanResult.getResourcesWithExtension("java")
				.stream()
				.map(resource -> JavaFileObjects.forResource(resource.getURL()))
				.toArray(JavaFileObject[]::new);
		}
	}

	@CsvSource({
		"ids, 'InternalGeneratedId, ExternalGeneratedId, ExternalGeneratedIdImplicit, InternalGeneratedPrimitiveLongId',",
		"labels, 'LabelOnNode1, LabelOnNode2', nodeswithdifferentlabelannotations",
		"primitives, 'Connector', ",
	})
	@ParameterizedTest
	void validSourceFiles(String scenario, @ConvertWith(StringArrayConverter.class) String[] expected,
		String subpackage) {

		Compilation compilation = getCompiler()
			.withProcessors(new OGMAnnotationProcessor())
			.compile(getJavaResources("/org/neo4j/cypherdsl/codegen/ogm/models/" + scenario));

		CompilationSubject.assertThat(compilation).succeeded();
		CompilationSubject.assertThat(compilation).hadWarningCount(0);

		for (String expectedSourceFile : expected) {
			String finalName = scenario + "." + (subpackage == null ? "" : subpackage + ".") + expectedSourceFile + "_";
			CompilationSubject.assertThat(compilation)
				.generatedSourceFile("org.neo4j.cypherdsl.codegen.ogm.models." + finalName)
				.hasSourceEquivalentTo(JavaFileObjects.forResource(finalName.replaceAll("\\.", "/") + ".java"));
		}
	}

	static class StringArrayConverter extends SimpleArgumentConverter {

		@Override
		protected Object convert(Object source, Class<?> targetType) throws ArgumentConversionException {
			if (source instanceof String && String[].class.isAssignableFrom(targetType)) {
				return Arrays.stream(((String) source).split("\\s*,\\s*")).map(String::trim).toArray(String[]::new);
			} else {
				throw new IllegalArgumentException(
					"Conversion from " + source.getClass() + " to " + targetType + " not supported.");
			}
		}
	}
}
