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
 * @author Shinigami92 (Christopher Quadflieg)
 */
class OGMAnnotationProcessorTest {

	static Compiler getCompiler(String release, Object... options) {

		String ts = "-Aorg.neo4j.cypherdsl.codegen.timestamp=2025-09-21T21:21:00+01:00";
		Object[] defaultOptions;

		if (ToolProvider.getSystemJavaCompiler().isSupportedOption("--release") >= 0) {
			// release 8 deprecated since Java 21, surprised with -options
			defaultOptions = new Object[] { "-Xlint:-options", ts, "--release", release };
		} else if ("8".equals(release)) {
			defaultOptions = new Object[] { ts, "-source", "8", "-target", "8" };
		} else {
			throw new IllegalArgumentException("Release %s not supported for testing".formatted(release));
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
		"8, ids, 'InternalGeneratedId, ExternalGeneratedId, ExternalGeneratedIdImplicit, InternalGeneratedPrimitiveLongId',",
		"8, simple, 'Person, Movie, ActedIn, Follows, Directed, Produced',",
		"8, labels, 'LabelOnNode1, LabelOnNode2', nodeswithdifferentlabelannotations",
		"8, same_properties_for_rel_type, 'Person, Movie, Play, ActedIn', ",
		"8, different_properties_for_rel_type, 'Person, Movie, Play, ActedInPlay, ActedInMovie', ",
		"8, same_rel_different_target, 'Person, Movie, Book, Wrote', ",
		"8, same_rel_different_source, 'Person, Movie, Book, Wrote', ",
		"8, same_rel_mixed, 'Person, Movie, Book, Wrote', ",
		"8, same_rel_mixed_different_directions, 'Person, Movie, Book, Wrote', ",
		"8, abstract_rels, 'Person, Movie, Directed',",
		"8, primitives, 'Connector', ",
		"8, enums_and_inner_classes, 'ConnectorTransport', ",
		"8, related_classes_not_on_cp_like_in_reallife, 'Movie, Person', ",
		"8, self_referential, 'Example', ",
		"17, records, 'NodeWithRecordProperties, RecordAsRelationship, RecordTarget', "
	})
	@ParameterizedTest
	void validSourceFiles(String release, String scenario, @ConvertWith(StringArrayConverter.class) String[] expected,
		String subpackage) {

		Compilation compilation = getCompiler(release)
			.withProcessors(new OGMAnnotationProcessor())
			.compile(getJavaResources("/org/neo4j/cypherdsl/codegen/ogm/models/" + scenario));

		CompilationSubject.assertThat(compilation).succeeded();
		if ("abstract_rels".equals(scenario)) {
			CompilationSubject.assertThat(compilation).hadWarningContaining(
				"Cannot resolve generic type, not generating a property for relationships referring to org.neo4j.cypherdsl.codegen.ogm.models.abstract_rels.Actor");
		} else {
			CompilationSubject.assertThat(compilation).hadWarningCount(0);
		}

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
