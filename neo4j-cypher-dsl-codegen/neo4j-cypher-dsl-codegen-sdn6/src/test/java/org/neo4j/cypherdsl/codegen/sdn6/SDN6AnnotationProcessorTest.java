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
package org.neo4j.cypherdsl.codegen.sdn6;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Arrays;

import javax.tools.JavaFileObject;
import javax.tools.ToolProvider;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.converter.ArgumentConversionException;
import org.junit.jupiter.params.converter.ConvertWith;
import org.junit.jupiter.params.converter.SimpleArgumentConverter;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.core.convert.converter.Converter;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.lang.NonNull;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.CompilationSubject;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;

/**
 * @author Michael J. Simons
 * @soundtrack Slime - Viva la Muerte
 */
class SDN6AnnotationProcessorTest {

	/**
	 * We have Spring on the Classpath anyway... So not reinvent the wheel for finding the resources.
	 */
	private final static PathMatchingResourcePatternResolver resourceResolver
		= new PathMatchingResourcePatternResolver();

	static Compiler getCompiler(Object... options) {

		String ts = "-Aorg.neo4j.cypherdsl.codegen.timestamp=2019-09-21T21:21:00+01:00";
		Object[] defaultOptions;

		if (ToolProvider.getSystemJavaCompiler().isSupportedOption("--release") >= 0) {
			defaultOptions = new Object[] { ts, "--release", "8" };
		} else {
			defaultOptions = new Object[] { ts, "-source", "8", "-target", "8" };
		}

		Object[] finalOptions = new Object[options.length + defaultOptions.length];
		System.arraycopy(defaultOptions, 0, finalOptions, 0, defaultOptions.length);
		System.arraycopy(options, 0, finalOptions, defaultOptions.length, options.length);

		return Compiler.javac().withOptions(finalOptions);
	}

	JavaFileObject[] getJavaResources(String base) {

		try {
			Resource[] resources = resourceResolver.getResources(base + "/**/*.java");
			JavaFileObject[] result = new JavaFileObject[resources.length];
			for (int i = 0; i < resources.length; i++) {
				result[i] = JavaFileObjects.forResource(resources[i].getURL());
			}
			return result;
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	/**
	 * A test converter spotting a non-default constructor, so that it cannot be instantiated
	 */
	@SuppressWarnings("unused")
	static class SomeConverter implements Converter<String, String> {

		SomeConverter(@SuppressWarnings("unused") boolean ignoreMe) {
		}

		@Override
		public String convert(@NonNull String source) {
			return new StringBuilder(source).reverse().toString();
		}
	}

	@ValueSource(strings = { "foo", "org.neo4j.cypherdsl.codegen.sdn6.SDN6AnnotationProcessorTest$SomeConverter",
		"org.neo4j.cypherdsl.codegen.sdn6.models.enums_and_inner_classes.InnerInnerClassConverter" })
	@ParameterizedTest
	void shouldNotFailWithInvalidConverters(String converter) {
		Compilation compilation = getCompiler("-Aorg.neo4j.cypherdsl.codegen.sdn.custom_converter_classes=" + converter)
			.withProcessors(new SDN6AnnotationProcessor())
			.compile(getJavaResources("org/neo4j/cypherdsl/codegen/sdn6/models/simple"));

		CompilationSubject.assertThat(compilation).succeeded();
		String expectedMessage;
		if (converter.endsWith("InnerInnerClassConverter")) {
			expectedMessage = "Cannot use dedicated Neo4j persistent property converter of type `" + converter + "` as Spring converter, it will be ignored.";
		} else {
			expectedMessage = "Cannot load converter of type `" + converter + "`, it will be ignored: ";
		}
		CompilationSubject.assertThat(compilation).hadWarningContaining(expectedMessage);
	}

	@Test
	void shouldRecognizeGlobalConvertersOnInnerClasses() {
		Compilation compilation = getCompiler(
			"-Aorg.neo4j.cypherdsl.codegen.sdn.custom_converter_classes=org.neo4j.cypherdsl.codegen.sdn6.models.enums_and_inner_classes.SpringBasedConverter")
			.withProcessors(new SDN6AnnotationProcessor())
			.compile(getJavaResources("org/neo4j/cypherdsl/codegen/sdn6/models/enums_and_inner_classes"));

		CompilationSubject.assertThat(compilation).succeeded();
		CompilationSubject.assertThat(compilation)
			.generatedSourceFile(
				"org.neo4j.cypherdsl.codegen.sdn6.models.enums_and_inner_classes.ConnectorTransport_")
			.hasSourceEquivalentTo(
				JavaFileObjects.forResource("enums_and_inner_classes/ConnectorTransportWithGlobalConverter_.java"));
	}

	@CsvSource({
		"ids, 'InternalGeneratedId, InternalGeneratedIdWithSpringId, ExternalGeneratedId, ExternalGeneratedIdImplicit, InternalGeneratedPrimitiveLongId',",
		"simple, 'Person, Movie, ActedIn, Follows, Directed, Produced',",
		"labels, 'LabelOnNode1, LabelOnNode2, LabelOnNode3, MultipleLabels1, MultipleLabels2, MultipleLabels3', nodeswithdifferentlabelannotations",
		"same_properties_for_rel_type, 'Person, Movie, Play, ActedIn', ",
		"different_properties_for_rel_type, 'Person, Movie, Play, ActedInPlay, ActedInMovie', ",
		"same_rel_different_target, 'Person, Movie, Book, Wrote', ",
		"same_rel_different_source, 'Person, Movie, Book, Wrote', ",
		"same_rel_mixed, 'Person, Movie, Book, Wrote', ",
		"same_rel_mixed_different_directions, 'Person, Movie, Book, Wrote', ",
		"abstract_rels, 'Person, Movie, Directed',",
		"primitives, 'Connector', ",
		"enums_and_inner_classes, 'ConnectorTransport', ",
		"related_classes_not_on_cp_like_in_reallife, 'Movie, Person', ",
		"self_referential, 'Example', "
	})
	@ParameterizedTest
	void validSourceFiles(String scenario, @ConvertWith(StringArrayConverter.class) String[] expected,
		String subpackage) {

		Compilation compilation = getCompiler()
			.withProcessors(new SDN6AnnotationProcessor())
			.compile(getJavaResources("org/neo4j/cypherdsl/codegen/sdn6/models/" + scenario));

		CompilationSubject.assertThat(compilation).succeeded();
		if ("abstract_rels".equals(scenario)) {
			CompilationSubject.assertThat(compilation).hadWarningContaining(
				"Cannot resolve generic type, not generating a property for relationships referring to org.neo4j.cypherdsl.codegen.sdn6.models.abstract_rels.Actor");
		} else {
			CompilationSubject.assertThat(compilation).hadWarningCount(0);
		}

		for (String expectedSourceFile : expected) {
			String finalName = scenario + "." + (subpackage == null ? "" : subpackage + ".") + expectedSourceFile + "_";
			CompilationSubject.assertThat(compilation)
				.generatedSourceFile("org.neo4j.cypherdsl.codegen.sdn6.models." + finalName)
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
