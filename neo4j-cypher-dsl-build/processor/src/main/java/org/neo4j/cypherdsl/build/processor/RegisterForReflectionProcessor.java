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
package org.neo4j.cypherdsl.build.processor;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Collection;
import java.util.Comparator;
import java.util.Set;
import java.util.TreeSet;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedOptions;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import javax.tools.FileObject;
import javax.tools.StandardLocation;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;

/**
 * Processor reacting on {@code @RegisterForReflection}.
 *
 * @author Michael J. Simons
 * @since 2022.2.2
 */
@SupportedAnnotationTypes("org.neo4j.cypherdsl.build.annotations.RegisterForReflection")
@SupportedOptions(RegisterForReflectionProcessor.NATIVE_IMAGE_SUBDIR_OPTION)
public final class RegisterForReflectionProcessor extends AbstractProcessor {

	static final String NATIVE_IMAGE_SUBDIR_OPTION = "org.neo4j.cypherdsl.build.native_config_dir";

	private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);

	private final Collection<Entry> entries = new TreeSet<>(Comparator.comparing(Entry::getName));

	static boolean registersElements(RegisterForReflection registerForReflection) {
		return registerForReflection.allDeclaredMethods() || registerForReflection.allDeclaredConstructors();
	}

	@Override
	public SourceVersion getSupportedSourceVersion() {
		return SourceVersion.latestSupported();
	}

	@Override
	public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

		if (roundEnv.processingOver() && !this.entries.isEmpty()) {
			try {
				String subDir = this.processingEnv.getOptions().getOrDefault(NATIVE_IMAGE_SUBDIR_OPTION, "");
				if (!(subDir.isEmpty() || subDir.endsWith("/"))) {
					subDir += "/";
				}
				String reflectionConfigPath = String.format("META-INF/native-image/%sreflection-config.json", subDir);
				FileObject fileObject = this.processingEnv.getFiler()
					.createResource(StandardLocation.CLASS_OUTPUT, "", reflectionConfigPath);
				try (OutputStream oos = fileObject.openOutputStream()) {
					OBJECT_MAPPER.writeValue(oos, this.entries);
				}
			}
			catch (IOException ex) {
				this.processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, ex.getMessage());
			}
		}
		else if (!annotations.isEmpty()) {
			roundEnv.getElementsAnnotatedWith(RegisterForReflection.class)
				.stream()
				.filter(e -> e.getKind().isClass() && registersElements(e.getAnnotation(RegisterForReflection.class)))
				.map(TypeElement.class::cast)
				.map(e -> {
					RegisterForReflection registerForReflection = e.getAnnotation(RegisterForReflection.class);
					Entry entry = new Entry(e.getQualifiedName().toString());
					entry.setAllDeclaredMethods(registerForReflection.allDeclaredMethods());
					entry.setAllDeclaredConstructors(registerForReflection.allDeclaredConstructors());
					return entry;
				})
				.forEach(this.entries::add);
		}

		return true;
	}

}
