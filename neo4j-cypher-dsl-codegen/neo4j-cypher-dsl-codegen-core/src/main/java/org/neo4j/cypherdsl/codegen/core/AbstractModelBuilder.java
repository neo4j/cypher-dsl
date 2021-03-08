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
package org.neo4j.cypherdsl.codegen.core;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.time.Clock;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.stream.Stream;

import javax.lang.model.element.Modifier;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.codegen.core.Configuration.JavaVersion;
import org.neo4j.cypherdsl.core.MapExpression;
import org.neo4j.cypherdsl.core.Node;
import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.Property;
import org.neo4j.cypherdsl.core.RelationshipBase;
import org.neo4j.cypherdsl.core.SymbolicName;

import com.squareup.javapoet.AnnotationSpec;
import com.squareup.javapoet.ClassName;
import com.squareup.javapoet.CodeBlock;
import com.squareup.javapoet.FieldSpec;
import com.squareup.javapoet.JavaFile;
import com.squareup.javapoet.TypeSpec;

/**
 * Base class with some shared state and information for builders of {@link NodeBase} and
 * {@link RelationshipBase}.
 *
 * @param <T> Concrete type of this builder
 * @author Michael J. Simons
 * @soundtrack Bear McCreary - Battlestar Galactica Season 2
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
abstract class AbstractModelBuilder<T extends ModelBuilder> implements ModelBuilder<T> {

	protected static final ClassName TYPE_NAME_NODE = ClassName.get(Node.class);
	protected static final ClassName TYPE_NAME_NODE_BASE = ClassName.get(NodeBase.class);
	protected static final ClassName TYPE_NAME_SYMBOLIC_NAME = ClassName.get(SymbolicName.class);
	protected static final ClassName TYPE_NAME_LIST = ClassName.get(List.class);
	protected static final ClassName TYPE_NAME_STRING = ClassName.get(String.class);
	protected static final ClassName TYPE_NAME_MAP_EXPRESSION = ClassName.get(MapExpression.class);

	private volatile JavaFile javaFile;

	/**
	 * Generator for field names.
	 */
	protected final FieldNameGenerator fieldNameGenerator;

	/**
	 * The fully qualified name of the generated class.
	 */
	protected final ClassName className;

	/**
	 * Simple, undecorated class name.
	 */
	protected final String plainClassName;

	/**
	 * A set of fields to generate.
	 */
	protected final Set<PropertyDefinition> properties = new LinkedHashSet<>();

	private final JavaVersion target;

	private final String indent;

	private Clock clock = Clock.systemDefaultZone();

	private boolean addAtGenerated = true;

	AbstractModelBuilder(FieldNameGenerator fieldNameGenerator, ClassName className, String plainClassName, JavaVersion target, String indent) {

		this.fieldNameGenerator = fieldNameGenerator;
		this.className = className;
		this.plainClassName = plainClassName;
		this.target = target;
		this.indent = indent;
	}

	/**
	 * Central method to trigger building of the {@link #javaFile} if that has not been done yet. {@link #buildJavaFile()} is
	 * supposed to fail when it realizes the build has already been done.
	 *
	 * @return
	 */
	protected abstract JavaFile buildJavaFile();

	@Override
	public final T addProperty(String newProperty) {

		return addProperty(PropertyDefinition.create(newProperty, newProperty));
	}

	@Override
	public final T addProperty(PropertyDefinition newProperty) {

		return addProperties(Collections.singleton(newProperty));
	}

	@Override
	@SuppressWarnings("unchecked")
	public final T addProperties(Collection<PropertyDefinition> newProperties) {

		return (T) callOnlyWithoutJavaFilePresent(() -> {
			this.properties.addAll(newProperties);
			return this;
		});
	}

	@Override
	public final String getPackageName() {
		return className.packageName();
	}

	@Override
	public final String getCanonicalClassName() {
		return className.canonicalName();
	}

	@Override
	public final String getPlainClassName() {
		return plainClassName;
	}

	@Override
	public final void writeTo(Path path) {

		try {
			getJavaFile().writeTo(path);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	@Override
	public final void writeTo(Appendable appendable) {
		try {
			getJavaFile().writeTo(appendable);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	@Override
	public final String writeToString() {

		try {
			StringBuilder out = new StringBuilder();
			getJavaFile().writeTo(out);
			return out.toString();
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	@Override
	public final String getFieldName() {
		return fieldNameGenerator.generate(getPlainClassName());
	}

	@SuppressWarnings("unchecked")
	final T apply(Configuration configuration) {
		configuration.getClock().ifPresent(c -> this.clock = c);
		this.addAtGenerated = configuration.isAddAtGenerated();
		return (T) this;
	}

	/**
	 * Makes sure that no Java file has been created before modifying the builder
	 */
	final <V> V callOnlyWithoutJavaFilePresent(Callable<V> c) {

		synchronized (this) {
			if (javaFile == null) {
				try {
					return c.call();
				} catch (Exception e) {
					throw new RuntimeException(e);
				}
			}
		}
		throw new IllegalStateException("Class has already been generated, cannot add properties.");
	}

	/**
	 * Adds information about the generated class.
	 * @param builder The builder that should be decorated
	 * @return A type builder.
	 */
	final TypeSpec.Builder addGenerated(TypeSpec.Builder builder) {

		ClassName nameOfAtGenerated;
		if (target == JavaVersion.RELEASE_8) {
			nameOfAtGenerated = ClassName.get("javax.annotation", "Generated");
		} else if (target == JavaVersion.RELEASE_11 && addAtGenerated) {
			nameOfAtGenerated = ClassName.get("javax.annotation.processing", "Generated");
		} else {
			nameOfAtGenerated = null;
		}

		String comment = "This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.";
		if (nameOfAtGenerated == null) {
			builder.addJavadoc(CodeBlock.of(comment));
		} else {
			AnnotationSpec spec = AnnotationSpec.builder(nameOfAtGenerated)
				.addMember("value", "$S", getClass().getName())
				.addMember("date", "$S", ZonedDateTime.now(clock).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
				.addMember("comments", "$S", comment)
				.build();
			builder.addAnnotation(spec);
		}
		return builder;
	}

	/**
	 * Turns the property definitions into field spec
	 *
	 * @return a stream of field specs
	 */
	final Stream<FieldSpec> generateFieldSpecsFromProperties() {
		return properties.stream().map(p -> {

				String fieldName;
				CodeBlock initializer;
				if (p.getNameInDomain() == null) {
					fieldName = p.getNameInGraph();
					initializer = CodeBlock.of("this.property($S)", p.getNameInGraph());
				} else {
					fieldName = p.getNameInDomain();
					initializer = CodeBlock.of("this.property($S).referencedAs($S)", p.getNameInGraph(), p.getNameInDomain());
				}

				return FieldSpec
					.builder(Property.class, fieldNameGenerator.generate(fieldName), Modifier.PUBLIC, Modifier.FINAL)
					.initializer(initializer)
					.build();
			}
		);
	}

	/**
	 * Prepares a file builder for the given type.
	 *
	 * @param typeSpec The type that should be written
	 * @return A builder with some shared settings like indent etc. applied.
	 */
	final JavaFile.Builder prepareFileBuilder(TypeSpec typeSpec) {
		return JavaFile.builder(getPackageName(), typeSpec)
			.skipJavaLangImports(true)
			.indent(indent);
	}

	/**
	 * @return The internal class name used by JavaPoet
	 */
	final ClassName getClassName() {
		return className;
	}

	/**
	 * Internal utility method to extract the actual class name while trying to avoid best guesses.
	 * @param optionalSource The source from which to extract the class name to be generated
	 * @return A class name
	 */
	static final ClassName extractClassName(ModelBuilder<?> optionalSource) {

		if (optionalSource == null) {
			return null;
		} else if (optionalSource instanceof NodeImplBuilder) {
			return ((NodeImplBuilder) optionalSource).getClassName();
		} else if (optionalSource instanceof RelationshipImplBuilder) {
			return ((RelationshipImplBuilder) optionalSource).getClassName();
		} else {
			return ClassName.bestGuess(optionalSource.getCanonicalClassName());
		}
	}

	private JavaFile getJavaFile() {

		JavaFile result = this.javaFile;
		if (result == null) {
			synchronized (this) {
				result = this.javaFile;
				if (result == null) {
					this.javaFile = buildJavaFile();
					result = this.javaFile;
				}
			}
		}
		return result;
	}
}
