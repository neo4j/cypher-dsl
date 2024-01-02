/*
 * Copyright (c) 2019-2024 "Neo4j,"
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

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.lang.model.element.Modifier;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Properties;

import com.squareup.javapoet.ClassName;
import com.squareup.javapoet.CodeBlock;
import com.squareup.javapoet.FieldSpec;
import com.squareup.javapoet.JavaFile;
import com.squareup.javapoet.MethodSpec;
import com.squareup.javapoet.ParameterSpec;
import com.squareup.javapoet.ParameterizedTypeName;
import com.squareup.javapoet.TypeName;
import com.squareup.javapoet.TypeSpec;

/**
 * This is a builder. It builds classes extending {@link NodeBase}. The workflow is as follows: Create an instance via
 * {@link #create(Configuration, String, String)}, defining the target package (fully qualified name) as well as the type.
 * Unless {@link #writeTo(java.nio.file.Path)} or {@link #writeToString()} is called, additional labels and properties can be added
 * with {@link #addLabels(Collection)} and {@link #addProperty(String)}.
 * A call to any of the {@code writeToXXX} methods will trigger the generation of source code and after that, this instance
 * becomes effectively immutable.
 *
 * @author Michael J. Simons
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class NodeImplBuilder extends AbstractModelBuilder<NodeModelBuilder> implements NodeModelBuilder {

	private static final ClassName TYPE_NAME_NODE_LABEL = ClassName.get(NodeLabel.class);

	private final Set<String> labels = new LinkedHashSet<>();

	private final Set<RelationshipPropertyDefinition> relationshipDefinitions = new LinkedHashSet<>();
	private final Set<RelationshipFactoryDefinition> relationshipMethodDefinitions = new LinkedHashSet<>();

	static NodeModelBuilder create(Configuration configuration, String packageName, String suggestedTypeName) {

		String className = configuration.getNodeNameGenerator().generate(suggestedTypeName);
		String usedPackageName = packageName == null ? configuration.getDefaultPackage() : packageName;
		NodeImplBuilder builder = new NodeImplBuilder(
			configuration,
			ClassName.get(usedPackageName, configuration.getTypeNameDecorator().apply(className)),
			className
		);
		return builder.apply(configuration);
	}

	private NodeImplBuilder(Configuration configuration, ClassName className, String fieldName) {
		super(configuration.getConstantFieldNameGenerator(), className, fieldName, configuration.getTarget(),
			configuration.getIndent());
	}

	@Override
	public NodeModelBuilder addLabel(String newLabel) {

		return addLabels(newLabel == null ? Collections.emptyList() : Collections.singleton(newLabel));
	}

	@Override
	public NodeModelBuilder addLabels(Collection<String> newLabels) {

		return callOnlyWithoutJavaFilePresent(() -> {
			if (newLabels != null) {
				this.labels.addAll(newLabels);
			}
			return this;
		});
	}

	@Override
	public NodeModelBuilder addRelationshipDefinition(RelationshipPropertyDefinition definition) {

		return callOnlyWithoutJavaFilePresent(() -> {
			if (definition != null) {
				this.relationshipDefinitions.add(definition);
			}
			return this;
		});
	}

	@Override
	public NodeModelBuilder addRelationshipFactory(RelationshipFactoryDefinition definition) {
		return callOnlyWithoutJavaFilePresent(() -> {
			if (definition != null) {
				this.relationshipMethodDefinitions.add(definition);
			}
			return this;
		});
	}

	private MethodSpec buildDefaultConstructor() {

		CodeBlock.Builder superCallBuilder = CodeBlock.builder().add("super(");
		superCallBuilder.add(CodeBlock.join(this.labels.stream().map(l -> CodeBlock.of("$S", l)).toList(), ", "));
		superCallBuilder.add(")");

		return MethodSpec.constructorBuilder()
			.addModifiers(Modifier.PUBLIC)
			.addStatement(superCallBuilder.build())
			.build();
	}

	private MethodSpec buildCopyConstructor() {

		ParameterSpec symbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "symbolicName").build();
		ParameterSpec labelsParameter = ParameterSpec
			.builder(ParameterizedTypeName.get(TYPE_NAME_LIST, TYPE_NAME_NODE_LABEL), "labels").build();
		ParameterSpec properties = ParameterSpec.builder(ClassName.get(Properties.class), "properties").build();
		return MethodSpec.constructorBuilder()
			.addModifiers(Modifier.PRIVATE)
			.addParameter(symbolicName)
			.addParameter(labelsParameter)
			.addParameter(properties)
			.addStatement("super($N, $N, $N)", symbolicName, labelsParameter, properties)
			.build();

	}

	private MethodSpec buildNamedMethod() {

		ParameterSpec newSymbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "newSymbolicName").build();
		return MethodSpec.methodBuilder("named")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.returns(className)
			.addParameter(newSymbolicName)
			.addStatement("return new $T($N, getLabels(), getProperties())", className, newSymbolicName)
			.build();
	}

	private MethodSpec buildWithPropertiesMethod() {

		ParameterSpec newProperties = ParameterSpec.builder(TYPE_NAME_MAP_EXPRESSION, "newProperties")
			.build();
		return MethodSpec.methodBuilder("withProperties")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.returns(className)
			.addParameter(newProperties)
			.addStatement("return new $T(getSymbolicName().orElse(null), getLabels(), Properties.create($N))",
				className, newProperties)
			.build();
	}

	private static boolean isSelfReferential(RelationshipPropertyDefinition rpd) {
		return rpd.getStart() == rpd.getEnd();
	}

	private static boolean isNotSelfReferential(RelationshipPropertyDefinition rpd) {
		return !isSelfReferential(rpd);
	}

	private List<FieldSpec> buildFields() {

		FieldSpec defaultInstance = FieldSpec
			.builder(className, getFieldName(), Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
			.initializer("new $T()", className)
			.build();

		Stream<FieldSpec> properties = generateFieldSpecsFromProperties();

		Stream<FieldSpec> relationships = relationshipDefinitions.stream().filter(NodeImplBuilder::isNotSelfReferential)
			.map(p -> {

				String fieldName = fieldNameGenerator.generate(
					p.getNameInDomain() == null ? p.getType() : p.getNameInDomain());
				ClassName startClass = extractClassName(p.getStart());
				ClassName endClass = extractClassName(p.getEnd());

				var relationshipClassName = getRelationTypeWithParameters(p.getRelationshipBuilder(), startClass,
					endClass);
				FieldSpec.Builder builder = FieldSpec.builder(relationshipClassName, fieldName, Modifier.PUBLIC,
					Modifier.FINAL);
				if (this == p.getStart()) {
					builder.initializer("new $T(this, $T.$N)", relationshipClassName, endClass,
						p.getEnd().getFieldName());
				} else {
					builder.initializer("new $T($T.$N, this)", relationshipClassName, startClass,
						p.getStart().getFieldName());
				}
				return builder.build();
			});

		return Stream.concat(Stream.of(defaultInstance), Stream.concat(properties, relationships)).toList();
	}

	static String capitalize(String str) {

		if (str == null || str.isBlank()) {
			return str;
		} else {
			char baseChar = str.charAt(0);
			char updatedChar = Character.toUpperCase(baseChar);
			if (baseChar == updatedChar) {
				return str;
			} else {
				char[] chars = str.toCharArray();
				chars[0] = updatedChar;
				return new String(chars);
			}
		}
	}

	@Override
	protected JavaFile buildJavaFile() {

		if (this.labels.isEmpty()) {
			throw new IllegalStateException("Cannot build NodeImpl without labels!");
		}

		TypeSpec.Builder builder = addGenerated(TypeSpec.classBuilder(className))
			.superclass(ParameterizedTypeName.get(TYPE_NAME_NODE_BASE, className))
			.addModifiers(Modifier.PUBLIC, Modifier.FINAL)
			.addFields(buildFields())
			.addMethod(buildDefaultConstructor())
			.addMethod(buildCopyConstructor())
			.addMethod(buildNamedMethod())
			.addMethod(buildWithPropertiesMethod());

		relationshipDefinitions.stream().filter(NodeImplBuilder::isSelfReferential).map(buildSelfReferentialAccessor())
			.forEach(builder::addMethod);

		relationshipMethodDefinitions.stream().map(buildRelationshipFactoryMethod())
			.forEach(builder::addMethod);

		TypeSpec newType = builder
			.build();

		return prepareFileBuilder(newType).build();
	}

	private TypeName getRelationTypeWithParameters(RelationshipModelBuilder relationshipBuilder, TypeName startClass,
		ClassName endClass) {
		TypeName relationshipClassName;
		if (relationshipBuilder instanceof RelationshipImplBuilder impl) {
			relationshipClassName = impl.getTypeName(startClass, endClass);
		} else {
			relationshipClassName = extractClassName(relationshipBuilder);
		}
		return relationshipClassName;
	}

	private Function<RelationshipPropertyDefinition, MethodSpec> buildSelfReferentialAccessor() {
		return p -> {
			ClassName relationshipClassName = extractClassName(p.getRelationshipBuilder());
			ParameterSpec end = ParameterSpec.builder(className, p.getNameInDomain()).build();
			return MethodSpec.methodBuilder("with" + capitalize(p.getNameInDomain()))
				.addModifiers(Modifier.PUBLIC)
				.returns(relationshipClassName)
				.addParameter(end)
				.addStatement("return new $T(this, $N)", relationshipClassName, end)
				.build();
		};
	}

	private Function<RelationshipFactoryDefinition, MethodSpec> buildRelationshipFactoryMethod() {
		return p -> {
			ClassName startClass = extractClassName(p.getStart());
			ClassName endClass = extractClassName(p.getEnd());

			TypeName relationshipClassName = getRelationTypeWithParameters(p.getRelationshipBuilder(), startClass,
				endClass);
			var b = MethodSpec.methodBuilder(p.getNameInDomain())
				.addModifiers(Modifier.PUBLIC)
				.returns(relationshipClassName);
			if (this == p.getStart()) {
				ParameterSpec param = ParameterSpec.builder(endClass, "end").build();
				b.addParameter(param)
					.addStatement("return new $T(this, $N)", relationshipClassName, param);
			} else {
				ParameterSpec param = ParameterSpec.builder(startClass, "start").build();
				b.addParameter(param)
					.addStatement("return new $T($N, this)", relationshipClassName, param);
			}
			return b.build();
		};
	}
}
