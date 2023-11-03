/*
 * Copyright (c) 2019-2023 "Neo4j,"
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.lang.model.element.Modifier;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.Properties;
import org.neo4j.cypherdsl.core.RelationshipBase;

import com.squareup.javapoet.ClassName;
import com.squareup.javapoet.FieldSpec;
import com.squareup.javapoet.JavaFile;
import com.squareup.javapoet.MethodSpec;
import com.squareup.javapoet.ParameterSpec;
import com.squareup.javapoet.ParameterizedTypeName;
import com.squareup.javapoet.TypeName;
import com.squareup.javapoet.TypeSpec;
import com.squareup.javapoet.TypeVariableName;
import com.squareup.javapoet.WildcardTypeName;

/**
 * This is a builder. It builds classes extending {@link RelationshipImplBuilder}.
 *
 * @author Michael J. Simons
 * @soundtrack Bear McCreary - Battlestar Galactica Season 2
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class RelationshipImplBuilder extends AbstractModelBuilder<RelationshipModelBuilder> implements RelationshipModelBuilder {

	private static final ClassName TYPE_NAME_RELATIONSHIP_BASE = ClassName.get(RelationshipBase.class);
	private static final TypeVariableName S = TypeVariableName.get("S",
		ParameterizedTypeName.get(TYPE_NAME_NODE_BASE, WildcardTypeName.subtypeOf(Object.class)));
	private static final TypeVariableName E = TypeVariableName.get("E",
		ParameterizedTypeName.get(TYPE_NAME_NODE_BASE, WildcardTypeName.subtypeOf(Object.class)));

	static RelationshipModelBuilder create(Configuration configuration, String packageName, String relationshipType,
		String alternateClassNameSuggestion) {

		String className = configuration.getRelationshipNameGenerator()
			.generate(alternateClassNameSuggestion != null ? alternateClassNameSuggestion : relationshipType);
		String usedPackageName = packageName == null ? configuration.getDefaultPackage() : packageName;
		RelationshipImplBuilder builder = new RelationshipImplBuilder(
			configuration,
			relationshipType,
			ClassName.get(usedPackageName, configuration.getTypeNameDecorator().apply(className)),
			className
		);
		return builder.apply(configuration);
	}

	/**
	 * The required relationship type.
	 */
	private final FieldSpec relationshipTypeField;

	/**
	 * This will be the final type name used as return type of various methods. It is not necessary the same as the initial
	 * {@code ClassName} passed to this builder. The actual type is decided during the build process, depending on the
	 * presence of the start and end node types.
	 */
	private TypeName parameterizedTypeName;

	private final RelationTypes legacyRelation = new RelationTypes(S, E);
	/**
	 * The possible start- and end-node connections
	 */
	private final List<RelationTypes> relations = new ArrayList<>();

	private RelationshipImplBuilder(Configuration configuration, String relationshipType, ClassName className,
		String fieldName) {
		super(configuration.getConstantFieldNameGenerator(), className, fieldName, configuration.getTarget(),
			configuration.getIndent());

		if (relationshipType == null) {
			throw new IllegalStateException("Cannot build a RelationshipImpl without a single relationship type!");
		}

		this.relationshipTypeField = FieldSpec
			.builder(String.class, configuration.getConstantFieldNameGenerator().generate("$TYPE"), Modifier.PUBLIC,
				Modifier.FINAL, Modifier.STATIC)
			.initializer("$S", relationshipType)
			.build();
		this.parameterizedTypeName = className;
	}

	@Override
	@SuppressWarnings("removal")
	public RelationshipModelBuilder setStartNode(NodeModelBuilder newStartNode) {

		return callOnlyWithoutJavaFilePresent(() -> {
			this.legacyRelation.startNode = extractClassName(newStartNode);
			if (!relations.contains(legacyRelation)) {
				this.relations.add(legacyRelation);
			}
			return this;
		});
	}

	@Override
	@SuppressWarnings("removal")
	public RelationshipModelBuilder setEndNode(NodeModelBuilder newEndNode) {

		return callOnlyWithoutJavaFilePresent(() -> {
			legacyRelation.endNode = extractClassName(newEndNode);
			if (!relations.contains(legacyRelation)) {
				this.relations.add(legacyRelation);
			}
			return this;
		});
	}

	private MethodSpec buildConstructor(RelationTypes relation) {
		ParameterSpec startNodeParam = ParameterSpec.builder(relation.startNode, "start").build();
		ParameterSpec endNodeParam = ParameterSpec.builder(relation.endNode, "end").build();
		return MethodSpec.constructorBuilder()
			.addModifiers(Modifier.PUBLIC)
			.addParameter(startNodeParam)
			.addParameter(endNodeParam)
			.addStatement("super($N, $N, $N)", startNodeParam, relationshipTypeField, endNodeParam)
			.build();
	}

	@Override
	public RelationshipModelBuilder addRelationship(NodeModelBuilder newStartNode, NodeModelBuilder newEndNode) {
		return callOnlyWithoutJavaFilePresent(() -> {
			var startNode = newStartNode == null ? S : extractClassName(newStartNode);
			var endNode = newEndNode == null ? E : extractClassName(newEndNode);
			this.relations.add(new RelationTypes(startNode, endNode));
			return this;
		});
	}

	private MethodSpec buildCopyConstructor() {

		ParameterSpec symbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "symbolicName").build();
		ParameterSpec start = ParameterSpec.builder(TYPE_NAME_NODE, "start").build();
		ParameterSpec properties = ParameterSpec.builder(ClassName.get(Properties.class), "properties").build();
		ParameterSpec end = ParameterSpec.builder(TYPE_NAME_NODE, "end").build();
		return MethodSpec.constructorBuilder()
			.addModifiers(Modifier.PRIVATE)
			.addParameters(Arrays.asList(symbolicName, start, properties, end))
			.addStatement("super($N, $N, $N, $N, $N)", symbolicName, start, relationshipTypeField, properties, end)
			.build();

	}

	private MethodSpec buildNamedMethod() {

		ParameterSpec newSymbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "newSymbolicName").build();
		return MethodSpec.methodBuilder("named")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.returns(parameterizedTypeName)
			.addParameter(newSymbolicName)
			.addStatement("return new $T$L($N, getLeft(), getDetails().getProperties(), getRight())",
				super.className, parameterizedTypeName == super.className ? "" : "<>", newSymbolicName)
			.build();
	}

	private MethodSpec buildWithPropertiesMethod() {

		ParameterSpec newProperties = ParameterSpec.builder(TYPE_NAME_MAP_EXPRESSION, "newProperties")
			.build();
		return MethodSpec.methodBuilder("withProperties")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.returns(parameterizedTypeName)
			.addParameter(newProperties)
			.addStatement(
				"return new $T$L(getSymbolicName().orElse(null), getLeft(), Properties.create($N), getRight())",
				super.className, parameterizedTypeName == super.className ? "" : "<>", newProperties)
			.build();
	}

	private List<FieldSpec> buildFields() {
		return Stream.concat(Stream.of(relationshipTypeField), generateFieldSpecsFromProperties()).toList();
	}

	@Override
	protected JavaFile buildJavaFile() {
		if (relations.isEmpty()) {
			relations.add(legacyRelation);
		}

		var startNodes = relations.stream().map(relationTypes -> relationTypes.startNode).collect(Collectors.toSet());
		var endNodes = relations.stream().map(relationTypes -> relationTypes.endNode).collect(Collectors.toSet());
		var startNode = startNodes.size() == 1 && !startNodes.contains(S) ? startNodes.iterator().next() : S;
		var endNode = endNodes.size() == 1 && !startNodes.contains(E) ? endNodes.iterator().next() : E;

		TypeSpec.Builder builder = TypeSpec.classBuilder(super.className);
		if (startNode == S && endNode == E) {
			parameterizedTypeName = ParameterizedTypeName.get(super.className, S, E);
			builder.addTypeVariable(S);
			builder.addTypeVariable(E);
		} else if (startNode == S) {
			parameterizedTypeName = ParameterizedTypeName.get(super.className, S);
			builder.addTypeVariable(S);
		} else if (endNode == E) {
			parameterizedTypeName = ParameterizedTypeName.get(super.className, E);
			builder.addTypeVariable(E);
		} else {
			parameterizedTypeName = super.className;
		}
		addGenerated(builder)
			.superclass(ParameterizedTypeName.get(TYPE_NAME_RELATIONSHIP_BASE, startNode, endNode,
				parameterizedTypeName))
			.addModifiers(Modifier.PUBLIC, Modifier.FINAL)
			.addFields(buildFields());

		relations.stream().map(this::buildConstructor).forEach(builder::addMethod);

		TypeSpec newType = builder
			.addMethod(buildCopyConstructor())
			.addMethod(buildNamedMethod())
			.addMethod(buildWithPropertiesMethod())
			.build();

		return prepareFileBuilder(newType).build();
	}

	public TypeName getTypeName(TypeName startNode, TypeName endNode) {
		var startNodes = relations.stream().map(relationTypes -> relationTypes.startNode).collect(Collectors.toSet());
		var endNodes = relations.stream().map(relationTypes -> relationTypes.endNode).collect(Collectors.toSet());

		var expectedStartNode = startNodes.size() == 1 && !startNodes.contains(S) ? startNodes.iterator().next() : S;
		var expectedEndNode = endNodes.size() == 1 && !startNodes.contains(E) ? endNodes.iterator().next() : E;

		if (expectedStartNode == S && expectedEndNode == E) {
			return ParameterizedTypeName.get(super.className, startNode, endNode);
		} else if (expectedStartNode == S) {
			return ParameterizedTypeName.get(super.className, startNode);
		} else if (expectedEndNode == E) {
			return ParameterizedTypeName.get(super.className, endNode);
		} else {
			return super.className;
		}
	}

	private static class RelationTypes {
		private RelationTypes(TypeName startNode, TypeName endNode) {
			this.startNode = startNode;
			this.endNode = endNode;
		}

		TypeName startNode;
		TypeName endNode;
	}
}
