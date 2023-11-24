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

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Collection;
import java.util.Deque;
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
@API(status = INTERNAL, since = "2021.1.0") final class RelationshipImplBuilder
	extends AbstractModelBuilder<RelationshipModelBuilder> implements RelationshipModelBuilder {

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
	 * The possible start- and end-node connections
	 */
	private final Deque<Edge> edges = new ArrayDeque<>();

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
	}

	@Override
	public RelationshipModelBuilder setStartNode(NodeModelBuilder newStartNode) {

		return callOnlyWithoutJavaFilePresent(() -> {
			var lastRelation = getOrCreateLastDefinedRelationType();
			lastRelation.start = extractClassName(newStartNode);
			return this;
		});
	}

	@Override
	public RelationshipModelBuilder setEndNode(NodeModelBuilder newEndNode) {

		return callOnlyWithoutJavaFilePresent(() -> {
			var lastRelation = getOrCreateLastDefinedRelationType();
			lastRelation.end = extractClassName(newEndNode);
			return this;
		});
	}

	private Edge getOrCreateLastDefinedRelationType() {
		var lastRelation = this.edges.peek();
		if (lastRelation == null) {
			lastRelation = new Edge(S, E);
			this.edges.push(lastRelation);
		}
		return lastRelation;
	}

	private MethodSpec buildConstructor(Edge relation, Edge expectedTypes) {
		ParameterSpec startNodeParam = ParameterSpec.builder(relation.start, "start").build();
		ParameterSpec endNodeParam = ParameterSpec.builder(relation.end, "end").build();
		MethodSpec.Builder builder = MethodSpec.constructorBuilder()
			.addModifiers(Modifier.PUBLIC)
			.addParameter(startNodeParam)
			.addParameter(endNodeParam);
		if (relation.start != expectedTypes.start) {
			if (relation.end != expectedTypes.end) {
				builder.addStatement("super(($T) $N, $N, ($T) $N)", expectedTypes.start, startNodeParam,
					relationshipTypeField, expectedTypes.end, endNodeParam);
			} else {
				builder.addStatement("super(($T) $N, $N, $N)", expectedTypes.start, startNodeParam,
					relationshipTypeField, endNodeParam);
			}
		} else if (relation.end != expectedTypes.end) {
			builder.addStatement("super($N, $N, ($T) $N)", startNodeParam, relationshipTypeField, expectedTypes.end,
				endNodeParam);
		} else {
			builder.addStatement("super($N, $N, $N)", startNodeParam, relationshipTypeField, endNodeParam);
		}
		return builder.build();
	}

	@Override
	public RelationshipModelBuilder addRelationship(NodeModelBuilder newStartNode, NodeModelBuilder newEndNode) {
		return callOnlyWithoutJavaFilePresent(() -> {
			var startNode = newStartNode == null ? S : extractClassName(newStartNode);
			var endNode = newEndNode == null ? E : extractClassName(newEndNode);
			this.edges.add(new Edge(startNode, endNode));
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

	private MethodSpec buildNamedMethod(TypeName typeName) {

		ParameterSpec newSymbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "newSymbolicName").build();
		return MethodSpec.methodBuilder("named")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.returns(typeName)
			.addParameter(newSymbolicName)
			.addStatement("return new $T$L($N, getLeft(), getDetails().getProperties(), getRight())",
				super.className, typeName == super.className ? "" : "<>", newSymbolicName)
			.build();
	}

	private MethodSpec buildWithPropertiesMethod(TypeName typeName) {

		ParameterSpec newProperties = ParameterSpec.builder(TYPE_NAME_MAP_EXPRESSION, "newProperties")
			.build();
		return MethodSpec.methodBuilder("withProperties")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.returns(typeName)
			.addParameter(newProperties)
			.addStatement(
				"return new $T$L(getSymbolicName().orElse(null), getLeft(), Properties.create($N), getRight())",
				super.className, typeName == super.className ? "" : "<>", newProperties)
			.build();
	}

	private List<FieldSpec> buildFields() {
		return Stream.concat(Stream.of(relationshipTypeField), generateFieldSpecsFromProperties()).toList();
	}

	@Override
	protected JavaFile buildJavaFile() {

		var expectedTypes = pickDefault(edges);
		var builder = TypeSpec.classBuilder(super.className);
		var typeName = getTypeName(expectedTypes);
		if (typeName instanceof ParameterizedTypeName pt) {
			for (TypeName t : pt.typeArguments) {
				if (t instanceof TypeVariableName tn) {
					builder.addTypeVariable(tn);
				}
			}
		}

		addGenerated(builder)
			.superclass(ParameterizedTypeName.get(TYPE_NAME_RELATIONSHIP_BASE, expectedTypes.start, expectedTypes.end,
				typeName))
			.addModifiers(Modifier.PUBLIC, Modifier.FINAL)
			.addFields(buildFields());

		(edges.isEmpty() ? Stream.of(new Edge(S, E)) : edges.stream())
			.map(relation -> buildConstructor(relation, expectedTypes)).forEach(builder::addMethod);

		var newType = builder
			.addMethod(buildCopyConstructor())
			.addMethod(buildNamedMethod(typeName))
			.addMethod(buildWithPropertiesMethod(typeName))
			.build();

		return prepareFileBuilder(newType).build();
	}

	private TypeName getTypeName(Edge expectedTypes) {
		return getTypeName(expectedTypes, null, null);
	}

	TypeName getTypeName(TypeName concreteStartType, TypeName concreteEndType) {
		return getTypeName(pickDefault(this.edges), concreteStartType, concreteEndType);
	}

	private TypeName getTypeName(Edge expectedEdge, TypeName concreteStartType, TypeName concreteEndType) {

		var concreteOrDefaultStart = concreteStartType == null ? S : concreteStartType;
		var concreteOrDefaultEnd = concreteEndType == null ? E : concreteEndType;
		if (expectedEdge.start == S && expectedEdge.end == E) {
			return ParameterizedTypeName.get(super.className, concreteOrDefaultStart, concreteOrDefaultEnd);
		} else if (expectedEdge.start == S) {
			return ParameterizedTypeName.get(super.className, concreteOrDefaultStart);
		} else if (expectedEdge.end == E) {
			return ParameterizedTypeName.get(super.className, concreteOrDefaultEnd);
		} else {
			return super.className;
		}
	}

	private static class Edge {
		private Edge(TypeName start, TypeName end) {
			this.start = start;
			this.end = end;
		}

		TypeName start;
		TypeName end;
	}

	static Edge pickDefault(Collection<Edge> relations) {
		var startNodes = relations.stream().map(edge -> edge.start)
			.collect(Collectors.toSet());
		var endNodes = relations.stream().map(edge -> edge.end)
			.collect(Collectors.toSet());

		var expectedStartNode = startNodes.size() == 1 ? startNodes.iterator().next() : S;
		var expectedEndNode = endNodes.size() == 1 ? endNodes.iterator().next() : E;
		return new Edge(expectedStartNode, expectedEndNode);
	}
}
