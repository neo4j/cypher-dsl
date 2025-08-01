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
package org.neo4j.cypherdsl.codegen.core;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;

import javax.lang.model.element.Modifier;

import org.neo4j.cypherdsl.core.NodeBase;
import org.neo4j.cypherdsl.core.NodeLabel;
import org.neo4j.cypherdsl.core.Properties;

import com.squareup.javapoet.AnnotationSpec;
import com.squareup.javapoet.ClassName;
import com.squareup.javapoet.CodeBlock;
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
 * This is a builder. It builds classes extending {@link NodeBase}. The workflow is as follows: Create an instance via
 * {@link #create(Configuration, String, String)}, defining the target package (fully qualified name) as well as the type.
 * Unless {@link #writeTo(java.nio.file.Path)} or {@link #writeToString()} is called, additional labels and properties can be added
 * with {@link #addLabels(Collection)} and {@link #addProperty(String)}.
 * A call to any of the {@code writeToXXX} methods will trigger the generation of source code and after that, this instance
 * becomes effectively immutable.
 *
 * @author Michael J. Simons
 * @author Andreas Berger
 * @since 2021.1.0
 */
final class NodeImplBuilder extends AbstractModelBuilder<NodeModelBuilder>
	implements NodeModelBuilder {

	private static final ClassName TYPE_NAME_NODE_LABEL = ClassName.get(NodeLabel.class);

	private final Set<String> labels = new LinkedHashSet<>();

	private final Set<RelationshipPropertyDefinition> relationshipDefinitions = new LinkedHashSet<>();
	private final Set<RelationshipFactoryDefinition> relationshipMethodDefinitions = new LinkedHashSet<>();

	private NodeModelBuilder baseModel;
	private TypeVariableName self;
	private boolean extensible;

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
		this.self = TypeVariableName.get("SELF", className);
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

	@Override
	public NodeModelBuilder setBaseNodeModel(NodeModelBuilder baseNodeModel) {
		return callOnlyWithoutJavaFilePresent(() -> {
			this.baseModel = baseNodeModel;
			return this;
		});
	}

	@Override
	public NodeModelBuilder setExtensible(boolean isExtensible) {
		return callOnlyWithoutJavaFilePresent(() -> {
			synchronized (NodeImplBuilder.this) {
				this.extensible = isExtensible;
				if (this.extensible) {
					this.self = TypeVariableName.get("SELF",
						ParameterizedTypeName.get(className, WildcardTypeName.subtypeOf(Object.class)));
				} else {
					this.self = TypeVariableName.get("SELF", className);
				}
				return this;
			}
		});
	}

	@Override
	public boolean isExtensible() {
		return this.extensible;
	}

	private MethodSpec buildDefaultConstructor(FieldSpec primaryLabelField) {

		var superCallBuilder = CodeBlock.builder().add("super(");
		superCallBuilder.add(CodeBlock.of("$N", primaryLabelField));
		if (this.labels.size() > 1) {
			superCallBuilder.add(", ");
			superCallBuilder.add(
				CodeBlock.join(this.labels.stream().skip(1).map(l -> CodeBlock.of("$S", l)).toList(), ", "));
		}
		superCallBuilder.add(")");

		return MethodSpec.constructorBuilder()
			.addModifiers(Modifier.PUBLIC)
			.addStatement(superCallBuilder.build())
			.build();
	}

	private MethodSpec buildConstructorForInheritance(FieldSpec primaryLabelField) {

		var primaryLabelParam = ParameterSpec.builder(String.class, "primaryLabel").build();
		var additionalLabels = ParameterSpec.builder(String[].class, "additionalLabels").build();

		CodeBlock.Builder superCallBuilder = CodeBlock.builder().add("super(");
		superCallBuilder.add(CodeBlock.of("$N", primaryLabelParam));
		superCallBuilder.add(", ");
		superCallBuilder.add(CodeBlock.of("$T.concat($T.stream($N), $T.of($N",
			Stream.class,
			Arrays.class,
			additionalLabels,
			Stream.class,
			primaryLabelField
		));
		if (this.labels.size() > 1) {
			superCallBuilder.add(", ");
			superCallBuilder.add(
				CodeBlock.join(this.labels.stream().skip(1).map(l -> CodeBlock.of("$S", l)).toList(), ", "));
		}
		superCallBuilder.add(")).toArray($T[]::new))", String.class);

		return MethodSpec.constructorBuilder()
			.addModifiers(Modifier.PROTECTED)
			.addParameter(primaryLabelParam)
			.addParameter(additionalLabels)
			.varargs()
			.addStatement(superCallBuilder.build())
			.build();
	}

	private MethodSpec buildCreateMethod() {
		ParameterSpec symbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "symbolicName").build();
		ParameterSpec labelsParameter = ParameterSpec
			.builder(ParameterizedTypeName.get(TYPE_NAME_LIST, TYPE_NAME_NODE_LABEL), "labels").build();
		ParameterSpec properties = ParameterSpec.builder(ClassName.get(Properties.class), "properties").build();

		MethodSpec.Builder builder = MethodSpec.methodBuilder("create")
			.addModifiers(Modifier.PROTECTED)
			.addParameter(symbolicName)
			.addParameter(labelsParameter)
			.addParameter(properties);
		if (this.baseModel != null) {
			builder.addAnnotation(Override.class);
		}
		if (extensible) {
			builder.addAnnotation(
				AnnotationSpec.builder(SuppressWarnings.class).addMember("value", "\"unchecked\"").build());
			builder.addStatement("return ($T) new $T<>($N, $N, $N)", self, className, symbolicName, labelsParameter,
					properties)
				.returns(self);
		} else {
			builder.addStatement("return new $T($N, $N, $N)", className, symbolicName, labelsParameter, properties)
				.returns(className);
		}
		return builder.build();
	}

	private MethodSpec buildCopyConstructor() {

		ParameterSpec symbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "symbolicName").build();
		ParameterSpec labelsParameter = ParameterSpec
			.builder(ParameterizedTypeName.get(TYPE_NAME_LIST, TYPE_NAME_NODE_LABEL), "labels").build();
		ParameterSpec properties = ParameterSpec.builder(ClassName.get(Properties.class), "properties").build();
		return MethodSpec.constructorBuilder()
			.addModifiers(extensible ? Modifier.PROTECTED : Modifier.PRIVATE)
			.addParameter(symbolicName)
			.addParameter(labelsParameter)
			.addParameter(properties)
			.addStatement("super($N, $N, $N)", symbolicName, labelsParameter, properties)
			.build();

	}

	private MethodSpec buildNamedMethod() {

		ParameterSpec newSymbolicName = ParameterSpec.builder(TYPE_NAME_SYMBOLIC_NAME, "newSymbolicName").build();
		MethodSpec.Builder builder = MethodSpec.methodBuilder("named")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.addParameter(newSymbolicName);
		if (extensible) {
			builder
				.returns(self)
				.addStatement("return create($N, getLabels(), getProperties())", newSymbolicName);
		} else {
			builder.returns(className)
				.addStatement("return new $T($N, getLabels(), getProperties())", className, newSymbolicName);
		}
		return builder.build();
	}

	private MethodSpec buildWithPropertiesMethod() {

		ParameterSpec newProperties = ParameterSpec.builder(TYPE_NAME_MAP_EXPRESSION, "newProperties")
			.build();
		MethodSpec.Builder builder = MethodSpec.methodBuilder("withProperties")
			.addAnnotation(Override.class)
			.addModifiers(Modifier.PUBLIC)
			.addParameter(newProperties);
		if (extensible) {
			builder
				.returns(self)
				.addStatement("return create(getSymbolicName().orElse(null), getLabels(), Properties.create($N))",
					newProperties);
		} else {
			builder.returns(className)
				.addStatement("return new $T(getSymbolicName().orElse(null), getLabels(), Properties.create($N))",
					className, newProperties);
		}
		return builder.build();
	}

	private static boolean isSelfReferential(RelationshipPropertyDefinition rpd) {
		return rpd.getStart() == rpd.getEnd();
	}

	private static boolean isNotSelfReferential(RelationshipPropertyDefinition rpd) {
		return !isSelfReferential(rpd);
	}

	private List<FieldSpec> buildFields(FieldSpec primaryLabelField) {

		FieldSpec defaultInstance;
		if (extensible) {
			var type = ParameterizedTypeName.get(className,
				ParameterizedTypeName.get(className, WildcardTypeName.subtypeOf(Object.class)));
			defaultInstance = FieldSpec
				.builder(type, getFieldName(), Modifier.PUBLIC,
					Modifier.STATIC, Modifier.FINAL)
				.initializer("new $T<>()", className)
				.build();

		} else {
			defaultInstance = FieldSpec
				.builder(className, getFieldName(), Modifier.PUBLIC, Modifier.STATIC, Modifier.FINAL)
				.initializer("new $T()", className)
				.build();

		}

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

		return Stream.concat(Stream.of(primaryLabelField, defaultInstance), Stream.concat(properties, relationships))
			.toList();
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
		var baseNode = baseModel == null ? TYPE_NAME_NODE_BASE : extractClassName(baseModel);
		if (baseModel != null && (!(baseModel instanceof NodeImplBuilder nmb) || !nmb.extensible)) {
			throw new IllegalStateException("Cannot extend non-extensible node " + baseModel.getCanonicalClassName());
		}

		var primaryLabelField = FieldSpec
			.builder(String.class, this.fieldNameGenerator.generate("$PRIMARY_LABEL"), Modifier.PUBLIC,
				Modifier.FINAL, Modifier.STATIC)
			.initializer("$S", labels.stream().findFirst().orElseThrow())
			.build();

		TypeSpec.Builder builder = addGenerated(TypeSpec.classBuilder(className));
		builder.addModifiers(Modifier.PUBLIC);
		if (extensible) {
			builder
				.addTypeVariable(self)
				.superclass(ParameterizedTypeName.get(baseNode, self));
		} else {
			builder
				.addModifiers(Modifier.FINAL)
				.superclass(ParameterizedTypeName.get(baseNode, className));
		}
		builder
			.addFields(buildFields(primaryLabelField))
			.addMethod(buildDefaultConstructor(primaryLabelField));
		if (extensible) {
			builder.addMethod(buildConstructorForInheritance(primaryLabelField));
		}
		if (extensible || baseNode != TYPE_NAME_NODE_BASE) {
			builder.addMethod(buildCreateMethod());
		}
		builder.addMethod(buildCopyConstructor());
		if (baseNode == TYPE_NAME_NODE_BASE) {
			builder.addMethod(buildNamedMethod())
				.addMethod(buildWithPropertiesMethod());
		}

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
