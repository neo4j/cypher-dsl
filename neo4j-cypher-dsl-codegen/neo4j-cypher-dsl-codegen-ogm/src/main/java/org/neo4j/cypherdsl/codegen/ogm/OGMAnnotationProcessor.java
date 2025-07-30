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

import java.io.IOException;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedOptions;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.RecordComponentElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.ElementKindVisitor8;
import javax.lang.model.util.Elements;
import javax.lang.model.util.SimpleTypeVisitor8;
import javax.lang.model.util.TypeKindVisitor8;
import javax.tools.Diagnostic;

import org.neo4j.cypherdsl.codegen.core.AbstractMappingAnnotationProcessor;
import org.neo4j.cypherdsl.codegen.core.Configuration;
import org.neo4j.cypherdsl.codegen.core.ModelBuilder;
import org.neo4j.cypherdsl.codegen.core.NodeModelBuilder;
import org.neo4j.cypherdsl.codegen.core.PropertyDefinition;
import org.neo4j.cypherdsl.codegen.core.RelationshipModelBuilder;
import org.neo4j.cypherdsl.codegen.core.RelationshipPropertyDefinition;
import org.neo4j.ogm.annotation.NodeEntity;
import org.neo4j.ogm.annotation.Property;
import org.neo4j.ogm.annotation.Relationship;

/**
 * @author Shinigami
 * @since TODO
 */
@SupportedAnnotationTypes({
	OGMAnnotationProcessor.NODE_ENTITY_ANNOTATION,
	OGMAnnotationProcessor.RELATIONSHIP_ENTITY_ANNOTATION
})
@SupportedOptions({
	Configuration.PROPERTY_PREFIX,
	Configuration.PROPERTY_SUFFIX,
	Configuration.PROPERTY_INDENT_STYLE,
	Configuration.PROPERTY_INDENT_SIZE,
	Configuration.PROPERTY_TIMESTAMP,
	Configuration.PROPERTY_ADD_AT_GENERATED
})
public final class OGMAnnotationProcessor extends AbstractMappingAnnotationProcessor {

	static final String NODE_ENTITY_ANNOTATION = "org.neo4j.ogm.annotation.NodeEntity";
	static final String RELATIONSHIP_ENTITY_ANNOTATION = "org.neo4j.ogm.annotation.RelationshipEntity";
	static final Set<String> VALID_GENERATED_ID_TYPES = Set.of(Long.class.getName(), long.class.getName());
	private final List<TypeElement> convertAnnotationTypes = new ArrayList<>();
	private TypeElement propertyAnnotationType;
	private TypeElement nodeEntityAnnotationType;
	private TypeElement relationshipEntityAnnotationType;
	private TypeElement relationshipAnnotationType;
	private TypeElement startNodeAnnotationType;
	private TypeElement endNodeAnnotationType;
	private TypeElement ogmIdAnnotationType;
	private TypeElement generatedValueAnnotationType;

	@Override
	public void initFrameworkSpecific(ProcessingEnvironment processingEnv) {

		Elements elementUtils = processingEnv.getElementUtils();
		this.nodeEntityAnnotationType = elementUtils.getTypeElement(NODE_ENTITY_ANNOTATION);
		this.relationshipEntityAnnotationType = elementUtils.getTypeElement(RELATIONSHIP_ENTITY_ANNOTATION);

		this.relationshipAnnotationType = elementUtils.getTypeElement("org.neo4j.ogm.annotation.Relationship");
		this.startNodeAnnotationType = elementUtils.getTypeElement("org.neo4j.ogm.annotation.StartNode");
		this.endNodeAnnotationType = elementUtils.getTypeElement("org.neo4j.ogm.annotation.EndNode");
		for (var name : List.of("Convert", "DateLong", "DateString", "EnumString", "NumberString")) {
			this.convertAnnotationTypes.add(
				elementUtils.getTypeElement("org.neo4j.ogm.annotation.typeconversion.%s".formatted(name)));
		}
		this.propertyAnnotationType = elementUtils.getTypeElement(
			"org.neo4j.ogm.annotation.Property");

		this.ogmIdAnnotationType = elementUtils.getTypeElement("org.neo4j.ogm.annotation.Id");
		this.generatedValueAnnotationType = elementUtils.getTypeElement("org.neo4j.ogm.annotation.GeneratedValue");
	}

	@Override
	protected Collection<String> getLabel(TypeElement annotatedClass) {
		NodeEntity nodeAnnotation = annotatedClass.getAnnotation(NodeEntity.class);
		Set<String> labels = new LinkedHashSet<>();
		Consumer<String> addLabel = label -> {
			if (!label.isEmpty()) {
				labels.add(label);
			}
		};
		addLabel.accept(nodeAnnotation.label());
		// TODO Just so that you know @Shinigami92, you always have to look at both the original and the aliases
		addLabel.accept(nodeAnnotation.value());

		if (labels.isEmpty()) {
			addLabel.accept(annotatedClass.getSimpleName().toString());
		}
		return Collections.unmodifiableCollection(labels);
	}

	@Override
	public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
		if (annotations.isEmpty()) {
			return false;
		}

		Map<TypeElement, NodeModelBuilder> nodeBuilders = populateListOfNodes(
			getTypesAnnotatedWith(nodeEntityAnnotationType, roundEnv));
		Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> relationshipProperties = collectRelationshipProperties(
			roundEnv);
		Map<NodeModelBuilder, List<Element>> relationshipFields = populateNodePropertiesAndCollectRelationshipFields(
			nodeBuilders);
		Map<String, List<RelationshipModelBuilder>> relationshipBuilders = populateListOfRelationships(
			computeRelationshipDefinitions(relationshipFields, relationshipProperties, nodeBuilders));

		List<ModelBuilder<?>> allBuilders = new ArrayList<>(nodeBuilders.values());
		relationshipBuilders.values().forEach(allBuilders::addAll);
		try {
			writeSourceFiles(allBuilders);
		} catch (IOException e) {
			messager.printMessage(Diagnostic.Kind.ERROR, "Could not write source files: " + e.getMessage());
		}

		return true;
	}

	/**
	 * Collects classes annotated with {@code @RelationshipEntity}
	 *
	 * @param roundEnvironment The current environment
	 * @return A map from the other end of the relationship (target node) to the
	 * properties of the relationship
	 */
	private Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> collectRelationshipProperties(
		RoundEnvironment roundEnvironment) {

		Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> result = new HashMap<>();
		Set<TypeElement> relationshipProperties = getTypesAnnotatedWith(relationshipEntityAnnotationType,
			roundEnvironment);
		relationshipProperties.forEach(e -> {
			List<PropertyDefinition> properties = new ArrayList<>();
			for (Element enclosedElement : e.getEnclosedElements()) {
				TypeElement actualTargetType = null;
				if (!enclosedElement.getKind().isField()) {
					continue;
				}

				Set<Element> declaredAnnotations = enclosedElement.getAnnotationMirrors().stream()
					.map(AnnotationMirror::getAnnotationType).map(DeclaredType::asElement).collect(Collectors.toSet());
				if (declaredAnnotations.contains(startNodeAnnotationType)) {
					Element element = typeUtils.asElement(enclosedElement.asType());
					actualTargetType = element.accept(new TypeElementVisitor<>(Function.identity()), null);
					if (actualTargetType == null) {
						messager.printMessage(Diagnostic.Kind.WARNING,
							"Cannot resolve generic type, not generating a property for relationships referring to "
							+ e.getQualifiedName(), element);
					}
				} else if (!declaredAnnotations.contains(endNodeAnnotationType)) {
					properties.add(asPropertyDefinition(enclosedElement));
				}

				if (actualTargetType != null) {
					result.put(e,
						new AbstractMap.SimpleEntry<>(actualTargetType, Collections.unmodifiableList(properties)));
				}
			}
		});
		return Collections.unmodifiableMap(result);
	}

	@Override
	protected PropertyDefinition asPropertyDefinition(Element e) {
		Optional<Property> optionalPropertyAnnotation = Optional.ofNullable(e.getAnnotation(Property.class));

		PropertyDefinition propertyDefinition;
		String fieldName = e.getSimpleName().toString();

		if (optionalPropertyAnnotation.isPresent()) {
			Property propertyAnnotation = optionalPropertyAnnotation.get();

			String nameValue = propertyAnnotation.name();
			String valueValue = propertyAnnotation.value();

			if (!nameValue.isEmpty() && !valueValue.isEmpty()) {
				if (!nameValue.equals(valueValue)) {
					messager.printMessage(Diagnostic.Kind.ERROR,
						"Different @AliasFor mirror values for annotation [org.neo4j.ogm.annotation.Property]!", e);
				}
				propertyDefinition = PropertyDefinition.create(nameValue, fieldName);
			} else if (!nameValue.isEmpty()) {
				propertyDefinition = PropertyDefinition.create(nameValue, fieldName);
			} else if (!valueValue.isEmpty()) {
				propertyDefinition = PropertyDefinition.create(valueValue, fieldName);
			} else {
				propertyDefinition = PropertyDefinition.create(fieldName, null);
			}
		} else {
			propertyDefinition = PropertyDefinition.create(fieldName, null);
		}

		return propertyDefinition;
	}

	@Override
	protected RelationshipPropertyDefinition asRelationshipDefinition(NodeModelBuilder owner, Element e,
		Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> relationshipProperties,
		Map<TypeElement, NodeModelBuilder> nodeBuilders) {
		Optional<Relationship> optionalRelationshipAnnotation = Optional.ofNullable(
			e.getAnnotation(Relationship.class));

		String fieldName = e.getSimpleName().toString();

		// Default Relationship#direction is outgoing.
		boolean isIncoming = false;

		String relationshipType;
		if (optionalRelationshipAnnotation.isPresent()) {
			Relationship relationshipAnnotation = optionalRelationshipAnnotation.get();

			String typeValue = relationshipAnnotation.type();
			String valueValue = relationshipAnnotation.value();

			isIncoming = relationshipAnnotation.direction() == Relationship.Direction.INCOMING;

			if (!typeValue.isEmpty() && !valueValue.isEmpty()) {
				if (!typeValue.equals(valueValue)) {
					messager.printMessage(Diagnostic.Kind.ERROR,
						"Different @AliasFor mirror values for annotation [org.neo4j.ogm.annotation.Relationship]!", e);
				}
				relationshipType = typeValue;
			} else if (!typeValue.isEmpty()) {
				relationshipType = typeValue;
			} else if (!valueValue.isEmpty()) {
				relationshipType = valueValue;
			} else {
				relationshipType = fieldName;
			}
		} else {
			relationshipType = fieldName;
		}

		DeclaredType declaredType = e.asType().accept(new SimpleTypeVisitor8<DeclaredType, Void>() {
			@Override public DeclaredType visitDeclared(DeclaredType t, Void unused) {
				return t;
			}
		}, null);

		if (declaredType == null) {
			return null;
		}

		TypeMirror relatedType = null;
		if (declaredType.getTypeArguments().size() == 1) {
			relatedType = declaredType.getTypeArguments().get(0);
		} else if (declaredType.getTypeArguments().isEmpty()) {
			relatedType = declaredType;
		}

		Element key = typeUtils.asElement(relatedType);
		NodeModelBuilder end = key == null ? null : nodeBuilders.get(key);
		List<PropertyDefinition> properties = null;
		String optionalPropertyHolder = null;

		if (key == null) {
			return null;
		} else if (end == null) {
			Map.Entry<TypeElement, List<PropertyDefinition>> typeAndProperties = relationshipProperties.get(key);
			if (typeAndProperties != null) {
				optionalPropertyHolder = key.toString();
				end = nodeBuilders.get(typeAndProperties.getKey());
				properties = typeAndProperties.getValue();
			}
		}

		if (end == null) {
			return null;
		} else if (isIncoming) {
			return RelationshipPropertyDefinition.create(relationshipType, optionalPropertyHolder, fieldName, end,
				owner, properties);
		} else {
			return RelationshipPropertyDefinition.create(relationshipType, optionalPropertyHolder, fieldName, owner,
				end, properties);
		}
	}

	@Override
	protected PropertiesAndRelationshipGrouping newPropertiesAndRelationshipGrouping() {
		return new GroupPropertiesAndRelationships();
	}

	/**
	 * Pre-groups fields into properties and relationships to avoid running the association check multiple times.
	 */
	// Silence Sonar complaining about the class hierarchy, which is given through
	// the ElementKindVisitor8, which we need but cannot change
	@SuppressWarnings("squid:S110")
	class GroupPropertiesAndRelationships extends ElementKindVisitor8<Map<FieldType, List<Element>>, Void>
		implements PropertiesAndRelationshipGrouping {

		private final Map<FieldType, List<Element>> result;

		GroupPropertiesAndRelationships() {

			final Map<FieldType, List<Element>> hlp = new EnumMap<>(FieldType.class);
			hlp.put(FieldType.R, new ArrayList<>());
			hlp.put(FieldType.P, new ArrayList<>());
			this.result = Collections.unmodifiableMap(hlp);
		}

		public void apply(Element element) {
			element.accept(this, null);
		}

		public Map<FieldType, List<Element>> getResult() {
			return result;
		}

		@Override
		public Map<FieldType, List<Element>> visitTypeAsRecord(TypeElement e, Void unused) {
			// We must overwrite this or visitUnknown() in case we encounter a record
			return result;
		}

		@Override
		public Map<FieldType, List<Element>> visitRecordComponent(RecordComponentElement e, Void unused) {
			return visitFieldOrRecordComponent(e);
		}

		@Override
		public Map<FieldType, List<Element>> visitVariableAsField(VariableElement e, Void unused) {
			return visitFieldOrRecordComponent(e);
		}

		private Map<FieldType, List<Element>> visitFieldOrRecordComponent(Element e) {
			Set<Element> declaredAnnotations = e.getAnnotationMirrors().stream()
				.map(AnnotationMirror::getAnnotationType).map(DeclaredType::asElement).collect(Collectors.toSet());

			// Skip internal ids
			if (isInternalId(e, declaredAnnotations)) {
				return result;
			}

			result.get(isAssociation(declaredAnnotations, e) ? FieldType.R : FieldType.P).add(e);
			return result;
		}

		private boolean isInternalId(Element e, Set<Element> declaredAnnotations) {

			boolean idAnnotationPresent = declaredAnnotations.contains(ogmIdAnnotationType);
			if (!idAnnotationPresent) {
				return false;
			}

			return e.getAnnotationMirrors().stream()
				.filter(m -> m.getAnnotationType().asElement().equals(generatedValueAnnotationType)).findFirst()
				.map(generatedValue -> isUsingInternalIdGenerator(e, generatedValue)).orElse(false);
		}

		private boolean isUsingInternalIdGenerator(Element e, AnnotationMirror generatedValue) {

			Map<String, ? extends AnnotationValue> values = generatedValue.getElementValues().entrySet().stream()
				.collect(Collectors.toMap(entry -> entry.getKey().getSimpleName().toString(), Map.Entry::getValue));

			DeclaredType generatorClassValue = values.containsKey("strategy") ?
				(DeclaredType) values.get("strategy").getValue() :
				null;

			String name = null;
			if (generatorClassValue != null) {
				name = generatorClassValue.toString();
			}

			// The defaults will not be materialized
			return (name == null || "org.neo4j.ogm.id.InternalIdStrategy".equals(name))
				&& VALID_GENERATED_ID_TYPES.contains(e.asType().toString());
		}

		/**
		 * @param field A variable element describing a field. No further checks done if
		 *              this is true or not
		 * @return True if this field is an association
		 */
		private boolean isAssociation(Set<Element> declaredAnnotations, Element field) {
			TypeMirror typeMirrorOfField = field.asType();

			if (declaredAnnotations.contains(relationshipAnnotationType)) {
				return true;
			}

			if (declaredAnnotations.contains(propertyAnnotationType)) {
				return false;
			}

			// They will be converted anyway
			if (describesEnum(typeMirrorOfField)) {
				return false;
			}

			// Strings, primitives and their boxed variants are never associations
			if (typeMirrorOfField.getKind().isPrimitive()) {
				return false;
			} else {
				var type = typeMirrorOfField.accept(new TypeKindVisitor8<>() {
					@Override
					public String visitDeclared(DeclaredType t, Object o) {
						return t.asElement().accept(new TypeElementVisitor<>(newTypeElementNameFunction()), null);
					}
				}, null);
				if ("java.lang.String".equals(type)) {
					return false;
				}
				try {
					typeUtils.unboxedType(typeMirrorOfField);
					return false;
				} catch (IllegalArgumentException ex) {
					// Exception driven development for the win, yeah
				}
			}

			// Stuff that is explicitly converted can't be an association either
			for (var converterAnnotation : convertAnnotationTypes) {
				if (declaredAnnotations.contains(converterAnnotation)) {
					return false;
				}
			}

			// Unless it is a start / end node, it is by a big chance an implicit relationship and hence, an association
			boolean isStartNode = declaredAnnotations.contains(startNodeAnnotationType);
			boolean isEndNode = declaredAnnotations.contains(endNodeAnnotationType);
			return !(isStartNode || isEndNode);
		}
	}
}
