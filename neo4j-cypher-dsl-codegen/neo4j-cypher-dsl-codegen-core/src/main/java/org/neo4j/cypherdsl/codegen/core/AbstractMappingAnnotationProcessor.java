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

import java.io.IOException;
import java.io.Writer;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Filer;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleElementVisitor8;
import javax.lang.model.util.Types;
import javax.tools.JavaFileObject;

/**
 * Base class to build generators based on Neo4j supported object mapping frameworks (we
 * ship with support for SDN6+ and Neo4j-OGM).
 *
 * @author Michael J. Simons
 * @since 2025.0.0
 */
public abstract class AbstractMappingAnnotationProcessor extends AbstractProcessor {

	protected Messager messager;

	protected Types typeUtils;

	protected Filer filer;

	protected Configuration configuration;

	/**
	 * Default constructor is required by the annotation processor system of Java.
	 */
	public AbstractMappingAnnotationProcessor() {
	}

	/**
	 * Reads all supported from the processing environment and creates a suitable
	 * {@link Configuration}.
	 * @param processingEnv the processing environment
	 * @return a working configuration
	 */
	protected static Configuration createConfiguration(ProcessingEnvironment processingEnv) {
		Configuration.Builder builder = Configuration.newConfig();
		Map<String, String> options = processingEnv.getOptions();
		if (options.containsKey(Configuration.PROPERTY_PREFIX)) {
			builder.withPrefix(options.get(Configuration.PROPERTY_PREFIX));
		}
		if (options.containsKey(Configuration.PROPERTY_SUFFIX)) {
			builder.withSuffix(options.get(Configuration.PROPERTY_SUFFIX));
		}
		if (options.containsKey(Configuration.PROPERTY_INDENT_STYLE)) {
			builder
				.withIndentStyle(Configuration.IndentStyle.valueOf(options.get(Configuration.PROPERTY_INDENT_STYLE)));
		}
		if (options.containsKey(Configuration.PROPERTY_INDENT_SIZE)) {
			builder.withIndentSize(Integer.parseInt(options.get(Configuration.PROPERTY_INDENT_SIZE)));
		}
		return builder.withTimestamp(options.getOrDefault(Configuration.PROPERTY_TIMESTAMP, null))
			.withAddAtGenerated(
					Boolean.parseBoolean(options.getOrDefault(Configuration.PROPERTY_ADD_AT_GENERATED, "true")))
			.withTarget(processingEnv.getSourceVersion().equals(SourceVersion.RELEASE_8)
					? Configuration.JavaVersion.RELEASE_8 : Configuration.JavaVersion.RELEASE_11)
			.withExcludes(options.get(Configuration.PROPERTY_EXCLUDES))
			.build();
	}

	/**
	 * Checks if the definitions contain either all the same or no properties at all.
	 * @param definitions a list of all relationship definitions for one owning node
	 * @return true if all definitions have no or the same set of properties
	 */
	protected static boolean sameOrNoProperties(
			List<Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition>> definitions) {

		boolean same = true;
		Set<PropertyDefinition> properties = null;
		for (Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition> definition : definitions) {
			Set<PropertyDefinition> newProperties = definition.getValue().getProperties();
			if (properties == null) {
				properties = newProperties;
			}
			else if (properties.size() != newProperties.size() || !properties.containsAll(newProperties)) {
				same = false;
				break;
			}
		}
		return same;
	}

	/**
	 * Returns all types annotated with {@code annotation} found in the environment of
	 * this round.
	 * @param annotation the annotation to search for
	 * @param from the environment to extract from
	 * @return a set of type elements that match the given annotation
	 */
	protected Set<TypeElement> getTypesAnnotatedWith(TypeElement annotation, RoundEnvironment from) {
		return from.getElementsAnnotatedWith(annotation)
			.stream()
			.filter(e -> e.getKind().isClass())
			.map(TypeElement.class::cast)
			.filter(t -> !this.configuration.exclude(t.getQualifiedName().toString()))
			.collect(Collectors.toSet());
	}

	@Override
	public final synchronized void init(ProcessingEnvironment processingEnv) {
		super.init(processingEnv);

		this.typeUtils = processingEnv.getTypeUtils();
		this.filer = processingEnv.getFiler();
		this.messager = processingEnv.getMessager();

		this.configuration = createConfiguration(processingEnv);

		initFrameworkSpecific(processingEnv);
	}

	/**
	 * Do your framework specific initialisation here, such as figuring out your actual
	 * annotations.
	 * @param processingEnv the current processing env
	 */
	protected abstract void initFrameworkSpecific(ProcessingEnvironment processingEnv);

	@Override
	public final SourceVersion getSupportedSourceVersion() {
		return SourceVersion.latestSupported();
	}

	/**
	 * Creates {@link NodeModelBuilder node builder} for all classes annotated with
	 * {@code @Node}. The node builders won't have any properties associated with them.
	 * This is done in the next step.
	 * <p>
	 * The reason for not populating the properties in one go in this step is simple: When
	 * one annotated node class refers to another, we can be sure that this property is
	 * not a simple property but a relationship property, without going through other
	 * expensive checks. But to be able to do this, we must know the annotated classes
	 * upfront.
	 * @param classesAnnotatedWithNode the set of annotated classes
	 * @return a map from type to a node builder for that type
	 */
	protected final Map<TypeElement, NodeModelBuilder> populateListOfNodes(Set<TypeElement> classesAnnotatedWithNode) {

		Map<TypeElement, NodeModelBuilder> result = new HashMap<>();
		classesAnnotatedWithNode.forEach(annotatedClass -> {

			String qualifiedName = annotatedClass.getQualifiedName().toString();
			String suggestedTypeName = annotatedClass.getSimpleName().toString();
			String packageName = null;

			List<String> subpackages = new LinkedList<>();
			Element enclosingElement = annotatedClass.getEnclosingElement();
			while (!(enclosingElement == null || enclosingElement.getKind().equals(ElementKind.PACKAGE))) {
				subpackages.add(0, enclosingElement.getSimpleName().toString().toLowerCase(Locale.ROOT));
				enclosingElement = enclosingElement.getEnclosingElement();
			}

			if (enclosingElement == null) {
				int lastDot = qualifiedName.lastIndexOf('.');
				if (lastDot > 0) {
					packageName = qualifiedName.substring(0, lastDot);
				}
			}
			else if (enclosingElement.getKind().equals(ElementKind.PACKAGE)) {
				String q = ((PackageElement) enclosingElement).getQualifiedName().toString();
				packageName = q + ((q.isEmpty() || subpackages.isEmpty()) ? "" : ".") + String.join(".", subpackages);
			}

			NodeModelBuilder builder = NodeModelBuilder.create(this.configuration, packageName, suggestedTypeName)
				.addLabels(getLabel(annotatedClass));

			result.put(annotatedClass, builder);
		});
		return Collections.unmodifiableMap(result);
	}

	/**
	 * Finds labels on the annotated element. If there's no label specified on the node
	 * annotation, the simple class name is used. Otherwise, all labels in declaration
	 * order (primary, value, labels) are used.
	 * @param annotatedClass the annotated class
	 * @return a collection of labels
	 */
	protected abstract Collection<String> getLabel(TypeElement annotatedClass);

	protected final void writeSourceFiles(Collection<ModelBuilder<?>> builders) throws IOException {

		for (ModelBuilder<?> nodeModelBuilder : builders) {
			JavaFileObject jfo = this.filer.createSourceFile(nodeModelBuilder.getCanonicalClassName());
			try (Writer writer = jfo.openWriter()) {
				nodeModelBuilder.writeTo(writer);
			}
		}
	}

	protected abstract PropertiesAndRelationshipGrouping newPropertiesAndRelationshipGrouping();

	/**
	 * Turns an element into a property description.
	 * @param e the element to transform
	 * @return a new property description
	 */
	protected abstract PropertyDefinition asPropertyDefinition(Element e);

	/**
	 * This populates the properties of all node builders and returns all identifiable
	 * relationships. Why is this done as a side effect? The tests for relationships are
	 * rather expensive and may need to instantiate classes. When this is unavoidable, it
	 * makes sense to use that information right away and not do it a second time later
	 * one-
	 * @param nodeBuilders the map of all node builders for all annotated classes
	 * @return a map from a node builder to a list of fields describing relationships
	 */
	protected final Map<NodeModelBuilder, List<Element>> populateNodePropertiesAndCollectRelationshipFields(
			Map<TypeElement, NodeModelBuilder> nodeBuilders) {

		Map<NodeModelBuilder, List<Element>> relationshipFields = new HashMap<>();
		nodeBuilders.forEach((type, nodeImplBuilder) -> {

			var groupPropertiesAndRelationships = newPropertiesAndRelationshipGrouping();
			type.getEnclosedElements().forEach(groupPropertiesAndRelationships::apply);
			Map<FieldType, List<Element>> fields = groupPropertiesAndRelationships.getResult();

			nodeImplBuilder.addProperties(fields.get(FieldType.P).stream().map(this::asPropertyDefinition).toList());

			relationshipFields.put(nodeImplBuilder, fields.get(FieldType.R));
		});
		return Collections.unmodifiableMap(relationshipFields);
	}

	protected abstract RelationshipPropertyDefinition asRelationshipDefinition(NodeModelBuilder owner, Element e,
			Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> relationshipProperties,
			Map<TypeElement, NodeModelBuilder> nodeBuilders);

	/**
	 * This takes the list of all know fields pointing to relationships and computes a map
	 * with relationship types and their definitions. A specific relationship type may
	 * have been used between several other nodes.
	 * @param allRelationshipFields all known relationship files
	 * @param relationshipProperties a map of properties discovered for relationships
	 * @param nodeBuilders the current list of node builders to work with
	 * @return a map from a relationship type to a list of definitions. The definition
	 * including the node builders themselves. The entry in the list is (start,
	 * definition)
	 */
	protected final Map<String, List<Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition>>> computeRelationshipDefinitions(
			Map<NodeModelBuilder, List<Element>> allRelationshipFields,
			Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> relationshipProperties,
			Map<TypeElement, NodeModelBuilder> nodeBuilders) {

		Map<String, List<Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition>>> definitions = new HashMap<>();
		allRelationshipFields.forEach((start, l) -> l.forEach(f -> {
			RelationshipPropertyDefinition propertyDefinition = asRelationshipDefinition(start, f,
					relationshipProperties, nodeBuilders);
			if (propertyDefinition != null) {
				definitions.computeIfAbsent(propertyDefinition.getType(), k -> new ArrayList<>())
					.add(new AbstractMap.SimpleEntry<>(start, propertyDefinition));
			}
		}));
		return Collections.unmodifiableMap(definitions);
	}

	/**
	 * Creates and populates the list of relationships from their definitions. It also
	 * registered the freshly generated relationship builders with the previously created
	 * node builders.
	 * @param relationshipDefinitions definitions by type and owner
	 * @return map of builder per type. Can be multiple builders in case of different
	 * relationship property classes with different properties.
	 */
	protected final Map<String, List<RelationshipModelBuilder>> populateListOfRelationships(
			Map<String, List<Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition>>> relationshipDefinitions) {
		Map<String, List<RelationshipModelBuilder>> result = new HashMap<>();

		relationshipDefinitions.forEach((type, definitions) -> {
			RelationshipModelBuilder relationshipBuilder = null;

			// Simple case: All unique types
			if (definitions.size() == 1) {

				NodeModelBuilder owner = definitions.get(0).getKey();
				RelationshipPropertyDefinition definition = definitions.get(0).getValue();

				relationshipBuilder = RelationshipModelBuilder.create(this.configuration, owner.getPackageName(), type);
				relationshipBuilder.setStartNode(definition.getStart());
				relationshipBuilder.setEndNode(definition.getEnd());
				relationshipBuilder.addProperties(definition.getProperties());
			}
			else {
				Set<NodeModelBuilder> owners = definitions.stream().map(Map.Entry::getKey).collect(Collectors.toSet());

				// Exactly one owner, but variable targets
				if (owners.size() == 1) {

					NodeModelBuilder owner = owners.stream().findFirst().get();

					Map<Boolean, List<RelationshipPropertyDefinition>> ownerAtStartOrEnd = definitions.stream()
						.map(Map.Entry::getValue)
						.collect(Collectors.partitioningBy(p -> p.getStart() == owner));

					if (sameOrNoProperties(definitions)) {
						relationshipBuilder = RelationshipModelBuilder.create(this.configuration,
								owner.getPackageName(), type);
						relationshipBuilder.addProperties(definitions.get(0).getValue().getProperties());
						if (ownerAtStartOrEnd.get(true).isEmpty()) {
							relationshipBuilder.setEndNode(owner);
						}
						else if (ownerAtStartOrEnd.get(false).isEmpty()) {
							relationshipBuilder.setStartNode(owner);
						}
					}
					else {
						List<RelationshipModelBuilder> newBuilders = new ArrayList<>();
						for (Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition> definition : definitions) {

							RelationshipPropertyDefinition propertyDefinition = definition.getValue();
							RelationshipModelBuilder newBuilder = RelationshipModelBuilder.create(this.configuration,
									owner.getPackageName(), type, type + "_"
											+ propertyDefinition.getEnd().getPlainClassName().toUpperCase(Locale.ROOT));
							newBuilder.addProperties(propertyDefinition.getProperties());
							if (ownerAtStartOrEnd.get(true).isEmpty()) {
								newBuilder.setStartNode(propertyDefinition.getStart());
								newBuilder.setEndNode(owner);
							}
							else if (ownerAtStartOrEnd.get(false).isEmpty()) {
								newBuilder.setStartNode(owner);
								newBuilder.setEndNode(propertyDefinition.getEnd());
							}

							definition.getKey().addRelationshipDefinition(propertyDefinition.withBuilder(newBuilder));
							newBuilders.add(newBuilder);
						}
						result.put(type, Collections.unmodifiableList(newBuilders));
					}
				}
				else if (owners.size() > 1) {
					List<NodeModelBuilder> startNodes = definitions.stream()
						.map(d -> d.getValue().getStart())
						.distinct()
						.toList();
					List<NodeModelBuilder> endNodes = definitions.stream()
						.map(d -> d.getValue().getStart())
						.distinct()
						.toList();
					relationshipBuilder = RelationshipModelBuilder.create(this.configuration,
							owners.stream().findFirst().get().getPackageName(), type);
					if (startNodes.size() == 1) {
						relationshipBuilder.setStartNode(startNodes.get(0));
					}
					else if (endNodes.size() == 1) {
						relationshipBuilder.setStartNode(endNodes.get(0));
					}
				}
			}

			// A single builder created for all definitions
			// Multiple builders have been taken care of independent.
			if (relationshipBuilder != null) {
				for (Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition> definition : definitions) {
					RelationshipPropertyDefinition finishedDefinition = definition.getValue()
						.withBuilder(relationshipBuilder);
					definition.getKey().addRelationshipDefinition(finishedDefinition);
				}
				result.put(type, Collections.singletonList(relationshipBuilder));
			}
		});
		return Collections.unmodifiableMap(result);
	}

	protected final boolean describesEnum(TypeMirror typeMirror) {
		List<? extends TypeMirror> superTypes = this.typeUtils.directSupertypes(typeMirror);
		if (!(superTypes.size() == 1 && superTypes.get(0).getKind().equals(TypeKind.DECLARED))) {
			return false;
		}

		TypeMirror tm = superTypes.get(0);
		String name = ((DeclaredType) tm).asElement()
			.accept(new TypeElementVisitor<>(newTypeElementNameFunction()), null);
		return Enum.class.getName().equals(name);
	}

	protected final Function<TypeElement, String> newTypeElementNameFunction() {
		return new TypeElementNameFunction();
	}

	protected enum FieldType {

		P, R

	}

	protected interface PropertiesAndRelationshipGrouping {

		void apply(Element element);

		Map<FieldType, List<Element>> getResult();

	}

	/**
	 * Recursively extracts a loadable and instantiable name from a canonical class name
	 * by checking whether the current element is a nesting type. If so, the enclosing
	 * element will be visited and the elements simple name will be appended with a
	 * {@literal $}.
	 */
	private static final class TypeElementNameFunction implements Function<TypeElement, String> {

		@Override
		public String apply(TypeElement typeElement) {
			NestingKind nestingKind = typeElement.getNestingKind();
			if (nestingKind.isNested()) {
				return typeElement.getEnclosingElement().accept(new TypeElementVisitor<>(this), null) + "$"
						+ typeElement.getSimpleName();
			}
			return typeElement.getQualifiedName().toString();
		}

	}

	/**
	 * Utility class that can be used to extract a {@link TypeElement} from a type or
	 * anything else that one can retrieve from the actual element.
	 *
	 * @param <E> the type of the returned value
	 */
	protected static class TypeElementVisitor<E> extends SimpleElementVisitor8<E, Void> {

		private final Function<TypeElement, E> delegate;

		public TypeElementVisitor(Function<TypeElement, E> delegate) {
			this.delegate = delegate;
		}

		@Override
		public E visitType(TypeElement e, Void unused) {
			return this.delegate.apply(e);
		}

	}

}
