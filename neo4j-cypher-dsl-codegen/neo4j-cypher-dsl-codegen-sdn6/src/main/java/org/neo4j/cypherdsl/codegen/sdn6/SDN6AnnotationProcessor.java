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
package org.neo4j.cypherdsl.codegen.sdn6;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.io.IOException;
import java.io.Writer;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Filer;
import javax.annotation.processing.Messager;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedOptions;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.NestingKind;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.PrimitiveType;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.ElementKindVisitor8;
import javax.lang.model.util.Elements;
import javax.lang.model.util.SimpleTypeVisitor8;
import javax.lang.model.util.Types;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.codegen.core.Configuration;
import org.neo4j.cypherdsl.codegen.core.ModelBuilder;
import org.neo4j.cypherdsl.codegen.core.NodeModelBuilder;
import org.neo4j.cypherdsl.codegen.core.PropertyDefinition;
import org.neo4j.cypherdsl.codegen.core.RelationshipModelBuilder;
import org.neo4j.cypherdsl.codegen.core.RelationshipPropertyDefinition;
import org.springframework.data.neo4j.core.convert.Neo4jConversions;
import org.springframework.data.neo4j.core.convert.Neo4jPersistentPropertyConverter;
import org.springframework.data.neo4j.core.convert.Neo4jSimpleTypes;
import org.springframework.data.neo4j.core.schema.Node;
import org.springframework.data.neo4j.core.schema.Property;
import org.springframework.data.neo4j.core.schema.Relationship;

/**
 * Annotation processor supporting the annotations provided by
 * <a href="https://github.com/spring-projects/spring-data-neo4j">Spring Data Neo4j 6+</a>.
 *
 * @author Michael J. Simons
 * @soundtrack Bear McCreary - Battlestar Galactica Season 2
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
@SupportedAnnotationTypes({ SDN6AnnotationProcessor.NODE_ANNOTATION, SDN6AnnotationProcessor.RELATIONSHIP_PROPERTIES_ANNOTATION})
@SupportedOptions({
	Configuration.PROPERTY_PREFIX,
	Configuration.PROPERTY_SUFFIX,
	Configuration.PROPERTY_INDENT_STYLE,
	Configuration.PROPERTY_INDENT_SIZE,
	Configuration.PROPERTY_TIMESTAMP,
	Configuration.PROPERTY_ADD_AT_GENERATED,
	SDN6AnnotationProcessor.PROPERTY_CUSTOM_CONVERTER_CLASSES
})
public final class SDN6AnnotationProcessor extends AbstractProcessor {

	static final String NODE_ANNOTATION = "org.springframework.data.neo4j.core.schema.Node";
	static final String RELATIONSHIP_PROPERTIES_ANNOTATION = "org.springframework.data.neo4j.core.schema.RelationshipProperties";
	static final String PROPERTY_CUSTOM_CONVERTER_CLASSES = "org.neo4j.cypherdsl.codegen.sdn.custom_converter_classes";
	static final Set<String> VALID_GENERATED_ID_TYPES = Set.of(Long.class.getName(), long.class.getName());

	// Resources
	// * http://hannesdorfmann.com/annotation-processing/annotationprocessing101/
	// * https://speakerdeck.com/gunnarmorling/das-annotation-processing-api-use-cases-und-best-practices

	private enum FieldType {
		P, R
	}

	static {
		disableSpringConverterDebugLog();
	}

	@SuppressWarnings("PMD")
	private static void disableSpringConverterDebugLog() {
		try {
			Class<?> logback = Class.forName("ch.qos.logback.classic.Logger");
			// Don't replace the qualified names, Checkstyle will yell at you.
			logback.getMethod("setLevel").invoke(
				org.slf4j.LoggerFactory.getLogger("org.springframework.data.convert.CustomConversions"),
				org.slf4j.event.Level.DEBUG
			);
		} catch (Exception e) {
			// There's nothing we can do or should do in case there is no logback.
		}
	}

	private Types typeUtils;
	private Filer filer;
	private Messager messager;

	private TypeElement nodeAnnotationType;
	private TypeElement relationshipAnnotationType;
	private TypeElement compositePropertyAnnotationType;
	private TypeElement convertWithAnnotationType;
	private TypeElement targetNodeAnnotationType;
	private TypeElement relationshipPropertiesAnnotationType;
	private TypeElement sdnIdAnnotationType;
	private TypeElement sdcIdAnnotationType;
	private TypeElement generatedValueAnnotationType;

	private Configuration configuration;
	private Neo4jConversions conversions;

	@Override
	public synchronized void init(ProcessingEnvironment processingEnv) {
		super.init(processingEnv);

		this.typeUtils = processingEnv.getTypeUtils();
		this.filer = processingEnv.getFiler();
		this.messager = processingEnv.getMessager();

		this.configuration = createConfiguration(processingEnv);
		this.conversions = createConversions(processingEnv);

		Elements elementUtils = processingEnv.getElementUtils();
		this.nodeAnnotationType = elementUtils.getTypeElement(NODE_ANNOTATION);
		this.relationshipPropertiesAnnotationType = elementUtils.getTypeElement(RELATIONSHIP_PROPERTIES_ANNOTATION);

		this.relationshipAnnotationType = elementUtils
			.getTypeElement("org.springframework.data.neo4j.core.schema.Relationship");
		this.compositePropertyAnnotationType = elementUtils
			.getTypeElement("org.springframework.data.neo4j.core.schema.CompositeProperty");
		this.convertWithAnnotationType = elementUtils
			.getTypeElement("org.springframework.data.neo4j.core.convert.ConvertWith");
		this.targetNodeAnnotationType = elementUtils
			.getTypeElement("org.springframework.data.neo4j.core.schema.TargetNode");

		this.sdnIdAnnotationType = elementUtils.getTypeElement("org.springframework.data.neo4j.core.schema.Id");
		this.sdcIdAnnotationType = elementUtils.getTypeElement("org.springframework.data.annotation.Id");
		this.generatedValueAnnotationType = elementUtils
			.getTypeElement("org.springframework.data.neo4j.core.schema.GeneratedValue");
	}

	/**
	 * Create an instance of {@link Neo4jConversions} and registers optional additional converters with it.
	 * The converters must have a default-non-args constructor.
	 * @param processingEnv The processing environment
	 * @return a conversions instance
	 */
	private Neo4jConversions createConversions(ProcessingEnvironment processingEnv) {

		Map<String, String> options = processingEnv.getOptions();
		if (!options.containsKey(PROPERTY_CUSTOM_CONVERTER_CLASSES)) {
			return new Neo4jConversions();
		}
		String classNames = options.get(PROPERTY_CUSTOM_CONVERTER_CLASSES);
		List<Object> converters = new ArrayList<>();
		Arrays.stream(classNames.split(",")).map(String::trim).filter(cn -> !cn.isEmpty())
			.forEach(cn -> {
				try {
					Class<?> clazz = Class.forName(cn);
					if (Neo4jPersistentPropertyConverter.class.isAssignableFrom(clazz)) {
						messager.printMessage(Diagnostic.Kind.MANDATORY_WARNING,
							"Cannot use dedicated Neo4j persistent property converter of type `" + cn + "` as Spring converter, it will be ignored.");
						return;
					}
					converters.add(clazz.getDeclaredConstructor().newInstance());
				} catch (Exception e) {
					String message = e.getMessage();
					if (e instanceof ClassNotFoundException) {
						message = "Class `" + cn + "` not found";
					}
					messager.printMessage(Diagnostic.Kind.MANDATORY_WARNING,
						"Cannot load converter of type `" + cn + "`, it will be ignored: " + message + ".");
				}
			});
		return new Neo4jConversions(converters);
	}

	/**
	 * Reads all supported from the processing environment and creates a suitable {@link Configuration}.
	 * @param processingEnv The processing environment
	 * @return A working configuration
	 */
	private static Configuration createConfiguration(ProcessingEnvironment processingEnv) {
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
		return builder
			.withTimestamp(options.getOrDefault(Configuration.PROPERTY_TIMESTAMP, null))
			.withAddAtGenerated(Boolean.parseBoolean(options.getOrDefault(Configuration.PROPERTY_ADD_AT_GENERATED, "true")))
			.withTarget(processingEnv.getSourceVersion().equals(SourceVersion.RELEASE_8) ?
				Configuration.JavaVersion.RELEASE_8 : Configuration.JavaVersion.RELEASE_11)
			.build();
	}

	@Override
	public SourceVersion getSupportedSourceVersion() {
		return SourceVersion.latestSupported();
	}

	@Override
	public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {

		if (annotations.isEmpty()) {
			return false;
		}

		Map<TypeElement, NodeModelBuilder> nodeBuilders =
			populateListOfNodes(getTypesAnnotatedWith(nodeAnnotationType, roundEnv));
		Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> relationshipProperties =
			collectRelationshipProperties(roundEnv);
		Map<NodeModelBuilder, List<VariableElement>> relationshipFields =
			populateNodePropertiesAndCollectRelationshipFields(nodeBuilders);
		Map<String, List<RelationshipModelBuilder>> relationshipBuilders =
			populateListOfRelationships(computeRelationshipDefinitions(relationshipFields, relationshipProperties, nodeBuilders));

		List<ModelBuilder<?>> allBuilders = new ArrayList<>(nodeBuilders.values());
		relationshipBuilders.values().forEach(allBuilders::addAll);
		try {
			writeSourceFiles(allBuilders);
		} catch (IOException e) {
			messager.printMessage(Diagnostic.Kind.ERROR, "Could not write source files: " + e.getMessage());
		}

		return true;
	}

	private void writeSourceFiles(Collection<ModelBuilder<?>> builders) throws IOException {

		for (ModelBuilder<?> nodeModelBuilder : builders) {
			JavaFileObject jfo = filer.createSourceFile(nodeModelBuilder.getCanonicalClassName());
			try (Writer writer = jfo.openWriter()) {
				nodeModelBuilder.writeTo(writer);
			}
		}
	}

	/**
	 * Returns all types annotated with {@code annotation} found in the environment of this round.
	 *
	 * @param annotation The annotation to search for
	 * @param from       The environment to extract from
	 * @return A set of type elements that match the given annotation
	 */
	private static Set<TypeElement> getTypesAnnotatedWith(TypeElement annotation, RoundEnvironment from) {
		return from.getElementsAnnotatedWith(annotation)
			.stream()
			.filter(e -> e.getKind().isClass())
			.map(TypeElement.class::cast)
			.collect(Collectors.toSet());
	}

	/**
	 * Creates {@link NodeModelBuilder node builder} for all classes annotated with {@code @Node}. The node builders won't
	 * have any properties associated with them. This is done in the next step.
	 * <p>
	 * The reason for not populating the properties in one go in this step is simple: When one annotated node class refers
	 * to another, we can be sure that this property is not a simple property but a relationship property, without going
	 * through other expensive checks. But to be able to do this, we must know the the annotated classes upfront.
	 *
	 * @param classesAnnotatedWithNode The set of annotated classes
	 * @return A map from type to a node builder for that type
	 */
	private Map<TypeElement, NodeModelBuilder> populateListOfNodes(Set<TypeElement> classesAnnotatedWithNode) {

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
			} else if (enclosingElement.getKind().equals(ElementKind.PACKAGE)) {
				String q = ((PackageElement) enclosingElement).getQualifiedName().toString();
				packageName = q + (q.isEmpty() || subpackages.isEmpty() ? "" : ".") + String.join(".", subpackages);
			}

			NodeModelBuilder builder = NodeModelBuilder.create(configuration, packageName, suggestedTypeName)
				.addLabels(getLabel(annotatedClass));

			result.put(annotatedClass, builder);
		});
		return Collections.unmodifiableMap(result);
	}

	/**
	 * Finds labels on the annotated element. If there's no label specified on the node annotation, the simple class name
	 * is used. Otherwise, all labels in declaration order (primary, value, labels) are used.
	 *
	 * @param annotatedClass The annotated class
	 * @return A collection of labels
	 */
	private Collection<String> getLabel(TypeElement annotatedClass) {

		Node nodeAnnotation = annotatedClass.getAnnotation(Node.class);
		Set<String> labels = new LinkedHashSet<>();
		Consumer<String> addLabel = label -> {
			if (!label.isEmpty()) {
				labels.add(label);
			}
		};

		addLabel.accept(nodeAnnotation.primaryLabel());
		Arrays.stream(nodeAnnotation.value()).forEach(addLabel);
		Arrays.stream(nodeAnnotation.labels()).forEach(addLabel);

		if (labels.isEmpty()) {
			addLabel.accept(annotatedClass.getSimpleName().toString());
		}
		return Collections.unmodifiableCollection(labels);
	}

	/**
	 * Collects classes annotated with {@code @RelationshipProperties}
	 *
	 * @param roundEnvironment The current environment
	 * @return A map from the other end of the relationship (target node) to the properties of the relationship
	 */
	private Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> collectRelationshipProperties(RoundEnvironment roundEnvironment) {

		Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> result = new HashMap<>();
		Set<TypeElement> relationshipProperties = getTypesAnnotatedWith(relationshipPropertiesAnnotationType, roundEnvironment);
		relationshipProperties.forEach(e -> {
			List<PropertyDefinition> properties = new ArrayList<>();
			TypeElement actualTargetType = null;
			for (Element enclosedElement : e.getEnclosedElements()) {
				if (!enclosedElement.getKind().isField()) {
					continue;
				}

				Set<Element> declaredAnnotations = enclosedElement.getAnnotationMirrors().stream()
					.map(AnnotationMirror::getAnnotationType).map(DeclaredType::asElement).collect(Collectors.toSet());
				if (declaredAnnotations.contains(targetNodeAnnotationType)) {

					Element element = typeUtils.asElement(enclosedElement.asType());
					actualTargetType = element.accept(new TypeElementVisitor<>(Function.identity()), null);
					if (actualTargetType == null) {
						messager.printMessage(Diagnostic.Kind.WARNING,
							"Cannot resolve generic type, not generating a property for relationships referring to " + e
								.getQualifiedName(), element);
					}
				} else {
					properties.add(asPropertyDefinition(enclosedElement));
				}
			}

			if (actualTargetType != null) {
				result
					.put(e, new AbstractMap.SimpleEntry<>(actualTargetType, Collections.unmodifiableList(properties)));
			}
		});
		return Collections.unmodifiableMap(result);
	}

	/**
	 * This populates the properties of all node builders and returns all identifiable relationships. Why is this done
	 * as a side effect? The tests for relationships are rather expensive and may need to instantiate classes. When this
	 * is unavoidable, it makes sense to use that information right away and not do it a second time later one-
	 *
	 * @param nodeBuilders The map of all node builders for all annoated classes
	 * @return A map from a node builder to a list of fields describing relationships
	 */
	private Map<NodeModelBuilder, List<VariableElement>> populateNodePropertiesAndCollectRelationshipFields(
		Map<TypeElement, NodeModelBuilder> nodeBuilders) {

		Map<NodeModelBuilder, List<VariableElement>> relationshipFields = new HashMap<>();
		nodeBuilders.forEach((type, nodeImplBuilder) -> {

			GroupPropertiesAndRelationships groupPropertiesAndRelationships = new GroupPropertiesAndRelationships();
			type.getEnclosedElements().forEach(groupPropertiesAndRelationships::apply);
			Map<FieldType, List<VariableElement>> fields = groupPropertiesAndRelationships.getResult();

			nodeImplBuilder.addProperties(
				fields.get(FieldType.P).stream().map(this::asPropertyDefinition).toList()
			);

			relationshipFields.put(nodeImplBuilder, fields.get(FieldType.R));
		});

		return Collections.unmodifiableMap(relationshipFields);
	}

	/**
	 * This takes the list of all know fields pointing to relationships and computes a map with relationship types and
	 * their definitions. A specific relationship type may has been used between several other nodes.
	 *
	 * @param allRelationshipFields All known relationship files
	 * @param relationshipProperties a map of properties discovered for relationships
	 * @return A map from a relationship type to a list of definitions. The definition including the node builders themselves.
	 * The entry in the list is (start, definition)
	 */
	private Map<String, List<Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition>>> computeRelationshipDefinitions(
		Map<NodeModelBuilder, List<VariableElement>> allRelationshipFields,
		Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> relationshipProperties,
		Map<TypeElement, NodeModelBuilder> nodeBuilders
	) {

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
	 * Creates and populates the list of relationships from their definitions. It also registered the freshly generated
	 * relationship builders with the previously created node builders.
	 *
	 * @param relationshipDefinitions Definitions by type and owner
	 * @return Map of builder per type. Can be multiple builders in case of different relationship property classes with different properties.
	 */
	private Map<String, List<RelationshipModelBuilder>> populateListOfRelationships(
		Map<String, List<Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition>>> relationshipDefinitions
	) {
		Map<String, List<RelationshipModelBuilder>> result = new HashMap<>();

		relationshipDefinitions.forEach((type, definitions) -> {
			RelationshipModelBuilder relationshipBuilder = null;

			// Simple case: All unique types
			if (definitions.size() == 1) {

				NodeModelBuilder owner = definitions.get(0).getKey();
				RelationshipPropertyDefinition definition = definitions.get(0).getValue();

				relationshipBuilder = RelationshipModelBuilder.create(configuration, owner.getPackageName(), type);
				relationshipBuilder.setStartNode(definition.getStart());
				relationshipBuilder.setEndNode(definition.getEnd());
				relationshipBuilder.addProperties(definition.getProperties());
			} else {
				Set<NodeModelBuilder> owners = definitions.stream().map(Map.Entry::getKey).collect(Collectors.toSet());

				// Exactly one owner, but variable targets
				if (owners.size() == 1) {

					NodeModelBuilder owner = owners.stream().findFirst().get();

					Map<Boolean, List<RelationshipPropertyDefinition>> ownerAtStartOrEnd = definitions.stream()
						.map(Map.Entry::getValue).collect(Collectors.partitioningBy(p -> p.getStart() == owner));

					if (sameOrNoProperties(definitions)) {
						relationshipBuilder = RelationshipModelBuilder.create(configuration, owner.getPackageName(), type);
						relationshipBuilder.addProperties(definitions.get(0).getValue().getProperties());
						if (ownerAtStartOrEnd.get(true).isEmpty()) {
							relationshipBuilder.setEndNode(owner);
						} else if (ownerAtStartOrEnd.get(false).isEmpty()) {
							relationshipBuilder.setStartNode(owner);
						}
					} else {
						List<RelationshipModelBuilder> newBuilders = new ArrayList<>();
						for (Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition> definition : definitions) {

							RelationshipPropertyDefinition propertyDefinition = definition.getValue();
							RelationshipModelBuilder newBuilder = RelationshipModelBuilder.create(
								configuration,
								owner.getPackageName(),
								type,
								type + "_" + propertyDefinition.getEnd().getPlainClassName().toUpperCase(Locale.ROOT)
							);
							newBuilder.addProperties(propertyDefinition.getProperties());
							if (ownerAtStartOrEnd.get(true).isEmpty()) {
								newBuilder.setStartNode(propertyDefinition.getStart());
								newBuilder.setEndNode(owner);
							} else if (ownerAtStartOrEnd.get(false).isEmpty()) {
								newBuilder.setStartNode(owner);
								newBuilder.setEndNode(propertyDefinition.getEnd());
							}

							definition.getKey().addRelationshipDefinition(propertyDefinition.withBuilder(newBuilder));
							newBuilders.add(newBuilder);
						}
						result.put(type, Collections.unmodifiableList(newBuilders));
					}
				} else if (owners.size() > 1) {
					List<NodeModelBuilder> startNodes = definitions.stream().map(d -> d.getValue().getStart()).distinct().toList();
					List<NodeModelBuilder> endNodes = definitions.stream().map(d -> d.getValue().getStart()).distinct().toList();

					relationshipBuilder = RelationshipModelBuilder.create(configuration, owners.stream().findFirst().get().getPackageName(), type);
					if (startNodes.size() == 1) {
						relationshipBuilder.setStartNode(startNodes.get(0));
					} else if (endNodes.size() == 1) {
						relationshipBuilder.setStartNode(endNodes.get(0));
					}
				}
			}

			// A single builder created for all definitions
			// Multiple builders have been taken care of independent.
			if (relationshipBuilder != null) {
				for (Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition> definition : definitions) {
					RelationshipPropertyDefinition finishedDefinition = definition.getValue().withBuilder(relationshipBuilder);
					definition.getKey().addRelationshipDefinition(finishedDefinition);
				}
				result.put(type, Collections.singletonList(relationshipBuilder));
			}
		});
		return Collections.unmodifiableMap(result);
	}

	/**
	 * @param definitions A list of all relationship definitions for one owning node
	 * @return True if all definitions have no or the same set of properties
	 */
	private static boolean sameOrNoProperties(List<Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition>> definitions) {

		boolean same = true;
		Set<PropertyDefinition> properties = null;
		for (Map.Entry<NodeModelBuilder, RelationshipPropertyDefinition> definition : definitions) {
			Set<PropertyDefinition> newProperties = definition.getValue().getProperties();
			if (properties == null) {
				properties = newProperties;
			} else if (properties.size() != newProperties.size() || !properties.containsAll(newProperties)) {
				same = false;
				break;
			}
		}
		return same;
	}

	private PropertyDefinition asPropertyDefinition(Element e) {

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
						"Different @AliasFor mirror values for annotation [org.springframework.data.neo4j.core.schema.Property]!",
						e);
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

	private RelationshipPropertyDefinition asRelationshipDefinition(NodeModelBuilder owner, Element e,
		Map<TypeElement, Map.Entry<TypeElement, List<PropertyDefinition>>> relationshipProperties,
		Map<TypeElement, NodeModelBuilder> nodeBuilders
	) {

		Optional<Relationship> optionalRelationshipAnnotation = Optional.ofNullable(e.getAnnotation(Relationship.class));

		String fieldName = e.getSimpleName().toString();

		// Default SDN 6 is outgoing. SDN 6 does not support undirected.
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
						"Different @AliasFor mirror values for annotation [org.springframework.data.neo4j.core.schema.Relationship]!",
						e);
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
			@Override
			public DeclaredType visitDeclared(DeclaredType t, Void unused) {
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
			return RelationshipPropertyDefinition.create(relationshipType, optionalPropertyHolder, fieldName, end, owner, properties);
		} else {
			return RelationshipPropertyDefinition.create(relationshipType, optionalPropertyHolder, fieldName, owner, end, properties);
		}
	}

	/**
	 * Pre-groups fields into properties and relationships to avoid running the association check multiple times.
	 */
	@SuppressWarnings("squid:S110") // Not something we need or can do anything about.
	class GroupPropertiesAndRelationships extends ElementKindVisitor8<Map<FieldType, List<VariableElement>>, Void> {

		private final Map<FieldType, List<VariableElement>> result;

		GroupPropertiesAndRelationships() {

			final Map<FieldType, List<VariableElement>> hlp = new EnumMap<>(FieldType.class);
			hlp.put(FieldType.R, new ArrayList<>());
			hlp.put(FieldType.P, new ArrayList<>());
			this.result = Collections.unmodifiableMap(hlp);
		}

		void apply(Element element) {
			element.accept(this, null);
		}

		Map<FieldType, List<VariableElement>> getResult() {
			return result;
		}

		@Override
		public Map<FieldType, List<VariableElement>> visitVariableAsField(VariableElement e, Void unused) {

			Set<Element> declaredAnnotations = e.getAnnotationMirrors().stream()
				.map(AnnotationMirror::getAnnotationType).map(DeclaredType::asElement).collect(Collectors.toSet());

			// Skip internal ids
			if (isInternalId(e, declaredAnnotations)) {
				return result;
			}

			result.get(isAssociation(declaredAnnotations, e) ? FieldType.R : FieldType.P).add(e);
			return result;
		}

		private boolean isInternalId(VariableElement e, Set<Element> declaredAnnotations) {

			boolean idAnnotationPresent = declaredAnnotations.contains(sdcIdAnnotationType) || declaredAnnotations.contains(sdnIdAnnotationType);
			if (!idAnnotationPresent) {
				return false;
			}

			return e.getAnnotationMirrors().stream()
				.filter(m -> m.getAnnotationType().asElement().equals(generatedValueAnnotationType))
				.findFirst()
				.map(generatedValue -> isUsingInternalIdGenerator(e, generatedValue))
				.orElse(false);
		}

		private boolean isUsingInternalIdGenerator(VariableElement e, AnnotationMirror generatedValue) {

			Map<String, ? extends AnnotationValue> values = generatedValue
				.getElementValues().entrySet().stream()
				.collect(Collectors.toMap(entry -> entry.getKey().getSimpleName().toString(), Map.Entry::getValue));

			DeclaredType generatorClassValue = values.containsKey("generatorClass") ?
				(DeclaredType) values.get("generatorClass").getValue() : null;
			DeclaredType valueValue = values.containsKey("value") ?
				(DeclaredType) values.get("value").getValue() : null;

			String name = null;
			if (generatorClassValue != null && valueValue != null && !generatorClassValue.equals(valueValue)) {
				messager.printMessage(
					Diagnostic.Kind.ERROR,
					"Different @AliasFor mirror values for annotation [org.springframework.data.neo4j.core.schema.GeneratedValue]!",
					e
				);
			} else if (generatorClassValue != null) {
				name = generatorClassValue.toString();
			} else if (valueValue != null) {
				name = valueValue.toString();
			}

			// The defaults will not be materialized
			return (name == null || "org.springframework.data.neo4j.core.schema.GeneratedValue.InternalIdGenerator".equals(name))
				&& VALID_GENERATED_ID_TYPES.contains(e.asType().toString());
		}

		/**
		 * Reassembles org.springframework.data.neo4j.core.mapping.DefaultNeo4jPersistentProperty#isAssociation
		 *
		 * @param field A variable element describing a field. No further checks done if this is true or not
		 * @return True if this field is an association
		 */
		private boolean isAssociation(Set<Element> declaredAnnotations, VariableElement field) {

			TypeMirror typeMirrorOfField = field.asType();
			boolean explicitRelationship = declaredAnnotations.contains(relationshipAnnotationType);
			boolean isTargetNodeOrComposite =
				declaredAnnotations.contains(targetNodeAnnotationType) || declaredAnnotations
					.contains(compositePropertyAnnotationType);

			BooleanSupplier simpleTypeOrCustomWriteTarget = () -> {
				try {
					String className = typeMirrorOfField.accept(new SimpleTypeVisitor8<String, Void>() {
						@Override
						public String visitPrimitive(PrimitiveType t, Void unused) {
							// While I could use the fact that this is a primitive directly, I'd rather stick with the
							// wrapper class so that in turn this can be passed on to the Spring infrastructure.
							return typeUtils.boxedClass(t).getQualifiedName().toString();
						}

						@Override
						public String visitDeclared(DeclaredType t, Void unused) {
							return t.asElement().accept(new TypeElementVisitor<>(new TypeElementNameFunction()), null);
						}
					}, null);
					// This is likely to fail for everything but primitives as the associated thing is currently compiled
					Class<?> fieldType = Class.forName(className);
					return Neo4jSimpleTypes.HOLDER.isSimpleType(fieldType) || conversions.hasCustomWriteTarget(fieldType);
				} catch (ClassNotFoundException e) {
					return false;
				}
			};

			if (explicitRelationship) {
				return true;
			}

			// They will be converted anyway
			if (describesEnum(typeMirrorOfField)) {
				return false;
			}

			return !(isTargetNodeOrComposite || declaredAnnotations.contains(convertWithAnnotationType) || simpleTypeOrCustomWriteTarget.getAsBoolean());
		}

		private boolean describesEnum(TypeMirror typeMirror) {
			List<? extends TypeMirror> superTypes = typeUtils.directSupertypes(typeMirror);
			if (!(superTypes.size() == 1 && superTypes.get(0).getKind().equals(TypeKind.DECLARED))) {
				return false;
			}

			TypeMirror tm = superTypes.get(0);
			String name = ((DeclaredType) tm).asElement().accept(new TypeElementVisitor<>(new TypeElementNameFunction()), null);
			return Enum.class.getName().equals(name);
		}
	}

	/**
	 * Recursively extracts a loadable and instantiable name from a canonical class name by checking whether the current
	 * element is a nesting type. If so, the enclosing element will be visited and this elements simple name will be
	 * appended with a {@literal $}.
	 */
	private static class TypeElementNameFunction implements Function<TypeElement, String> {
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
}
