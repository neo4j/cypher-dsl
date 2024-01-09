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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;

/**
 * @author Andreas Berger
 */
class ModelBuilderTest {

	@Test
	void testGeneratingRelationWithMultipleStartAndEndNodes() {
		var configuration = Configuration.newConfig().build();
		var a = NodeModelBuilder.create(configuration, null, "A")
			.addLabel("A");
		var b = NodeModelBuilder.create(configuration, null, "B")
			.addLabel("B")
			.addLabel("X");
		var c = NodeModelBuilder.create(configuration, null, "C")
			.addLabel("C");
		var rel = RelationshipModelBuilder.create(configuration, null, "BELONGS_TO")
			.addRelationship(a, b)
			.addRelationship(a, c)
			.addRelationship(c, b);

		a.addRelationshipDefinition(
				RelationshipPropertyDefinition.create("BELONGS_TO", null, "belongsTo", a, b, null).withBuilder(rel))
			.addRelationshipFactory(RelationshipFactoryDefinition.create("belongsTo", a, b).withBuilder(rel))
			.addRelationshipFactory(RelationshipFactoryDefinition.create("belongsTo", a, c).withBuilder(rel));

		b.addRelationshipDefinition(
				RelationshipPropertyDefinition.create("BELONGS_TO", null, "belongsTo", a, b, null).withBuilder(rel))
			.addRelationshipFactory(RelationshipFactoryDefinition.create("belongsTo", a, b).withBuilder(rel));

		assertThat(a.writeToString()).isEqualTo("""
			import java.util.List;
			import org.neo4j.cypherdsl.core.MapExpression;
			import org.neo4j.cypherdsl.core.NodeBase;
			import org.neo4j.cypherdsl.core.NodeLabel;
			import org.neo4j.cypherdsl.core.Properties;
			import org.neo4j.cypherdsl.core.SymbolicName;

			/**
			 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
			 */
			public final class A_ extends NodeBase<A_> {
				public static final String $PRIMARY_LABEL = "A";

				public static final A_ A = new A_();

				public final BelongsTo_<A_, B_> BELONGS_TO = new BelongsTo_<A_, B_>(this, B_.B);

				public A_() {
					super($PRIMARY_LABEL);
				}

				private A_(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
					super(symbolicName, labels, properties);
				}

				@Override
				public A_ named(SymbolicName newSymbolicName) {
					return new A_(newSymbolicName, getLabels(), getProperties());
				}

				@Override
				public A_ withProperties(MapExpression newProperties) {
					return new A_(getSymbolicName().orElse(null), getLabels(), Properties.create(newProperties));
				}

				public BelongsTo_<A_, B_> belongsTo(B_ end) {
					return new BelongsTo_<A_, B_>(this, end);
				}

				public BelongsTo_<A_, C_> belongsTo(C_ end) {
					return new BelongsTo_<A_, C_>(this, end);
				}
			}
			""");

		assertThat(b.writeToString()).isEqualTo("""
			import java.util.List;
			import org.neo4j.cypherdsl.core.MapExpression;
			import org.neo4j.cypherdsl.core.NodeBase;
			import org.neo4j.cypherdsl.core.NodeLabel;
			import org.neo4j.cypherdsl.core.Properties;
			import org.neo4j.cypherdsl.core.SymbolicName;

			/**
			 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
			 */
			public final class B_ extends NodeBase<B_> {
				public static final String $PRIMARY_LABEL = "B";

				public static final B_ B = new B_();

				public final BelongsTo_<A_, B_> BELONGS_TO = new BelongsTo_<A_, B_>(A_.A, this);

				public B_() {
					super($PRIMARY_LABEL, "X");
				}

				private B_(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
					super(symbolicName, labels, properties);
				}

				@Override
				public B_ named(SymbolicName newSymbolicName) {
					return new B_(newSymbolicName, getLabels(), getProperties());
				}

				@Override
				public B_ withProperties(MapExpression newProperties) {
					return new B_(getSymbolicName().orElse(null), getLabels(), Properties.create(newProperties));
				}

				public BelongsTo_<A_, B_> belongsTo(A_ start) {
					return new BelongsTo_<A_, B_>(start, this);
				}
			}
			""");

		assertThat(rel.writeToString()).isEqualTo("""
			import org.neo4j.cypherdsl.core.MapExpression;
			import org.neo4j.cypherdsl.core.Node;
			import org.neo4j.cypherdsl.core.NodeBase;
			import org.neo4j.cypherdsl.core.Properties;
			import org.neo4j.cypherdsl.core.RelationshipBase;
			import org.neo4j.cypherdsl.core.SymbolicName;

			/**
			 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
			 */
			public final class BelongsTo_<S extends NodeBase<?>, E extends NodeBase<?>> extends RelationshipBase<S, E, BelongsTo_<S, E>> {
				public static final String $TYPE = "BELONGS_TO";

				public BelongsTo_(A_ start, B_ end) {
					super((S) start, $TYPE, (E) end);
				}

				public BelongsTo_(A_ start, C_ end) {
					super((S) start, $TYPE, (E) end);
				}

				public BelongsTo_(C_ start, B_ end) {
					super((S) start, $TYPE, (E) end);
				}

				private BelongsTo_(SymbolicName symbolicName, Node start, Properties properties, Node end) {
					super(symbolicName, start, $TYPE, properties, end);
				}

				@Override
				public BelongsTo_<S, E> named(SymbolicName newSymbolicName) {
					return new BelongsTo_<>(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
				}

				@Override
				public BelongsTo_<S, E> withProperties(MapExpression newProperties) {
					return new BelongsTo_<>(getSymbolicName().orElse(null), getLeft(), Properties.create(newProperties), getRight());
				}
			}
			""");
	}

	@Test
	void testConstructorsOfRelationsWithoutGeneric() {
		var configuration = Configuration.newConfig().build();
		var a = NodeModelBuilder.create(configuration, null, "A")
			.addLabel("A");
		var b = NodeModelBuilder.create(configuration, null, "B")
			.addLabel("B");

		assertThat(RelationshipModelBuilder.create(configuration, null, "BELONGS_TO")
			.addRelationship(a, b)
			.writeToString())
			.isEqualTo("""
				import org.neo4j.cypherdsl.core.MapExpression;
				import org.neo4j.cypherdsl.core.Node;
				import org.neo4j.cypherdsl.core.Properties;
				import org.neo4j.cypherdsl.core.RelationshipBase;
				import org.neo4j.cypherdsl.core.SymbolicName;

				/**
				 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
				 */
				public final class BelongsTo_ extends RelationshipBase<A_, B_, BelongsTo_> {
					public static final String $TYPE = "BELONGS_TO";

					public BelongsTo_(A_ start, B_ end) {
						super(start, $TYPE, end);
					}

					private BelongsTo_(SymbolicName symbolicName, Node start, Properties properties, Node end) {
						super(symbolicName, start, $TYPE, properties, end);
					}

					@Override
					public BelongsTo_ named(SymbolicName newSymbolicName) {
						return new BelongsTo_(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
					}

					@Override
					public BelongsTo_ withProperties(MapExpression newProperties) {
						return new BelongsTo_(getSymbolicName().orElse(null), getLeft(), Properties.create(newProperties), getRight());
					}
				}
				""");
	}

	@Test
	void testConstructorsOfRelationsWithEndNodeGeneric() {
		var configuration = Configuration.newConfig().build();
		var a = NodeModelBuilder.create(configuration, null, "A")
			.addLabel("A");
		var b = NodeModelBuilder.create(configuration, null, "B")
			.addLabel("B");
		var c = NodeModelBuilder.create(configuration, null, "C")
			.addLabel("C");

		assertThat(RelationshipModelBuilder.create(configuration, null, "BELONGS_TO")
			.addRelationship(a, b)
			.addRelationship(a, c)
			.writeToString())
			.isEqualTo("""
				import org.neo4j.cypherdsl.core.MapExpression;
				import org.neo4j.cypherdsl.core.Node;
				import org.neo4j.cypherdsl.core.NodeBase;
				import org.neo4j.cypherdsl.core.Properties;
				import org.neo4j.cypherdsl.core.RelationshipBase;
				import org.neo4j.cypherdsl.core.SymbolicName;

				/**
				 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
				 */
				public final class BelongsTo_<E extends NodeBase<?>> extends RelationshipBase<A_, E, BelongsTo_<E>> {
					public static final String $TYPE = "BELONGS_TO";

					public BelongsTo_(A_ start, B_ end) {
						super(start, $TYPE, (E) end);
					}

					public BelongsTo_(A_ start, C_ end) {
						super(start, $TYPE, (E) end);
					}

					private BelongsTo_(SymbolicName symbolicName, Node start, Properties properties, Node end) {
						super(symbolicName, start, $TYPE, properties, end);
					}

					@Override
					public BelongsTo_<E> named(SymbolicName newSymbolicName) {
						return new BelongsTo_<>(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
					}

					@Override
					public BelongsTo_<E> withProperties(MapExpression newProperties) {
						return new BelongsTo_<>(getSymbolicName().orElse(null), getLeft(), Properties.create(newProperties), getRight());
					}
				}
				""");
	}

	@Test
	void testConstructorsOfRelationsWithStartNodeGeneric() {
		var configuration = Configuration.newConfig().build();
		var a = NodeModelBuilder.create(configuration, null, "A")
			.addLabel("A");
		var b = NodeModelBuilder.create(configuration, null, "B")
			.addLabel("B");
		var c = NodeModelBuilder.create(configuration, null, "C")
			.addLabel("C");

		assertThat(RelationshipModelBuilder.create(configuration, null, "BELONGS_TO")
			.addRelationship(a, b)
			.addRelationship(c, b)
			.writeToString())
			.isEqualTo("""
				import org.neo4j.cypherdsl.core.MapExpression;
				import org.neo4j.cypherdsl.core.Node;
				import org.neo4j.cypherdsl.core.NodeBase;
				import org.neo4j.cypherdsl.core.Properties;
				import org.neo4j.cypherdsl.core.RelationshipBase;
				import org.neo4j.cypherdsl.core.SymbolicName;

				/**
				 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
				 */
				public final class BelongsTo_<S extends NodeBase<?>> extends RelationshipBase<S, B_, BelongsTo_<S>> {
					public static final String $TYPE = "BELONGS_TO";

					public BelongsTo_(A_ start, B_ end) {
						super((S) start, $TYPE, end);
					}

					public BelongsTo_(C_ start, B_ end) {
						super((S) start, $TYPE, end);
					}

					private BelongsTo_(SymbolicName symbolicName, Node start, Properties properties, Node end) {
						super(symbolicName, start, $TYPE, properties, end);
					}

					@Override
					public BelongsTo_<S> named(SymbolicName newSymbolicName) {
						return new BelongsTo_<>(newSymbolicName, getLeft(), getDetails().getProperties(), getRight());
					}

					@Override
					public BelongsTo_<S> withProperties(MapExpression newProperties) {
						return new BelongsTo_<>(getSymbolicName().orElse(null), getLeft(), Properties.create(newProperties), getRight());
					}
				}
				""");
	}

	@Test
	void testGeneratingWithInheritance() {
		var configuration = Configuration.newConfig().build();
		var a = NodeModelBuilder.create(configuration, null, "A")
			.addLabel("A")
			.addLabel("B")
			.addProperty("foo")
			.setExtensible(true);

		var c = NodeModelBuilder.create(configuration, null, "C")
			.addLabel("C")
			.addLabel("D")
			.setBaseNodeModel(a)
			.addProperty("bar");

		assertThat(a.writeToString()).isEqualTo("""
			import java.util.Arrays;
			import java.util.List;
			import java.util.stream.Stream;
			import org.neo4j.cypherdsl.core.MapExpression;
			import org.neo4j.cypherdsl.core.NodeBase;
			import org.neo4j.cypherdsl.core.NodeLabel;
			import org.neo4j.cypherdsl.core.Properties;
			import org.neo4j.cypherdsl.core.Property;
			import org.neo4j.cypherdsl.core.SymbolicName;

			/**
			 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
			 */
			public class A_<SELF extends A_> extends NodeBase<SELF> {
				public static final String $PRIMARY_LABEL = "A";

				public static final A_<A_> A = new A_<>();

				public final Property FOO = this.property("foo").referencedAs("foo");

				public A_() {
					super($PRIMARY_LABEL, "B");
				}

				protected A_(String primaryLabel, String... additionalLabels) {
					super(primaryLabel, Stream.concat(Arrays.stream(additionalLabels), Stream.of($PRIMARY_LABEL, "B")).toArray(String[]::new));
				}

				protected A_(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
					super(symbolicName, labels, properties);
				}

				protected SELF create(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
					return (SELF) new A_(symbolicName, labels, properties);
				}

				@Override
				public SELF named(SymbolicName newSymbolicName) {
					return create(newSymbolicName, getLabels(), getProperties());
				}

				@Override
				public SELF withProperties(MapExpression newProperties) {
					return create(getSymbolicName().orElse(null), getLabels(), Properties.create(newProperties));
				}
			}
			""");

		assertThat(c.writeToString()).isEqualTo("""
			import java.util.List;
			import org.neo4j.cypherdsl.core.NodeLabel;
			import org.neo4j.cypherdsl.core.Properties;
			import org.neo4j.cypherdsl.core.Property;
			import org.neo4j.cypherdsl.core.SymbolicName;

			/**
			 * This class is generated by the Neo4j Cypher-DSL. All changes to it will be lost after regeneration.
			 */
			public final class C_ extends A_<C_> {
				public static final String $PRIMARY_LABEL = "C";

				public static final C_ C = new C_();

				public final Property BAR = this.property("bar").referencedAs("bar");

				public C_() {
					super($PRIMARY_LABEL, "D");
				}

				private C_(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
					super(symbolicName, labels, properties);
				}

				@Override
				protected C_ create(SymbolicName symbolicName, List<NodeLabel> labels, Properties properties) {
					return new C_(symbolicName, labels, properties);
				}
			}
			""");
	}

	@Test
	void testThrowingErrorWhenExtendingNonExtensibleNode() {
		var configuration = Configuration.newConfig().build();
		var a = NodeModelBuilder.create(configuration, null, "A")
			.addLabel("A");

		var c = NodeModelBuilder.create(configuration, null, "C")
			.addLabel("C")
			.setBaseNodeModel(a);

		// test that exception is thrown
		assertThatThrownBy(c::writeToString)
			.isInstanceOf(IllegalStateException.class)
			.hasMessage("Cannot extend non-extensible node A_");

	}

}
