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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.time.Duration;
import java.time.Period;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAmount;
import java.time.temporal.TemporalUnit;
import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.BiFunction;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.apiguardian.api.API;
import org.jetbrains.annotations.NotNull;
import org.neo4j.cypherdsl.build.annotations.RegisterForReflection;
import org.neo4j.driver.Value;
import org.neo4j.driver.types.TypeSystem;

/**
 * @author Michael J. Simons
 * @soundtrack Prezident - Gesunder Eskapismus
 * @since 2023.2.1
 */
@API(status = INTERNAL, since = "2023.2.1")
@SuppressWarnings("unused")
@RegisterForReflection(allDeclaredConstructors = true)
class DriverValueAdapter implements ForeignAdapter<Value> {

	private final Value value;

	DriverValueAdapter(Value value) {
		this.value = value;
	}

	@Override
	@NotNull
	public Condition asCondition() {

		if (value.hasType(TypeSystem.getDefault().BOOLEAN())) {
			return asExpression().asCondition();
		}

		throw new UnsupportedOperationException("Only Boolean values can be adapted as condition");
	}

	@Override
	@NotNull
	public Expression asExpression() {

		return asExpression0(value);
	}

	private static Expression asExpression0(Value value) {
		var typeSystem = TypeSystem.getDefault();
		if (value.hasType(typeSystem.NODE())) {
			throw new IllegalArgumentException("Node values can only be adapted with asNode");
		}
		if (value.hasType(typeSystem.RELATIONSHIP())) {
			throw new IllegalArgumentException("Relationship values can only be adapted with asRelationship");
		}
		if (value.hasType(typeSystem.POINT())) {
			var p = value.asPoint();
			if (Double.isNaN(p.z())) {
				return new PointLiteral(new TreeMap<>(Map.of("srid", p.srid(), "x", p.x(), "y", p.y())));
			} else {
				return new PointLiteral(new TreeMap<>(Map.of("srid", p.srid(), "x", p.x(), "y", p.y(), "z", p.z())));
			}
		}
		if (value.hasType(typeSystem.FLOAT())) {
			return asFloatOrDouble(value);
		}
		if (value.hasType(typeSystem.DURATION())) {
			var d = value.asIsoDuration();
			return Cypher.literalOf(new TemporalAmountAdapter().apply(d));
		}
		if (value.hasType(typeSystem.BYTES())) {
			throw new IllegalArgumentException("byte[] values cannot be represented as expression.");
		}
		if (value.hasType(typeSystem.LIST())) {
			return Cypher.literalOf(value.asList(DriverValueAdapter::asExpression0));
		}
		if (value.hasType(typeSystem.MAP())) {
			return Cypher.literalOf(value.asMap(DriverValueAdapter::asExpression0));
		}

		return Cypher.literalOf(value.asObject());
	}

	@SuppressWarnings("squid:S1872") // See below error checking, it's a bug in the Neo4j Java driver not exporting that exception in the module path
	private static Literal<Object> asFloatOrDouble(Value value) {
		Number number;
		try {
			number = value.asFloat();
		} catch (Exception e) {
			if (!"org.neo4j.driver.exceptions.value.LossyCoercion".equals(e.getClass().getName())) {
				throw e;
			}
			number = value.asDouble();
		}
		return Cypher.literalOf(number);
	}

	@Override
	@NotNull
	public Node asNode() {

		if (!value.hasType(TypeSystem.getDefault().NODE())) {
			throw new IllegalArgumentException("Cannot adopt value with type " + value.type().name() + " as node");
		}

		var node = value.asNode();
		var labels = node.labels();
		String primaryLabel = null;
		var additionalLabels = new ArrayList<String>();
		for (String label : labels) {
			if (primaryLabel == null) {
				primaryLabel = label;
			} else {
				additionalLabels.add(label);
			}
		}
		var properties = node.size() == 0 ? null : MapExpression.create(node.asMap(DriverValueAdapter::asExpression0));
		if (primaryLabel != null) {
			return Cypher.node(primaryLabel, properties, additionalLabels);
		}
		return Cypher.anyNode().withProperties(properties);
	}

	@Override
	@NotNull
	public Relationship asRelationship() {

		if (!value.hasType(TypeSystem.getDefault().RELATIONSHIP())) {
			throw new IllegalArgumentException("Cannot adopt value with type " + value.type().name() + " as relationship");
		}

		var relationship = value.asRelationship();
		var properties = relationship.size() == 0 ? null : MapExpression.create(relationship.asMap(DriverValueAdapter::asExpression0));
		return Cypher.anyNode()
			.relationshipTo(Cypher.anyNode(), relationship.type()).withProperties(properties);
	}

	@Override
	@NotNull
	public SymbolicName asName() {
		throw new UnsupportedOperationException();
	}

	/**
	 * This adapter maps a Driver or embedded based {@link TemporalAmount} to a valid Java temporal amount. It tries to be
	 * as specific as possible: If the amount can be reliable mapped to a {@link Period}, it returns a period. If only
	 * fields are present that are no estimated time unites, then it returns a {@link Duration}. <br>
	 * <br>
	 * In cases a user has used Cypher and its <code>duration()</code> function, i.e. like so
	 * <code>CREATE (s:SomeTime {isoPeriod: duration('P13Y370M45DT25H120M')}) RETURN s</code> a duration object has been
	 * created that cannot be represented by either a {@link Period} or {@link Duration}. The user has to map it to a plain
	 * {@link TemporalAmount} in these cases. <br>
	 * The Java Driver uses a <code>org.neo4j.driver.types.IsoDuration</code>, embedded uses
	 * <code>org.neo4j.values.storable.DurationValue</code> for representing a temporal amount, but in the end, they can be
	 * treated the same. However, be aware that the temporal amount returned in that case may not be equal to the other one,
	 * only represents the same amount after normalization.
	 * <p>
	 * From Neo4j-OGM, to SDN6, to Cypher-DSL… The joy.
	 */
	static final class TemporalAmountAdapter implements UnaryOperator<TemporalAmount> {

		private static final int PERIOD_MASK = 0b11100;
		private static final int DURATION_MASK = 0b00011;
		private static final TemporalUnit[] SUPPORTED_UNITS = {ChronoUnit.YEARS, ChronoUnit.MONTHS, ChronoUnit.DAYS,
			ChronoUnit.SECONDS, ChronoUnit.NANOS};

		private static final short FIELD_YEAR = 0;
		private static final short FIELD_MONTH = 1;
		private static final short FIELD_DAY = 2;
		private static final short FIELD_SECONDS = 3;
		private static final short FIELD_NANOS = 4;

		private static final BiFunction<TemporalAmount, TemporalUnit, Integer> TEMPORAL_UNIT_EXTRACTOR = (d, u) -> {
			if (!d.getUnits().contains(u)) {
				return 0;
			}
			return Math.toIntExact(d.get(u));
		};

		@Override
		public TemporalAmount apply(TemporalAmount internalTemporalAmountRepresentation) {

			int[] values = new int[SUPPORTED_UNITS.length];
			int type = 0;
			for (int i = 0; i < SUPPORTED_UNITS.length; ++i) {
				values[i] = TEMPORAL_UNIT_EXTRACTOR.apply(internalTemporalAmountRepresentation, SUPPORTED_UNITS[i]);
				type |= (values[i] == 0) ? 0 : (0b10000 >> i);
			}

			boolean couldBePeriod = couldBePeriod(type);
			boolean couldBeDuration = couldBeDuration(type);

			if (couldBePeriod && !couldBeDuration) {
				return Period.of(values[FIELD_YEAR], values[FIELD_MONTH], values[FIELD_DAY]).normalized();
			} else if (couldBeDuration && !couldBePeriod) {
				return Duration.ofSeconds(values[FIELD_SECONDS]).plusNanos(values[FIELD_NANOS]);
			} else {
				return internalTemporalAmountRepresentation;
			}
		}

		private static boolean couldBePeriod(int type) {
			return (PERIOD_MASK & type) > 0;
		}

		private static boolean couldBeDuration(int type) {
			return (DURATION_MASK & type) > 0;
		}
	}

	private static final class PointLiteral extends LiteralBase<Map<String, Object>> {

		private PointLiteral(Map<String, Object> content) {
			super(content);
		}

		@Override
		@NotNull
		public String asString() {
			return content.entrySet().stream()
				.map(e -> e.getKey() + ": " + e.getValue())
				.collect(Collectors.joining(", ", "point({", "})"));
		}
	}
}
