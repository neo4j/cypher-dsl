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
package org.neo4j.cypherdsl.core.querydsl;

import com.querydsl.core.types.Ops;
import com.querydsl.core.types.PathType;
import com.querydsl.core.types.Templates;

/**
 * @author Michael J. Simons
 * @soundtrack Die Toten Hosen - Learning English, Lesson Three: Mersey Beat! The Sound Of Liverpool
 */
class CypherTemplates extends Templates {

	static final String UNSUPPORTED_MARKER = "__UNSUPPORTED__";
	static final Templates DEFAULT = new CypherTemplates();

	private CypherTemplates() {
		super('\\');

		// boolean
		add(Ops.AND, "{0} AND {1}", Precedence.AND);
		add(Ops.NOT, "NOT {0}", Precedence.NOT_HIGH);
		add(Ops.OR, "{0} OR {1}", Precedence.OR);
		add(Ops.XNOR, "NOT ({0} XOR {1})", Precedence.XNOR);
		add(Ops.XOR, "{0} XOR {1}", Precedence.XOR);

		// collection
		add(Ops.COL_IS_EMPTY, "size({0}) = 0");
		add(Ops.COL_SIZE, "size({0})");

		// array
		add(Ops.ARRAY_SIZE, "size({0})");

		// map
		add(Ops.MAP_SIZE, "size(keys({0}))");
		add(Ops.MAP_IS_EMPTY, "size(keys({0})) = 0");
		add(Ops.CONTAINS_KEY, "any(v in keys({0}) where v = {1})");
		add(Ops.CONTAINS_VALUE, "any(v in [k IN KEYS({0}) | {0}[k]] where v = {1})");

		// comparison
		add(Ops.BETWEEN, "{0} >= {1} AND {0} <= {2}", Precedence.COMPARISON);
		add(Ops.GOE, "{0} >= {1}", Precedence.COMPARISON);
		add(Ops.GT, "{0} > {1}", Precedence.COMPARISON);
		add(Ops.LOE, "{0} <= {1}", Precedence.COMPARISON);
		add(Ops.LT, "{0} < {1}", Precedence.COMPARISON);

		// numeric
		add(Ops.NEGATE, "-{0}", Precedence.NEGATE);
		add(Ops.ADD, "{0} + {1}", Precedence.ARITH_LOW);
		add(Ops.DIV, "{0} / {1}", Precedence.ARITH_HIGH);
		add(Ops.MOD, "{0} % {1}", Precedence.ARITH_HIGH);
		add(Ops.MULT, "{0} * {1}", Precedence.ARITH_HIGH);
		add(Ops.SUB, "{0} - {1}", Precedence.ARITH_LOW);

		// various
		add(Ops.EQ, "{0} = {1}", Precedence.EQUALITY);
		add(Ops.EQ_IGNORE_CASE, "{0l} = {1l}", Precedence.EQUALITY);
		add(Ops.INSTANCE_OF, UNSUPPORTED_MARKER, Precedence.COMPARISON);
		add(Ops.NE, "{0} != {1}", Precedence.EQUALITY);

		add(Ops.IN, "any(v in {0} where v = {1})", Precedence.COMPARISON);
		add(Ops.NOT_IN, "none(v in {0} where v = {1})", Precedence.COMPARISON);
		add(Ops.IS_NULL, "{0} is null", Precedence.COMPARISON);
		add(Ops.IS_NOT_NULL, "{0} is not null", Precedence.COMPARISON);
		add(Ops.ALIAS, "{0} as {1}", 0);

		add(Ops.NUMCAST, UNSUPPORTED_MARKER);
		add(Ops.STRING_CAST, UNSUPPORTED_MARKER);

		// string
		add(Ops.CONCAT, "{0} + {1}", Precedence.ARITH_LOW);
		add(Ops.LOWER, "toLower({0})");
		add(Ops.SUBSTR_1ARG, "substring({0}, {1})");
		add(Ops.SUBSTR_2ARGS, "substring({0}, {1}, {2})");
		add(Ops.TRIM, "trim({0})");
		add(Ops.UPPER, "toUpper({0})");
		add(Ops.MATCHES, "{0} =~ {1}");
		add(Ops.MATCHES_IC, "{0} =~ ('(?i)' + {1})");
		add(Ops.STARTS_WITH, "{0} STARTS WITH {1}");
		add(Ops.STARTS_WITH_IC, "{0l} STARTS WITH {1l}");
		add(Ops.ENDS_WITH, "{0} ENDS WITH {1}");
		add(Ops.ENDS_WITH_IC, "{0l} ENDS WITH {1l}");
		add(Ops.STRING_CONTAINS, "{0} CONTAINS {1}");
		add(Ops.STRING_CONTAINS_IC, "{0l} CONTAINS {1l}");
		add(Ops.CHAR_AT, "substring({0}, {1}, 1)");
		add(Ops.STRING_LENGTH, "size({0})");
		add(Ops.INDEX_OF, UNSUPPORTED_MARKER);
		add(Ops.INDEX_OF_2ARGS, UNSUPPORTED_MARKER);
		add(Ops.STRING_IS_EMPTY, "size({0}) = 0 ");
		add(Ops.LIKE, "{0} =~ '.*' + {1} + '.*'", Precedence.COMPARISON);
		add(Ops.LIKE_IC, "{0} =~ '(?i).*' + {1} + '.*'", Precedence.COMPARISON);
		add(Ops.LIKE_ESCAPE, UNSUPPORTED_MARKER, Precedence.COMPARISON);
		add(Ops.LIKE_ESCAPE_IC, UNSUPPORTED_MARKER, Precedence.COMPARISON);

		add(Ops.StringOps.LEFT, "left({0}, {1})");
		add(Ops.StringOps.RIGHT, "right({0}, {1})");
		add(Ops.StringOps.LTRIM, "ltrim({0})");
		add(Ops.StringOps.RTRIM, "rtrim({0})");
		add(Ops.StringOps.LOCATE, UNSUPPORTED_MARKER);
		add(Ops.StringOps.LOCATE2, UNSUPPORTED_MARKER);
		add(Ops.StringOps.LPAD, UNSUPPORTED_MARKER);
		add(Ops.StringOps.RPAD, UNSUPPORTED_MARKER);
		add(Ops.StringOps.LPAD2, UNSUPPORTED_MARKER);
		add(Ops.StringOps.RPAD2, UNSUPPORTED_MARKER);

		// date time
		add(Ops.DateTimeOps.SYSDATE, "datetime()");
		add(Ops.DateTimeOps.CURRENT_DATE, "date()");
		add(Ops.DateTimeOps.CURRENT_TIME, "time()");
		add(Ops.DateTimeOps.CURRENT_TIMESTAMP, "datetime().epochmillis");
		add(Ops.DateTimeOps.DATE, "date({0})");

		add(Ops.DateTimeOps.MILLISECOND, "{0}.millisecond");
		add(Ops.DateTimeOps.SECOND, "{0}.second");
		add(Ops.DateTimeOps.MINUTE, "{0}.minute");
		add(Ops.DateTimeOps.HOUR, "{0}.hour");
		add(Ops.DateTimeOps.WEEK, "{0}.week");
		add(Ops.DateTimeOps.MONTH, "{0}.month");
		add(Ops.DateTimeOps.YEAR, "{0}.year");
		add(Ops.DateTimeOps.YEAR_MONTH, UNSUPPORTED_MARKER);
		add(Ops.DateTimeOps.YEAR_WEEK, "{0}.weekYear");
		add(Ops.DateTimeOps.DAY_OF_WEEK, "{0}.dayOfWeek");
		add(Ops.DateTimeOps.DAY_OF_MONTH, UNSUPPORTED_MARKER);
		add(Ops.DateTimeOps.DAY_OF_YEAR, UNSUPPORTED_MARKER);

		add(Ops.DateTimeOps.ADD_YEARS, "{0} + duration({years: {1}})");
		add(Ops.DateTimeOps.ADD_MONTHS, "{0} + duration({months: {1}})");
		add(Ops.DateTimeOps.ADD_WEEKS, "{0} + duration({weeks: {1}})");
		add(Ops.DateTimeOps.ADD_DAYS, "{0} + duration({days: {1}})");
		add(Ops.DateTimeOps.ADD_HOURS, "{0} + duration({hours: {1}})");
		add(Ops.DateTimeOps.ADD_MINUTES, "{0} + duration({minutes: {1}})");
		add(Ops.DateTimeOps.ADD_SECONDS, "{0} + duration({seconds: {1}})");

		add(Ops.DateTimeOps.DIFF_YEARS, "duration.between({0}, {1}).years");
		add(Ops.DateTimeOps.DIFF_MONTHS, "duration.between({0}, {1}).months");
		add(Ops.DateTimeOps.DIFF_WEEKS, "duration.between({0}, {1}).weeks");
		add(Ops.DateTimeOps.DIFF_DAYS, "duration.between({0}, {1}).days");
		add(Ops.DateTimeOps.DIFF_HOURS, "duration.between({0}, {1}).hours");
		add(Ops.DateTimeOps.DIFF_MINUTES, "duration.between({0}, {1}).minutes");
		add(Ops.DateTimeOps.DIFF_SECONDS, "duration.between({0}, {1}).seconds");

		add(Ops.DateTimeOps.TRUNC_YEAR, "date.truncate('year', {0})");
		add(Ops.DateTimeOps.TRUNC_MONTH, "date.truncate('month', {0})");
		add(Ops.DateTimeOps.TRUNC_WEEK, "date.truncate('week', {0})");
		add(Ops.DateTimeOps.TRUNC_DAY, "date.truncate('day', {0})");
		add(Ops.DateTimeOps.TRUNC_HOUR, "datetime.truncate('hour', {0})");
		add(Ops.DateTimeOps.TRUNC_MINUTE, "datetime.truncate('minute', {0})");
		add(Ops.DateTimeOps.TRUNC_SECOND, "datetime.truncate('second', {0})");

		// math
		add(Ops.MathOps.ABS, "abs({0})");
		add(Ops.MathOps.ACOS, "acos({0})");
		add(Ops.MathOps.ASIN, "asin({0})");
		add(Ops.MathOps.ATAN, "atan({0})");
		add(Ops.MathOps.CEIL, "ceil({0})");
		add(Ops.MathOps.COS, "cos({0})");
		add(Ops.MathOps.COSH, UNSUPPORTED_MARKER);
		add(Ops.MathOps.COT, "cot({0})");
		add(Ops.MathOps.COTH, UNSUPPORTED_MARKER);
		add(Ops.MathOps.DEG, "degrees({0})");
		add(Ops.MathOps.TAN, "tan({0})");
		add(Ops.MathOps.TANH, UNSUPPORTED_MARKER);
		add(Ops.MathOps.SQRT, "sqrt({0})");
		add(Ops.MathOps.SIGN, "sign({0})");
		add(Ops.MathOps.SIN, "sin({0})");
		add(Ops.MathOps.SINH, UNSUPPORTED_MARKER);
		add(Ops.MathOps.ROUND, "round({0})");
		add(Ops.MathOps.ROUND2, "round({0}, {1})");
		add(Ops.MathOps.RAD, "radians({0})");
		add(Ops.MathOps.RANDOM, UNSUPPORTED_MARKER);
		add(Ops.MathOps.RANDOM2, UNSUPPORTED_MARKER);
		add(Ops.MathOps.POWER, UNSUPPORTED_MARKER);
		add(Ops.MathOps.MIN, "CASE WHEN {0} < {1} THEN {0} ELSE {1} END");
		add(Ops.MathOps.MAX, "CASE WHEN {0} > {1} THEN {0} ELSE {1} END");
		add(Ops.MathOps.LOG, UNSUPPORTED_MARKER);
		add(Ops.MathOps.LN, UNSUPPORTED_MARKER);
		add(Ops.MathOps.FLOOR, "floor({0})");
		add(Ops.MathOps.EXP, "exp({0})");

		// path types
		add(PathType.PROPERTY, "{0}.{1s}");
		add(PathType.VARIABLE, "{0s}");
		add(PathType.DELEGATE, "{0}");
		add(Ops.ORDINAL, UNSUPPORTED_MARKER);

		for (PathType type : new PathType[] {
			PathType.LISTVALUE,
			PathType.MAPVALUE,
			PathType.ARRAYVALUE}) {
			add(type, "{0}[{1}]");
		}
		for (PathType type : new PathType[] {
			PathType.LISTVALUE_CONSTANT,
			PathType.MAPVALUE_CONSTANT,
			PathType.ARRAYVALUE_CONSTANT}) {
			add(type, "{0}[{1s}]");
		}

		// case
		add(Ops.CASE, "CASE {0} END", Precedence.CASE);
		add(Ops.CASE_WHEN,  "WHEN {0} THEN {1} {2}", Precedence.CASE);
		add(Ops.CASE_ELSE,  "ELSE {0}", Precedence.CASE);

		// case for
		add(Ops.CASE_EQ, "CASE {0} {1} END", Precedence.CASE);
		add(Ops.CASE_EQ_WHEN,  "WHEN {1} THEN {2} {3}", Precedence.CASE);
		add(Ops.CASE_EQ_ELSE,  "ELSE {0}", Precedence.CASE);

		// coalesce
		add(Ops.COALESCE, "coalesce({0})");

		add(Ops.NULLIF, UNSUPPORTED_MARKER);

		// subquery
		add(Ops.EXISTS, "exists({0})", 0);

		// numeric aggregates
		add(Ops.AggOps.BOOLEAN_ALL, "all({0})");
		add(Ops.AggOps.BOOLEAN_ANY, "any({0})");
		add(Ops.AggOps.AVG_AGG, "avg({0})");
		add(Ops.AggOps.MAX_AGG, "max({0})");
		add(Ops.AggOps.MIN_AGG, "min({0})");
		add(Ops.AggOps.SUM_AGG, "sum({0})");
		add(Ops.AggOps.COUNT_AGG, "count({0})");
		add(Ops.AggOps.COUNT_DISTINCT_AGG, "count(distinct {0})");
		add(Ops.AggOps.COUNT_DISTINCT_ALL_AGG, "count(distinct *)");
		add(Ops.AggOps.COUNT_ALL_AGG, "count(*)");

		// quantified expressions
		add(Ops.QuantOps.AVG_IN_COL, "avg({0})");
		add(Ops.QuantOps.MAX_IN_COL, "max({0})");
		add(Ops.QuantOps.MIN_IN_COL, "min({0})");

		add(Ops.QuantOps.ANY, "any {0}");
		add(Ops.QuantOps.ALL, "all {0}");
	}
}
