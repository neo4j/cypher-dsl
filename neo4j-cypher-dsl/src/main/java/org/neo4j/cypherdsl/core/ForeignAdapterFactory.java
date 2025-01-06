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
package org.neo4j.cypherdsl.core;

import static org.apiguardian.api.API.Status.INTERNAL;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apiguardian.api.API;

/**
 * This factory is meant to decouple the instantiating respectively concrete usage of classes on the provided path as
 * much as possible, to avoid eager loading by some JVM and in turn, a class not found exception.
 *
 * @author Michael J. Simons
 * @soundtrack Various - Chef Aid: The South Park Album
 * @since 2021.1.0
 */
@API(status = INTERNAL, since = "2021.1.0")
final class ForeignAdapterFactory {

	private final Map<Class<?>, Constructor<ForeignAdapter<?>>> adapterCache = new HashMap<>();

	@SuppressWarnings("unchecked") // We do check the type of expression
	<FE> ForeignAdapter<FE> getAdapterFor(FE expression) {

		if (expression == null) {
			throw new IllegalArgumentException("Cannot adapt literal NULL expressions.");
		}

		var constructor = adapterCache.computeIfAbsent(expression.getClass(), k -> {
			var interfaces = getInterfaces(k);
			String adapterName;
			if (interfaces.stream().anyMatch("org.neo4j.driver.Value"::equals)) {
				adapterName = "org.neo4j.cypherdsl.core.DriverValueAdapter";
			} else if (interfaces.stream().anyMatch("com.querydsl.core.types.Expression"::equals)) {
				adapterName = "org.neo4j.cypherdsl.core.QueryDSLAdapter";
			} else {
				throw newCannotAdaptException(k, null);
			}
			try {
				return (Constructor<ForeignAdapter<?>>) Class.forName(adapterName).getDeclaredConstructors()[0];
			} catch (ClassNotFoundException e) {
				throw newCannotAdaptException(k, e);
			}
		});

		try {
			return (ForeignAdapter<FE>) constructor.newInstance(expression);
		} catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
			throw newCannotAdaptException(expression.getClass(), e);
		}
	}

	private static IllegalArgumentException newCannotAdaptException(Class<?> k, Exception cause) {
		var msg = "Cannot adapt expressions of type " + k.getName() + " to Cypher-DSL expressions.";
		return new IllegalArgumentException(msg, cause);
	}

	private static Set<String> getInterfaces(Class<?> type) {

		if (type == null || type == Object.class) {
			return Set.of();
		}

		return Stream.concat(getInterfaces(type.getSuperclass()).stream(), Arrays.stream(type.getInterfaces())
				.flatMap(i -> Stream.concat(Stream.of(i.getName()), getInterfaces(i).stream())))
			.collect(Collectors.toSet());
	}
}
