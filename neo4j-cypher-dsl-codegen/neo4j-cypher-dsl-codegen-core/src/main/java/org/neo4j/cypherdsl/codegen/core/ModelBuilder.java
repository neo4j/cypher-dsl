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

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;

import org.apiguardian.api.API;

/**
 * Shared interface for model builder. This interface and the concrete classes implementing it is considered to be public API.
 *
 * @author Michael J. Simons
 * @param <SELF> A type representing the concrete model builder itself
 * @soundtrack Queen - Queen Forever
 * @since 2021.1.0
 */
@API(status = EXPERIMENTAL, since = "2021.1.0")
public interface ModelBuilder<SELF extends ModelBuilder<?>> {

	/**
	 * Adds a single new property to this model.
	 *
	 * @param newProperty The new property
	 * @return This builder
	 * @throws IllegalStateException When this builder has already been used to create Java class.
	 */
	SELF addProperty(String newProperty);

	/**
	 * Adds a single new property to this model.
	 *
	 * @param newProperty The new property
	 * @return This builder
	 * @throws IllegalStateException When this builder has already been used to create Java class.
	 */
	SELF addProperty(PropertyDefinition newProperty);

	/**
	 * Adds a collection of properties to this model.
	 *
	 * @param newProperties A new collection of properties
	 * @return This builder
	 * @throws IllegalStateException When this builder has already been used to create Java class.
	 */
	SELF addProperties(Collection<PropertyDefinition> newProperties);

	/**
	 * @return The qualified package name if any and an empty string for the default package.
	 */
	String getPackageName();

	/**
	 * @return The canonical class name (as in {@link Class#getCanonicalName()}.
	 */
	String getCanonicalClassName();

	/**
	 * @return The simple class name without any pre- or suffix.
	 */
	String getPlainClassName();

	/**
	 * Triggers the creation of the final model and writes it as a Java class file to the given path. It is safe to call
	 * this method several times or any of the other {@code writeTo} methods, but changing the builder after this method
	 * has been called will lead to an {@link IllegalStateException}.
	 *
	 * @param path The path to write this model to.
	 * @throws java.io.UncheckedIOException in case IO fails, any {@link IOException} is wrapped and rethrown.
	 */
	void writeTo(Path path);

	/**
	 * Triggers the creation of the final model and returns a Java class definition as string. It is safe to call
	 * this method several times or any of the other {@code writeTo} methods, but changing the builder after this method
	 * has been called will lead to an {@link IllegalStateException}.
	 *
	 * @return The generated class as a string
	 * @throws java.io.UncheckedIOException in case IO fails, any {@link IOException} is wrapped and rethrown.
	 */
	String writeToString();

	/**
	 * Triggers the creation of the final model and appends it to the given {@link Appendable} as Java class.
	 * It is safe to call this method several times or any of the other {@code writeTo} methods, but changing the builder
	 * after this method has been called will lead to an {@link IllegalStateException}.
	 * <p>
	 * An appendable that is also closable will still be open after passing it to this method.
	 *
	 * @param appendable To where the generated source could should be appended to
	 * @throws java.io.UncheckedIOException in case IO fails, any {@link IOException} is wrapped and rethrown.
	 */
	void writeTo(Appendable appendable);

	/**
	 * @return A field name that is hopefully suitable for storing an instance of the class generated by this builder.
	 */
	String getFieldName();
}
