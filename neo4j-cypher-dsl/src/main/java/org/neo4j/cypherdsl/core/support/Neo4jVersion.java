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
package org.neo4j.cypherdsl.core.support;

import static org.apiguardian.api.API.Status.EXPERIMENTAL;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apiguardian.api.API;

/**
 * This annotation provides information which Neo4j version is required to be able to successfully run a query containing
 * a fragment generated via a method annotated with it.
 *
 * @author Michael J. Simons
 * @soundtrack Die Ärzte - Seitenhirsch
 * @since 2020.1.2
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD, ElementType.TYPE })
@Documented
@API(status = EXPERIMENTAL, since = "2020.1.2")
public @interface Neo4jVersion {

	/**
	 * @return The minimum version of Neo4j required to run the annotated construct.
	 */
	String minimum();

	/**
	 * @return The last version of Neo4j that supports running the annotated construct.
	 */
	String last() default "";
}
