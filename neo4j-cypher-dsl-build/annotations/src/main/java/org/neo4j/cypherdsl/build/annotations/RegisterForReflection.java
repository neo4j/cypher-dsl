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
package org.neo4j.cypherdsl.build.annotations;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used to mark classes to be included in `reflection-config.json` so that it doesn't need updates when new reflective visitors
 * and the like are added.
 * <p>
 * A thin documentation about the file format can be found <a href="https://github.com/oracle/graal/blob/master/docs/reference-manual/native-image/Reflection.md">here</a>.
 * For now, only {@code allDeclaredMethods} is supported.
 * <p>
 * In the future, support can be added for
 * <ul>
 *     <li><code>allPublicConstructors</code></li>
 *     <li><code>allPublicMethods</code></li>
 *     <li><code>allPublicFields</code></li>
 *     <li><code>allDeclaredConstructors</code></li>
 *     <li><code>allDeclaredFields</code></li>
 * </ul>
 *
 * @author Michael J. Simons
 * @soundtrack Ben Foster - Torchwood (Original Television Soundtrack)
 * @since 2022.2.2
 */
@Retention(RetentionPolicy.CLASS)
@Target(ElementType.TYPE)
public @interface RegisterForReflection {

	/**
	 * @return true if all declared methods should be marked for inclusion in a native image.
	 */
	boolean allDeclaredMethods() default true;

	/**
	 * @return true if all declared constructors should be marked for inclusions in a native image.
	 */
	boolean allDeclaredConstructors() default false;
}
