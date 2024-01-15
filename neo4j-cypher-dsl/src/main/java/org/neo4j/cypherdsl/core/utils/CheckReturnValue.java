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
package org.neo4j.cypherdsl.core.utils;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apiguardian.api.API;
import org.jetbrains.annotations.Contract;

/**
 * Simple version of the JSR 305 annotation that allows detecting accidentally omitted calls to
 * {@link org.neo4j.cypherdsl.core.Cypher#match(org.neo4j.cypherdsl.core.PatternElement...)} ()} and the likes in IntelliJ.
 * <p>
 * This annotation is {@link org.apiguardian.api.API.Status#INTERNAL}. Clients should not rely on its presence.
 * The annotation may be replaced with a more suitable one by JetBrains, e.g.
 * the {@link Contract} annotation, when a favourable use-case can be found.
 *
 * @author Lukas Eder
 * @author Michael J. Simons
 * @see <a href="https://github.com/jOOQ/jOOQ/issues/11718">https://github.com/jOOQ/jOOQ/issues/11718</a>
 * @see <a href="https://youtrack.jetbrains.com/issue/IDEA-265263">https://youtrack.jetbrains.com/issue/IDEA-265263</a>
 */
@Documented
@Target(ElementType.METHOD)
@Retention(RetentionPolicy.CLASS)
@API(status = API.Status.INTERNAL, since = "2021.1.1")
public @interface CheckReturnValue {
}
