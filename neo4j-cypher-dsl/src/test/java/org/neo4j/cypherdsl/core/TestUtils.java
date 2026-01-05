/*
 * Copyright (c) 2019-2026 "Neo4j,"
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * @author Michael J. Simons
 *
 */
final class TestUtils {

	private TestUtils() {
	}

	static Method findMethod(Class<?> clazz, String name, Class<?>... paramTypes) {
		try {
			return clazz.getDeclaredMethod(name, paramTypes);
		}
		catch (NoSuchMethodException ex) {
			throw new RuntimeException(ex);
		}
	}

	static Object invokeMethod(Method method, Object target, Object... args) {
		try {
			return method.invoke(target, args);
		}
		catch (InvocationTargetException ex) {
			Throwable targetException = ex.getTargetException();
			if (targetException instanceof RuntimeException) {
				throw ((RuntimeException) targetException);
			}
			else {
				throw new RuntimeException(targetException);
			}
		}
		catch (Exception ex) {

			throw new RuntimeException(ex);
		}
	}

}
