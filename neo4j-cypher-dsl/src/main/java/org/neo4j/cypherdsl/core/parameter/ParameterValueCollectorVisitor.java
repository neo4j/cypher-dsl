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
package org.neo4j.cypherdsl.core.parameter;

import org.neo4j.cypherdsl.core.Parameter;
import org.neo4j.cypherdsl.core.support.ReflectiveVisitor;
import org.neo4j.cypherdsl.core.support.Visitable;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.TreeMap;


/**
 * This is a simple implementation of a visitor to the Cypher AST created by the Cypher builder
 * based on the {@link ReflectiveVisitor reflective visitor}.
 * <p>
 * It collects all parameters with bound value into a map of used parameters
 * <p>
 *
 * @author Andreas Berger
 */
class ParameterValueCollectorVisitor extends ReflectiveVisitor {
    private final Map<String, Object> parameterValues = new TreeMap<>();
    private Map<String, Set<Object>> errors;

    @Override
    protected boolean preEnter(Visitable visitable) {
        return true;
    }

    @Override
    protected void postLeave(Visitable visitable) {
    }

    @SuppressWarnings("unused")
    void enter(Parameter parameter) {
        if (!parameter.hasBoundValue()) {
            return;
        }
        boolean containsKey = parameterValues.containsKey(parameter.getName());
        Object boundValue = parameter.getBoundValue();
        Object storedValue = parameterValues.put(parameter.getName(), boundValue);

        if (containsKey && !Objects.equals(storedValue, boundValue)) {
            if (errors == null) {
                errors = new TreeMap<>();
            }
            Set<Object> conflictingObjects = errors.computeIfAbsent(parameter.getName(), s -> {
                HashSet<Object> list = new HashSet<>();
                list.add(storedValue);
                return list;
            });
            conflictingObjects.add(boundValue);
        }
    }

    public Map<String, Object> getParameterValues() {
        if (errors != null && !errors.isEmpty()) {
            throw new ConflictingParametersException(errors);
        }
        return parameterValues;
    }

}
