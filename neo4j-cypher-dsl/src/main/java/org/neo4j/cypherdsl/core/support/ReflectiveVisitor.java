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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import org.neo4j.cypherdsl.core.AliasedExpression;

/**
 * This is a convenience class implementing a {@link Visitor} and it takes care of choosing the right methods
 * to dispatch the {@link Visitor#enter(Visitable)} and {@link Visitor#leave(Visitable)} calls to.
 * <p>
 * Classes extending this visitor need to provide corresponding {@code enter} and {@code leave} methods taking exactly
 * one argument of the type of {@link Visitable} they are interested it.
 * <p>
 * The type must be an exact match, this support class doesn't try to find a close match up in the class hierarchy if it
 * doesn't find an exact match.
 *
 * @author Michael J. Simons
 * @author Gerrit Meier
 * @since 1.0
 */
public abstract class ReflectiveVisitor implements Visitor {

	/**
	 * Private enum to specify a visiting phase.
	 */
	private enum Phase {
		ENTER("enter"),
		LEAVE("leave");

		final String methodName;

		Phase(String methodName) {
			this.methodName = methodName;
		}
	}

	/**
	 * A shared cache of unbound methods for entering and leaving phases.
	 * The key is the concrete class of the visitor as well as the hierarchy of the visitable.
	 */
	private static final Map<TargetAndPhase, Optional<Method>> VISITING_METHODS_CACHE = new ConcurrentHashMap<>();

	/**
	 * Keeps track of the ASTs current level.
	 */
	private Deque<Visitable> currentVisitedElements = new LinkedList<>();

	/**
	 * A map of aliased visitables that already have been seen and for which an alias must be used on each following
	 * appearance.
	 */
	protected final Map<Visitable, AliasedExpression> visitableToAliased = new HashMap<>();

	/**
	 * This is a hook that is called with the uncasted, raw visitable just before entering a visitable.
	 * <p>
	 * The hook is called regardless wither a matching {@code enter} is found or not.
	 *
	 * @param visitable The visitable that is passed on to a matching enter after this call.
	 * @return true, when visiting of elements should be stopped until this element is left again.
	 */
	protected abstract boolean preEnter(Visitable visitable);

	/**
	 * This is a hook that is called with the uncasted, raw visitable just after leaving the visitable.
	 * <p>
	 * The hook is called regardless wither a matching {@code leave} is found or not.
	 *
	 * @param visitable The visitable that is passed on to a matching leave after this call.
	 */
	protected abstract void postLeave(Visitable visitable);

	private Visitable getAliasedIfSeen(Visitable visitable) {
		return visitableToAliased.containsKey(visitable) ? visitableToAliased.get(visitable) : visitable;
	}

	@Override
	public final void enter(Visitable visitable) {

		Visitable used = getAliasedIfSeen(visitable);
		if (preEnter(used)) {
			currentVisitedElements.push(used);
			executeConcreteMethodIn(new TargetAndPhase(this, used.getClass(), Phase.ENTER), used);
		}
	}

	@Override
	public final void leave(Visitable visitable) {

		Visitable used = getAliasedIfSeen(visitable);
		if (currentVisitedElements.peek() == used) {
			executeConcreteMethodIn(new TargetAndPhase(this, used.getClass(), Phase.LEAVE), used);
			postLeave(used);
			currentVisitedElements.pop();
		}

		if (visitable instanceof AliasedExpression) {
			AliasedExpression aliasedExpression = (AliasedExpression) visitable;
			visitableToAliased.put((aliasedExpression).getDelegate(), aliasedExpression);
		}
	}

	private void executeConcreteMethodIn(TargetAndPhase targetAndPhase, Visitable onVisitable) {
		Optional<Method> optionalMethod = VISITING_METHODS_CACHE
			.computeIfAbsent(targetAndPhase, ReflectiveVisitor::findHandleFor);
		optionalMethod.ifPresent(handle -> {
			try {
				handle.invoke(this, onVisitable);
			} catch (Throwable throwable) {
				throwable.printStackTrace();
			}
		});
	}

	private static Optional<Method> findHandleFor(TargetAndPhase targetAndPhase) {

		for (Class<?> clazz : targetAndPhase.classHierarchyOfVisitable) {
			try {
				Method method = targetAndPhase.visitorClass
					.getDeclaredMethod(targetAndPhase.phase.methodName, clazz);
				method.setAccessible(true);
				return Optional.of(method);
			} catch (NoSuchMethodException e) {
				// We don't do anything if the method doesn't exists
				// Try the next parameter type in the hierarchy
			}
		}
		return Optional.empty();
	}

	private static class TargetAndPhase {
		private final Class<? extends ReflectiveVisitor> visitorClass;

		private final List<Class<?>> classHierarchyOfVisitable;

		private final Phase phase;

		<T extends ReflectiveVisitor> TargetAndPhase(T visitor, Class<? extends Visitable> concreteVisitableClass,
			Phase phase) {
			this.visitorClass = visitor.getClass();
			this.phase = phase;
			this.classHierarchyOfVisitable = new ArrayList<>();

			Class<?> classOfVisitable = concreteVisitableClass;
			do {
				this.classHierarchyOfVisitable.add(classOfVisitable);
				classOfVisitable = classOfVisitable.getSuperclass();
			} while (classOfVisitable != null);
		}

		@Override public boolean equals(Object o) {
			if (this == o) {
				return true;
			}
			if (!(o instanceof TargetAndPhase)) {
				return false;
			}
			TargetAndPhase that = (TargetAndPhase) o;
			return visitorClass.equals(that.visitorClass) &&
				classHierarchyOfVisitable.equals(that.classHierarchyOfVisitable) &&
				phase == that.phase;
		}

		@Override
		public int hashCode() {
			return Objects.hash(visitorClass, classHierarchyOfVisitable, phase);
		}
	}
}
