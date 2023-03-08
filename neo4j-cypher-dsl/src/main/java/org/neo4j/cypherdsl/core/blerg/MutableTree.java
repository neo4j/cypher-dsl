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
package org.neo4j.cypherdsl.core.blerg;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * A  mutable   tree  structure  providing   <a  href="https://en.wikipedia.org/wiki/Breadth-first_search">Breadth-first
 * search</a>         (aka         in-level         order          traversal)         and         pre-ordered         <a
 * href="https://en.wikipedia.org/wiki/Depth-first_search">depth-first search</a>. This class is thread-safe.
 *
 * @param <E> The type of each node value
 * @author Michael J. Simons
 * @soundtrack Black Sabbath - The End: Live in Birmingham
 * @since TBA
 */
public final class MutableTree<E> {

	/**
	 * Creates a new tree, starting at the root.
	 *
	 * @param value The actual value
	 * @param <E>   The type of the value
	 * @return The new node
	 */
	static <E> MutableTree<E> root(E value) {
		return new MutableTree<>(null, 0, value);
	}

	private final MutableTree<E> parent;
	private final int level;
	private final Collection<MutableTree<E>> children;
	private final E value;

	private MutableTree(MutableTree<E> parent, int level, E value) {
		this.parent = parent;
		this.level = level;
		this.value = value;
		this.children = new ConcurrentLinkedQueue<>();
	}

	/**
	 * Appends a value to  this node and thus creating a new  child node. This node will be modified  and keeps track of
	 * the child node.
	 *
	 * @param value The value of the new child node
	 * @return The new child (this node will be the parent of the new node)
	 */
	public MutableTree<E> append(E value) {
		var newChild = new MutableTree<>(this, this.level + 1, value);
		this.children.add(newChild);
		return newChild;
	}

	/**
	 * @return The parent of this node or {@literal null} if this is a root node.
	 */
	public MutableTree<E> getParent() {
		return parent;
	}

	/**
	 * @return The value of this node.
	 */
	public E value() {
		return value;
	}

	private static final class BreadthFirstIterator<E> implements Iterator<MutableTree<E>> {

		private final Queue<MutableTree<E>> queue;

		BreadthFirstIterator(MutableTree<E> root) {
			this.queue = new ArrayDeque<>();
			this.queue.add(root);
		}

		@Override
		public boolean hasNext() {
			return !queue.isEmpty();
		}

		@Override
		public MutableTree<E> next() {
			if (queue.isEmpty()) {
				throw new NoSuchElementException();
			}
			var n = queue.remove();
			queue.addAll(n.children);
			return n;
		}
	}

	public Iterator<MutableTree<E>> breadthFirst() {
		return new BreadthFirstIterator<>(this);
	}

	public Iterator<MutableTree<E>> bfs() {
		Queue<MutableTree<E>> q = new ArrayDeque<>();
		q.add(this);
		var currentLevel = 0;
		while (!q.isEmpty()) {
			var n = q.remove();
			q.addAll(n.children);

			if (currentLevel != n.level) {
				System.out.println();
				currentLevel = n.level;
			}
			System.out.print(n.value + "(" + n.level + ") ");
		}
		System.out.println();
		return null;
	}

	private static final class PreOrderIterator<E> implements Iterator<MutableTree<E>> {

		private final Deque<Iterator<MutableTree<E>>> stack;

		public PreOrderIterator(MutableTree<E> root) {
			this.stack = new ArrayDeque<>();
			this.stack.push(List.of(root).iterator());
		}

		@Override
		public boolean hasNext() {
			return !stack.isEmpty() && stack.peek().hasNext();
		}

		@Override
		public MutableTree<E> next() {
			if (stack.isEmpty()) {
				throw new NoSuchElementException();
			}
			var nodesUpNext = stack.peek();
			var currentNode = nodesUpNext.next();

			if (!nodesUpNext.hasNext()) {
				stack.pop();
			}

			if (!currentNode.children.isEmpty()) {
				stack.push(currentNode.children.iterator());
			}

			return currentNode;
		}
	}

	public Iterator<MutableTree<E>> preOrder() {
		return new PreOrderIterator<>(this);
	}


	public void dfs() {

		Deque<Iterator<MutableTree<E>>> s = new ArrayDeque<>();
		s.push(List.of(this).iterator());
		while (!s.isEmpty()) {
			var f = s.peek();
			var n = f.next();


			if (!f.hasNext()) {
				s.pop();
			}
			if (!n.children.isEmpty()) {
				s.push(n.children.iterator());
			}
			System.out.println(n.value + " (" + n.level + ")");
		}
	}

	@Override
	public String toString() {
		return "MutableTree{" +
		       "level=" + level +
		       ", value=" + value +
		       '}';
	}
}
