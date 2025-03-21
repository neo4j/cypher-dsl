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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.function.Consumer;
import java.util.function.Function;

import org.apiguardian.api.API;
import org.neo4j.cypherdsl.core.ast.Visitable;
import org.neo4j.cypherdsl.core.ast.Visitor;

/**
 * A  mutable   tree  structure  providing   <a  href="https://en.wikipedia.org/wiki/Breadth-first_search">Breadth-first
 * search</a>         (aka         in-level         order          traversal)         and         pre-ordered         <a
 * href="https://en.wikipedia.org/wiki/Depth-first_search">depth-first search</a>. This class is thread-safe.
 *
 * @author Michael J. Simons
 * @param <E> The type of each node value
 * @soundtrack Black Sabbath - The End: Live in Birmingham
 * @since 2023.2.0
 */
@API(status = API.Status.EXPERIMENTAL, since = "2023.2.0")
public final class TreeNode<E> {

	/**
	 * Creates a  tree from a  {@link Statement}. This  allows to visit all  elements of statement  without implementing
	 * custom {@link Visitor visitors.} The root of the returned tree will always be the statement.
	 *
	 * @param statement The statement that should be represented as a tree
	 * @return A tree with the statement as root
	 */
	public static TreeNode<Visitable> from(Statement statement) {
		var visitor = new TreeBuildingVisitor();
		statement.accept(visitor);
		return visitor.root;
	}

	/**
	 * Creates a new tree, starting at the root.
	 *
	 * @param value The actual value
	 * @param <E>   The type of the value
	 * @return The new node
	 */
	static <E> TreeNode<E> root(E value) {
		return new TreeNode<>(null, 0, value);
	}

	private final TreeNode<E> parent;
	private final int level;
	private final List<TreeNode<E>> children;
	private final E value;

	private TreeNode(TreeNode<E> parent, int level, E value) {
		this.parent = parent;
		this.level = level;
		this.value = value;
		this.children = new ArrayList<>();
	}

	/**
	 * Appends a value to  this node and thus creating a new  child node. This node will be modified  and keeps track of
	 * the child node.
	 *
	 * @param childValue The value of the new child node
	 * @return The new child (this node will be the parent of the new node)
	 */
	TreeNode<E> append(E childValue) {
		var newChild = new TreeNode<>(this, this.level + 1, childValue);
		this.children.add(newChild);
		return newChild;
	}

	/**
	 * @return {@literal true} if this is the root node
	 */
	public boolean isRoot() {
		return this.parent == null;
	}

	/**
	 * @return The level or the height in this tree ({@literal 0} is the level of the root node)
	 */
	public int getLevel() {
		return level;
	}

	/**
	 * @return The parent of this node or {@literal null} if this is a root node.
	 */
	public TreeNode<E> getParent() {
		return parent;
	}

	/**
	 * @return An immutable collection of this nodes children
	 */
	public Collection<TreeNode<E>> getChildren() {
		return List.copyOf(children);
	}

	/**
	 * @return The value of this node.
	 */
	public E getValue() {
		return value;
	}

	/**
	 * @return a breadth-first iterator of this node and it's children
	 */
	public Iterator<TreeNode<E>> breadthFirst() {
		return new BreadthFirstIterator<>(this);
	}

	/**
	 * @return a depth-first, pre-ordered iterator of this node and it's children
	 */
	public Iterator<TreeNode<E>> preOrder() {
		return new PreOrderIterator<>(this);
	}

	/**
	 * Creates an ASCII representation of this node and its children
	 *
	 * @param target   The target to which to print this tree to
	 * @param toString How to format nodes if this type
	 */
	public void printTo(Consumer<CharSequence> target, Function<TreeNode<E>, String> toString) {
		this.printTo0(target, toString, this, "", true);
	}

	private void printTo0(Consumer<CharSequence> target, Function<TreeNode<E>, String> toString, TreeNode<E> node, String prefix, boolean isTail) {

		var localValue = toString.apply(node);
		var connector = isTail ? "└── " : "├── ";
		if (this == node) {
			connector = "";
		}
		target.accept(prefix + connector + localValue + "\n");

		var newPrefix = prefix + (isTail ? " ".repeat(connector.length()) : "│   ");
		for (int i = 0; i < node.children.size(); ++i) {
			var child = node.children.get(i);
			printTo0(target, toString, child, newPrefix, i + 1 == node.getChildren().size());
		}
	}

	private static class TreeBuildingVisitor implements Visitor {

		final Deque<TreeNode<Visitable>> nodes = new ArrayDeque<>();

		TreeNode<Visitable> root;

		@Override
		public void enter(Visitable segment) {
			var currentParent = nodes.peek();
			if (currentParent == null) {
				currentParent = TreeNode.root(segment);
			} else {
				currentParent = currentParent.append(segment);
			}

			nodes.push(currentParent);
		}

		@Override
		public void leave(Visitable segment) {
			root = nodes.pop();
		}
	}

	private static final class BreadthFirstIterator<E> implements Iterator<TreeNode<E>> {

		private final Queue<TreeNode<E>> queue;

		BreadthFirstIterator(TreeNode<E> root) {
			this.queue = new ArrayDeque<>();
			this.queue.add(root);
		}

		@Override
		public boolean hasNext() {
			return !queue.isEmpty();
		}

		@Override
		public TreeNode<E> next() {
			if (queue.isEmpty()) {
				throw new NoSuchElementException();
			}
			var n = queue.remove();
			queue.addAll(n.children);
			return n;
		}
	}

	private static final class PreOrderIterator<E> implements Iterator<TreeNode<E>> {

		private final Deque<Iterator<TreeNode<E>>> stack;

		PreOrderIterator(TreeNode<E> root) {
			this.stack = new ArrayDeque<>();
			this.stack.push(List.of(root).iterator());
		}

		@Override
		public boolean hasNext() {
			return !stack.isEmpty() && stack.peek().hasNext();
		}

		@Override
		public TreeNode<E> next() {
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
}
