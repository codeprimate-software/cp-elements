/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.cp.elements.lang.support;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Composite;
import org.cp.elements.lang.NullSafe;
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The {@link ComposableVisitor} class is a collection of {@link Visitor} objects enabling multiple
 * {@link Visitor Visitors} to visit a graph of {@link Visitable} objects simultaneously.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see java.util.Iterator
 * @see org.cp.elements.lang.Composite
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
public class ComposableVisitor implements Composite<Visitor>, Iterable<Visitor>, Visitor {

  private final List<Visitor> visitors = new LinkedList<>();

  /**
   * Adds the specified Visitor to the composition (collection) of Visitors.  The Visitor will be added if and only if
   * the Visitor is not null and is not already a member of this composition.
   *
   * @param visitor the Visitor object to add to this composition.
   * @return a boolean value indicating whether the Visitor was added successfully or not.
   * @throws NullPointerException if the Visitor is null.
   * @see #contains(org.cp.elements.lang.Visitor)
   */
  public boolean add(Visitor visitor) {
    Assert.notNull(visitor, "The Visitor to add to this composite cannot be null");
    return (visitor != this && !contains(visitor) && visitors.add(visitor));
  }

  /**
   * @inheritDoc
   */
  @NullSafe
  @Override
  public Visitor compose(Visitor one, Visitor two) {
    if (one != null) {
      add(one);
    }

    if (two != null) {
      add(two);
    }

    return this;
  }

  /**
   * Determines whether the specified Visitor is a member of this composition.
   *
   * @param visitor the Visitor object determined for containment in this composition.
   * @return a boolean value indicating whether the specified Visitor is a member of this composition.
   */
  public boolean contains(Visitor visitor) {
    return this.visitors.contains(visitor);
  }

  /**
   * Determines whether this {@link ComposableVisitor} contains any {@link Visitor Visitors}.
   *
   * @return a boolean value indicating whether this {@link ComposableVisitor} contains any {@link Visitor Visitors}.
   */
  public boolean isEmpty() {
    return this.visitors.isEmpty();
  }

  /**
   * Gets an Iterator of the Visitors in this composition.
   *
   * @return an Iterator iterating over the Visitors contained in this composition.
   */
  public Iterator<Visitor> iterator() {
    return Collections.unmodifiableList(this.visitors).iterator();
  }

  /**
   * Removes the specified Visitor from this composition.
   *
   * @param visitor the Visitor object to remove from this composition.
   * @return a boolean value if the Visitor was actually contained by this composition
   * and was successfully removed.
   */
  public boolean remove(Visitor visitor) {
    return this.visitors.remove(visitor);
  }

  /**
   * Returns the number of {@link Visitor Visitors} in this composite.
   *
   * @return a integer value indicating the number of {@link Visitor Visitors} in this composite.
   */
  public int size() {
    return this.visitors.size();
  }

  /**
   * Visits the given Visitable object in order to carryout some function or investigation of the targeted object.
   *
   * @param visitable the Visitable object visited by this Visitor.
   */
  public void visit(Visitable visitable) {
    for (Visitor visitor : this) {
      visitor.visit(visitable);
    }
  }
}
