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
import org.cp.elements.lang.Visitable;
import org.cp.elements.lang.Visitor;

/**
 * The ComposableVisitor class is a collection of Visitors allowing multiple Visitors to visit a graph of Visitable
 * objects simultaneously.
 *
 * @author John J. Blum
 * @see java.lang.Iterable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.Visitor
 * @since 1.0.0
 */
public class ComposableVisitor implements Iterable<Visitor>, Visitor {

  private final List<Visitor> visitors = new LinkedList<Visitor>();

  /**
   * Adds the specified Visitor to the composition (collection) of Visitors.  The Visitor will be added if and only if
   * the Visitor is not null and is not already a member of this composition.
   *
   * @param visitor the Visitor object to add to this composition.
   * @return a boolean value indicating whether the Visitor was added successfully or not.
   * @throws NullPointerException if the Visitor is null.
   * @see #contains(org.cp.elements.lang.Visitor)
   */
  public boolean add(final Visitor visitor) {
    Assert.notNull(visitor, "The Visitor to add to this composition cannot be null!");
    return (!contains(visitor) && visitors.add(visitor));
  }

  /**
   * Determines whether the specified Visitor is a member of this composition.
   *
   * @param visitor the Visitor object determined for containment in this composition.
   * @return a boolean value indicating whether the specified Visitor is a member of this composition.
   */
  public boolean contains(final Visitor visitor) {
    return visitors.contains(visitor);
  }

  /**
   * Gets an Iterator of the Visitors in this composition.
   *
   * @return an Iterator iterating over the Visitors contained in this composition.
   */
  public Iterator<Visitor> iterator() {
    //return CollectionUtils.unmodifiableIterator(visitors.iterator());
    return Collections.unmodifiableList(visitors).iterator();
  }

  /**
   * Removes the specified Visitor from this composition.
   *
   * @param visitor the Visitor object to remove from this composition.
   * @return a boolean value if the Visitor was actually contained by this composition
   * and was successfully removed.
   */
  public boolean remove(final Visitor visitor) {
    return visitors.remove(visitor);
  }

  /**
   * Gets the number of Visitors in this composition.
   *
   * @return a integer value representing the count of Visitors in this composition.
   */
  public int size() {
    return visitors.size();
  }

  /**
   * Visits the given Visitable object in order to carryout some function or investigation of the targeted object.
   *
   * @param visitable the Visitable object visited by this Visitor.
   */
  public void visit(final Visitable visitable) {
    for (final Visitor visitor : this) {
      visitor.visit(visitable);
    }
  }

}
