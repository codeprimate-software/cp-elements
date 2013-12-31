/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
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
 * <p/>
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
   * <p/>
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
   * <p/>
   * @param visitor the Visitor object determined for containment in this composition.
   * @return a boolean value indicating whether the specified Visitor is a member of this composition.
   */
  public boolean contains(final Visitor visitor) {
    return visitors.contains(visitor);
  }

  /**
   * Gets an Iterator of the Visitors in this composition.
   * <p/>
   * @return an Iterator iterating over the Visitors contained in this composition.
   */
  public Iterator<Visitor> iterator() {
    //return CollectionUtils.unmodifiableIterator(visitors.iterator());
    return Collections.unmodifiableList(visitors).iterator();
  }

  /**
   * Removes the specified Visitor from this composition.
   * <p/>
   * @param visitor the Visitor object to remove from this composition.
   * @return a boolean value if the Visitor was actually contained by this composition
   * and was successfully removed.
   */
  public boolean remove(final Visitor visitor) {
    return visitors.remove(visitor);
  }

  /**
   * Gets the number of Visitors in this composition.
   * <p/>
   * @return a integer value representing the count of Visitors in this composition.
   */
  public int size() {
    return visitors.size();
  }

  /**
   * Visits the given Visitable object in order to carryout some function or investigation of the targeted object.
   * <p/>
   * @param visitable the Visitable object visited by this Visitor.
   */
  public void visit(final Visitable visitable) {
    for (final Visitor visitor : this) {
      visitor.visit(visitable);
    }
  }

}
