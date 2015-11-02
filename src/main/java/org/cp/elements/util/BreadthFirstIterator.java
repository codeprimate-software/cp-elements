/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.NoSuchElementException;
import java.util.concurrent.atomic.AtomicBoolean;

import org.cp.elements.lang.Assert;

/**
 * The BreadthFirstIterator class is an Iterator implementation and facade for an Iterator of Iterators
 * iterating elements in row order, left to right, top to bottom.
 *
 * @author John Blum
 * @see java.util.Iterator
 * @see org.cp.elements.util.DepthFirstIterator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BreadthFirstIterator<T> implements Iterator<T> {

  private AtomicBoolean nextCalled = new AtomicBoolean(false);

  private final Deque<Iterator<T>> iterators = new LinkedList<>();

  /**
   * Constructs an instance of the BreadthFirstIterator class wrapping the Iterator of Iterators to collectively
   * traverse the Iterators in order, left to right, top to bottom.
   *
   * @param iterators the Iterator of Iterators to encapsulate in a breadth-first traversal.
   * @throws java.lang.NullPointerException if the Iterator of Iterators reference is null.
   * @see java.util.Iterator
   */
  public BreadthFirstIterator(final Iterator<Iterator<T>> iterators) {
    Assert.notNull(iterators, "The Iterator of Iterators must not be null!");

    for (Iterator<T> iterator : CollectionUtils.iterable(iterators)) {
      if (iterator != null) {
        this.iterators.add(iterator);
      }
    }
  }

  /**
   * Determines whether another element is available in the iteration.
   *
   * @return a boolean value indicating whether another element is available in the iteration.
   * @see java.util.Iterator#hasNext()
   */
  @Override
  public boolean hasNext() {
    while (!(iterators.isEmpty() || iterators.peek().hasNext())) {
      Assert.isFalse(iterators.removeFirst().hasNext(), new IllegalStateException("removing a non-empty Iterator"));
    }

    return (!iterators.isEmpty() && iterators.peek().hasNext());
  }

  /**
   * Gets the next element in the iteration.
   *
   * @return the next element in the iteration.
   * @throws java.util.NoSuchElementException if the iteration has exhausted all elements.
   * @see java.util.Iterator#next()
   * @see #hasNext()
   */
  @Override
  public T next() {
    Assert.isTrue(hasNext(), new NoSuchElementException("The iteration has no more elements!"));
    T value = iterators.peek().next();
    nextCalled.set(true);
    return value;
  }

  /**
   * Removes the current element in the iteration.
   *
   * @throws java.lang.IllegalStateException if the next method has not yet been called, or the remove method
   * has already been called after the last call to the next method.
   * @see java.util.Iterator#remove()
   */
  @Override
  public void remove() {
    Assert.state(nextCalled.compareAndSet(true, false), "next was not called before remove");
    iterators.peek().remove();
  }

}
