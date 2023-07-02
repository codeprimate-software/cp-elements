/*
 * Copyright 2011-Present Author or Authors.
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

package org.cp.elements.util.paging;

import static java.util.stream.StreamSupport.stream;

import java.util.Comparator;

/**
 * Abstract Data Type (ADT) modeling an individual page in a {@link Pageable object} and is a collection of elements
 * or items contained on the page.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the elements or items contained in this {@link Page}.
 * @see java.lang.Iterable
 * @see org.cp.elements.util.paging.Pageable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Page<T> extends Iterable<T> {

  /**
   * Gets the page number of this {@link Page} in the sequence (collection) of pages.
   *
   * @return an integer value indicating the page number of this {@link Page}.
   */
  int getNumber();

  /**
   * Determines whether there is a next {@link Page}.
   *
   * @return a boolean value indicating whether there is a next {@link Page}.
   */
  boolean hasNext();

  /**
   * Determines whether there is a previous {@link Page}.
   *
   * @return a boolean value indicating whether there is a previous {@link Page}.
   */
  boolean hasPrevious();

  /**
   * Returns the next {@link Page} in the sequence of {@link Page Pages}.
   *
   * @return the next {@link Page} in the sequence of {@link Page Pages}.
   * @throws PageNotFoundException if there is no next {@link Page}.
   * @see org.cp.elements.util.paging.Page
   */
  Page<T> next();

  /**
   * Returns the previous {@link Page} in the sequence of {@link Page Pages}.
   *
   * @return the previous {@link Page} in the sequence of {@link Page Pages}.
   * @throws PageNotFoundException if there is no previous {@link Page}.
   * @see org.cp.elements.util.paging.Page
   */
  Page<T> previous();

  /**
   * Indicates the number of elements or items contained in this {@link Page}.
   *
   * @return an integer value indicating the number of elements or items contained in this {@link Page}.
   */
  default int size() {
    return (int) stream(this.spliterator(), false).count();
  }

  /**
   * Sorts (orders) only the elements contained in this {@link Page}.
   *
   * @param orderBy {@link Comparator} used to sort (order) only the elements contained in this {@link Page}.
   * @see java.util.Comparator
   */
  void sort(Comparator<T> orderBy);

}
