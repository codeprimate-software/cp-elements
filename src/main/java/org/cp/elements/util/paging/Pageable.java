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
import static org.cp.elements.lang.ElementsExceptionsFactory.newPageNotFoundException;

import java.util.Comparator;

import org.cp.elements.lang.Assert;

/**
 * The {@link Pageable} interface defines a contract for implementing objects that support paging
 * over the contents of the object, such as a {@link Iterable}.
 *
 * @author John J. Blum
 * @param <T> {@link Class type} of the elements or items contained in the {@link Page pages}
 * of this {@link Pageable} object.
 * @see java.lang.Iterable
 * @see org.cp.elements.util.paging.Page
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Pageable<T> extends Iterable<Page<T>> {

  /**
   * Determines whether this {@link Pageable} object contains any {@link Page Pages}.
   *
   * @return a boolean value indicating whether this {@link Pageable} object contains any {@link Page Pages}.
   * @see #count()
   */
  default boolean isEmpty() {
    return count() < 1;
  }

  /**
   * Get the total number of {@link Page pages} in this {@link Pageable} object.
   *
   * @return an int value indicating the total number of {@link Page pages} in this {@link Pageable} object.
   */
  default int count() {
    return (int) stream(this.spliterator(), false).count();
  }

  /**
   * Jump to the first {@link Page} in this {@link Pageable} collection of {@link Page Pages}.
   *
   * @return the first {@link Page}.
   * @throws PageNotFoundException if no {@link Page Pages} are available.
   * @see #getPage(int)
   */
  default Page<T> firstPage() {

    try {
      return getPage(1);
    }
    catch (Exception cause) {
      throw newPageNotFoundException(cause, "No first page");
    }
  }

  /**
   * Gets the {@link Page} for the given page {@link Integer#TYPE number}, starting with page one.
   *
   * @param number integer value indicating the page number of the {@link Page} to return.
   * @return the {@link Page} with {@link Integer#TYPE number}.
   * @throws IllegalArgumentException if the {@link Integer#TYPE page number} is less than equal to 0.
   * @throws PageNotFoundException if a {@link Page} with {@link Integer#TYPE number} is not found.
   * @see org.cp.elements.util.paging.Page
   */
  default Page<T> getPage(int number) {

    Assert.isTrue(number > 0, "Page number [%d] must be greater than 0", number);

    int count = 0;

    for (Page<T> page : this) {
      if (++count == number) {
        return page;
      }
    }

    throw newPageNotFoundException("Page with number [%d] not found", number);
  }

  /**
   * Jump to the last {@link Page} in this {@link Pageable} collection of {@link Page Pages}.
   *
   * @return the last {@link Page}.
   * @throws PageNotFoundException if no {@link Page Pages} are available.
   * @see #getPage(int)
   */
  default Page<T> lastPage() {

    try {
      return getPage(count());
    }
    catch (Exception cause) {
      throw newPageNotFoundException(cause, "No last page");
    }
  }

  /**
   * Sorts (orders) all the elements across all the {@link Page Pages} contained by this {@link Pageable}.
   *
   * @param orderBy {@link Comparator} used to sort (order) all the elements across all the {@link Page Pages}
   * contained by this {@link Pageable}.
   * @see java.util.Comparator
   */
  void sort(Comparator<T> orderBy);

}
