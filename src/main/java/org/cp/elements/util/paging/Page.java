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

package org.cp.elements.util.paging;

import static java.util.stream.StreamSupport.stream;

import java.util.Comparator;

/**
 * The {@link Page} interface defines an Abstract Data Type (ADT) modeling an individual page
 * in a {@link Pageable} object and is a collection of elements or items contained on the page.
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
