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
   * Get the total number of {@link Page pages} in this {@link Pageable} object.
   *
   * @return an integer value indicating the total number of {@link Page pages} in this {@link Pageable} object.
   */
  int count();

  /**
   * Gets the {@link Page} for the given page {@link Integer#TYPE number}, starting with page one.
   *
   * @param number integer value indicating the page number of the {@link Page} to return.
   * @return the {@link Page} with {@link Integer#TYPE number}.
   * @throws PageNotFoundException if a {@link Page} with {@link Integer#TYPE number} is not found.
   * @see org.cp.elements.util.paging.Page
   */
  Page<T> getPage(int number);

}
