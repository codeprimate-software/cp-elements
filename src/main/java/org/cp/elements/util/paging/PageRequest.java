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

import org.cp.elements.lang.Orderable;

/**
 * The {@link PageRequest} class models a request in a pageable (paging) operation
 * that indicates the starting {@link Page} as well as the number of elements or items
 * per {@link Page}.
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface PageRequest<T extends Comparable<T>> extends Orderable<T> {

  /**
   * Returns an {@link Integer#TYPE integer value} indicating the number of {@link Page pages} to prefetch.
   *
   * @return an {@link Integer#TYPE integer value} indicating the number of {@link Page pages} to prefetch.
   */
  int getFetchSize();

  /**
   * Gets the {@link Integer#TYPE requested size} for the {@link Page pages} returned in the {@link Pageable}.
   *
   * @return an integer value indicated the desired number of elements or items per {@link Page}.
   */
  int getPageSize();

  /**
   * Gets the requested starting {@link Page} within the {@link Pageable}.
   *
   * @return an integer value indicating the {@link Page} number to start at in the {@link Pageable}.
   */
  int getStartingPageNumber();

}
