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
 * The Page interface defines an individual page in a Pageable object and is a collection of elements or items contained
 * on the page.
 *
 * @author John J. Blum
 * @param <T> the Class type of the elements or items on this Page.
 * @see java.lang.Iterable
 * @see org.cp.elements.util.paging.Pageable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Page<T> extends Iterable<T> {

  /**
   * Gets the page number in the sequence (collection) of pages.
   *
   * @return an integer value indicating the page number of this Page.
   */
  int getNumber();

  /**
   * Indicates the number of elements or items on this page.
   *
   * @return an integer value indicating the number of elements or items on this page.
   */
  int size();

}
