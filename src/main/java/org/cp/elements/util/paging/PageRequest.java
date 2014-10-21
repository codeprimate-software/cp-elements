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
 * The PageRequest class...
 *
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface PageRequest<T extends Comparable<T>> extends Orderable<T> {

  /**
   * Gets the requested size for the pages returned in the pageable.
   *
   * @return an integer value indicated the requested page size.
   */
  int getPageSize();

  /**
   * Gets the requested starting page within the pageable.
   *
   * @return an integer value indicating the page number to start at in the pageable.
   */
  int getStartingPageNumber();

}
