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

import java.util.Comparator;

import org.cp.elements.lang.Ordered;

/**
 * The OrderedComparator class is an implementation of the Comparator interface for Ordered objects.
 * <p/>
 * @author John J. Blum
 * @see java.util.Comparator
 * @see org.cp.elements.lang.Ordered
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class OrderedComparator implements Comparator<Ordered> {

  /**
   * Compares two Ordered objects to determine their relative order by index.
   * <p/>
   * @param ordered1 the first Ordered object in the order comparison.
   * @param ordered2 the second Ordered object in the order comparison.
   * @return an integer value indicating one Ordered object's order relative to another Ordered object.
   */
  @Override
  public int compare(final Ordered ordered1, final Ordered ordered2) {
    return (ordered1.getIndex() < ordered2.getIndex() ? -1 : (ordered1.getIndex() > ordered2.getIndex() ? 1 : 0));
  }

}
