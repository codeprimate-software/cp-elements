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

import org.cp.elements.lang.Orderable;

/**
 * The OrderableComparator class is an implementation of the Comparator interface to compare Orderable objects.
 * <p/>
 * @author John J. Blum
 * @param <T> a type parameter indicating the class of the Orderable type.
 * @see java.util.Comparator
 * @see org.cp.elements.lang.Orderable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class OrderableComparator<T extends Comparable<T>> implements Comparator<Orderable<T>> {

  /**
   * Compares two Orderable objects to determine their relative order by their order property.
   * <p/>
   * @param orderable1 the first Orderable object in the order comparison.
   * @param orderable2 the second Orderable object in the order comparison.
   * @return an integer value indicating one Orderable object's order relative to another Orderable object.
   */
  @Override
  public int compare(final Orderable<T> orderable1, final Orderable<T> orderable2) {
    return orderable1.getOrder().compareTo(orderable2.getOrder());
  }

}
