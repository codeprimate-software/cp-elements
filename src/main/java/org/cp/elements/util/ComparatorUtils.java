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

package org.cp.elements.util;

import java.util.Comparator;

/**
 * The ComparatorUtils class provides common functionality for ordering operations using the Comparable
 * and Comparator interfaces.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ComparatorUtils {

  /**
   * Compares two Comparable objects for absolute ordering, ignoring possible null values.  Null values are
   * ordered last.
   * <p/>
   * @param obj1 the Comparable object being compared with obj2.
   * @param obj2 the Comparable object being compared with obj1.
   * @param <T> the type of Comparable objects in the comparison.
   * @return a integer value indicating the absolute ordering of the Comparable objects as defined by
   * their natural ordering.
   * @see java.lang.Comparable
   */
  public static <T extends Comparable<T>> int compareIgnoreNull(final T obj1, final T obj2) {
    return (obj1 == null ? 1 : (obj2 == null ? -1 : obj1.compareTo(obj2)));
  }

  /**
   * Inverts the result of the Comparator.  Used to implement a descending sort order.
   * <p/>
   * @param comparator the Comparator to invert.
   * @param <T> the type of Comparable objects in the comparison.
   * @return a Comparator wrapper around the specified Comparator inverting the result of the comparison.
   * @see java.util.Comparator
   */
  public static <T> Comparator<T> invert(final Comparator<T> comparator) {
    return new Comparator<T>() {
      @Override public int compare(final T obj1, final T obj2) {
        return (-1 * comparator.compare(obj1, obj2));
      }
    };
  }

}
