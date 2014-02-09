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

package org.cp.elements.util.sort.support;

import java.util.List;

import org.cp.elements.util.sort.AbstractSorter;

/**
 * The CombSort class is an implementation of the Comb Sort algorithm and Sorter interface.
 * <p/>
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.util.sort.AbstractSorter
 * @link http://en.wikipedia.org/wiki/Comb_sort
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CombSort extends AbstractSorter {

  protected static final double SHRINK = 1.3d;

  /**
   * Uses the Comb (Bubble) Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    boolean swapped = true;

    for (int size = elements.size(), gap = size; gap > 0 && swapped; ) {
      gap = Math.max(1, (int) Math.floor(gap / SHRINK));
      swapped = false;

      for (int index = 0; index + gap < size; index++) {
        if (getOrderBy().compare(elements.get(index), elements.get(index + gap)) > 0) {
          swap(elements, index, index + gap);
          swapped = true;
        }
      }
    }

    return elements;
  }

}
