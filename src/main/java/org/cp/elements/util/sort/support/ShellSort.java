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
 * The ShellSort class is an implementation of the Shell Sort algorithm and Sorter interface.
 * <p/>
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.util.sort.AbstractSorter
 * @link http://en.wikipedia.org/wiki/Shellsort
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ShellSort extends AbstractSorter {

  /**
   * Calculates the gap for the Shell Sort algorithm based on the size of the List of elements.
   * <p/>
   * @param elements the List of elements to base the gap on.
   * @return an integer value defining the gap to use in the Shell Sort algorithm.
   * @see java.util.List
   */
  protected int getGap(final List elements) {
    return (elements.size() / 3);
  }

  /**
   * Uses the Shell (Insertion) Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    for (int gap = getGap(elements); gap > 0; gap /= 2) {
      for (int i = gap, size = elements.size(); i < size; i++) {
        E elementAtI = elements.get(i);
        int j = i;

        for ( ; j >= gap && getOrderBy().compare(elements.get(j - gap), elementAtI) > 0; j -= gap) {
          elements.set(j, elements.get(j - gap));
        }

        elements.set(j, elementAtI);
      }
    }

    return elements;
  }

}
