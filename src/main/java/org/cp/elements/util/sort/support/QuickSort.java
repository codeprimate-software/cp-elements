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

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.sort.AbstractSorter;
import org.cp.elements.util.sort.Sorter;

/**
 * The QuickSort class is an implementation of the Quick Sort algorithm and the Sorter interface.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.support.InsertionSort
 * @link http://en.wikipedia.org/wiki/Quicksort
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class QuickSort extends AbstractSorter {

  protected static final int DEFAULT_SIZE_THRESHOLD = 7;

  protected static final Sorter DEFAULT_SORTER = new InsertionSort();

  private int sizeThreshold = DEFAULT_SIZE_THRESHOLD;

  private Sorter sorter = DEFAULT_SORTER;

  /**
   * Gets the size threshold used to stop the recursion of the Quick Sort and resort to another sorting algorithm
   * as determined by the Sorter property.
   * <p/>
   * @return an integer value indicating the size threshold in terms of the remaining unsorted elements from the
   * collection to use a non-recursive sorting algorithm on.  Returns the DEFAULT_THRESHOLD value if the user
   * specified size threshold is less than the default.
   * @see #DEFAULT_SIZE_THRESHOLD
   * @see #setSizeThreshold(int)
   * @see #getSorter()
   */
  public int getSizeThreshold() {
    return Math.max(sizeThreshold, DEFAULT_SIZE_THRESHOLD);
  }

  /**
   * Sets the size threshold used to stop the recursion of the Quick Sort and resort to another sorting algorithm
   * as determined by the Sorter property.
   * <p/>
   * @param sizeThreshold an integer value indicating the size threshold in terms of the remaining unsorted elements
   * from the collection to use a non-recursive sorting algorithm on.
   * @see #getSizeThreshold()
   */
  public void setSizeThreshold(final int sizeThreshold) {
    this.sizeThreshold = sizeThreshold;
  }

  /**
   * Gets the Sorter implementation to use when the remaining elements from the collection to sort using the Quick Sort
   * algorithm is less than equal to the size threshold.
   * <p/>
   * @return the alternate sorting algorithm implementation, or the default sorting algorithm when the user defined
   * Sorter is unspecified, or has not been configured.
   * @see #DEFAULT_SORTER
   * @see #setSorter(org.cp.elements.util.sort.Sorter)
   * @see #getSizeThreshold()
   * @see org.cp.elements.util.sort.Sorter
   */
  public Sorter getSorter() {
    return ObjectUtils.defaultIfNull(sorter, DEFAULT_SORTER);
  }

  /**
   * Sets the Sorter implementation to use when the remaining elements from the collection to sort using the Quick Sort
   * algorithm is less than equal to the size threshold.
   * <p/>
   * @param sorter an alternate sorting algorithm implementation to use when the size threshold is reached during the
   * Quick Sort.
   * @see #getSorter()
   * @see org.cp.elements.util.sort.Sorter
   */
  public void setSorter(final Sorter sorter) {
    this.sorter = sorter;
  }

  /**
   * Uses the Quick Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    int elementsSize = elements.size();

    if (elementsSize <= getSizeThreshold()) {
      return getSorter().sort(elements);
    }
    else {
      int beginIndex = 1;
      int endIndex = (elementsSize - 1);

      E pivotElement = elements.get(0);

      while (beginIndex < endIndex) {
        while (beginIndex < endIndex && getOrderBy().compare(elements.get(beginIndex), pivotElement) <= 0) {
          beginIndex++;
        }

        while (endIndex >= beginIndex && getOrderBy().compare(elements.get(endIndex), pivotElement) >= 0) {
          endIndex--;
        }

        if (beginIndex < endIndex) {
          swap(elements, beginIndex, endIndex);
        }
      }

      swap(elements, 0, endIndex);
      sort(elements.subList(0, endIndex));
      sort(elements.subList(endIndex + 1, elementsSize));

      return elements;
    }
  }

}
