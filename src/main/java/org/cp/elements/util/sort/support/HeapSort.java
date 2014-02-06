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
 * The HeapSort class is an implementation of the Heap Sort algorithm and the Sorter interface.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.AbstractSorter
 * @link http://en.wikipedia.org/wiki/Heapsort
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class HeapSort extends AbstractSorter {

  /**
   * Uses the Heap Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see #siftDown(java.util.List, int, int)
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    int size = elements.size();

    // create the heap
    for (int parentIndex = ((size - 2) / 2); parentIndex >= 0; parentIndex--) {
      siftDown(elements, parentIndex, size - 1);
    }

    // swap the first and last elements in the heap of array since the first is the largest value in the heap,
    // and then heapify the heap again
    for (int count = (size - 1); count > 0; count--) {
      swap(elements, 0, count);
      siftDown(elements, 0, count - 1);
    }

    return elements;
  }

  /**
   * Creates a binary heap with the list of elements with the largest valued element at the root followed by the next
   * largest valued elements as parents down to the leafs.
   * <p/>
   * @param <E> the Class type of the elements in the List.
   * @param elements the List of elements to heapify.
   * @param startIndex an integer value indicating the starting index in the heap in the List of elements.
   * @param endIndex an integer value indicating the ending index in the heap in the List of elements.
   */
  protected <E> void siftDown(final List<E> elements, final int startIndex, final int endIndex) {
    int rootIndex = startIndex;

    while ((rootIndex * 2 + 1) <= endIndex) {
      int swapIndex = rootIndex;
      int leftChildIndex = (rootIndex * 2 + 1);
      int rightChildIndex = (leftChildIndex + 1);

      if (getOrderBy().compare(elements.get(swapIndex), elements.get(leftChildIndex)) < 0) {
        swapIndex = leftChildIndex;
      }

      if (rightChildIndex <= endIndex && getOrderBy().compare(elements.get(swapIndex), elements.get(rightChildIndex)) < 0) {
        swapIndex = rightChildIndex;
      }

      if (swapIndex != rootIndex) {
        swap(elements, rootIndex, swapIndex);
        rootIndex = swapIndex;
      }
      else {
        return;
      }
    }
  }

}
