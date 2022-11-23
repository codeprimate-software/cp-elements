/*
 * Copyright 2011-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cp.elements.util.sort.support;

import java.util.List;

import org.cp.elements.util.sort.AbstractSorter;

/**
 * The HeapSort class is an implementation of the Heap Sort algorithm
 * and the {@link org.cp.elements.util.sort.Sorter} interface.
 *
 * @author John J. Blum
 * @see java.util.List
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see org.cp.elements.util.sort.Sorter
 * @see <a href="http://en.wikipedia.org/wiki/Heapsort">Heapsort</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class HeapSort extends AbstractSorter {

  /**
   * Uses the Heap Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   *
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
   *
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

      if (rightChildIndex <= endIndex
          && getOrderBy().compare(elements.get(swapIndex), elements.get(rightChildIndex)) < 0) {
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
