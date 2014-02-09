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

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

import org.cp.elements.util.sort.AbstractSorter;

/**
 * The MergeSort class is an implementation of the Merge Sort algorithm and the Sorter interface.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.AbstractSorter
 * @link http://en.wikipedia.org/wiki/Merge_sort
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class MergeSort extends AbstractSorter {

  /**
   * Uses the Merge Sort to sort an array of elements as defined by the Comparator, or as determined by the elements
   * in the array if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   * @see #sort(java.util.List)
   * @see org.cp.elements.util.sort.AbstractSorter.SortableArrayList
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> E[] sort(final E... elements) {
    return (E[]) sort(new SortableArrayList(elements)).toArray(
      (E[]) Array.newInstance(elements.getClass().getComponentType(), elements.length));
  }

  /**
   * Uses the Merge Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    if (elements.size() > 1) {
      int size = elements.size();
      int count = ((size / 2) + (size % 2));

      List<E> leftElements = sort(elements.subList(0, count));
      List<E> rightElements = sort(elements.subList(count, size));

      return merge(leftElements, rightElements);
    }

    return elements;
  }

  /**
   * Merges two List of elements in sorted order as determined by the orderBy Comparator, or as determined by the
   * natural order of the elements in the Lists.
   * <p/>
   * @param <E> the Class type of the elements in the List.
   * @param leftElements the left List of elements to merge.
   * @param rightElements the right List of elements to merge.
   * @return a new List containing the elements from the left and right Lists merged in sorted order.
   * @see java.util.List
   */
  // TODO solve the ArrayList allocation (memory resource consumption) problem
  protected <E> List<E> merge(final List<E> leftElements, final List<E> rightElements) {
    int leftIndex = 0;
    int leftSize = leftElements.size();
    int rightIndex = 0;
    int rightSize = rightElements.size();

    List<E> mergedElements = new ArrayList<E>(leftSize + rightSize);

    for (int mergeIndex = 0, mergeSize = (leftSize + rightSize); mergeIndex < mergeSize; mergeIndex++) {
      if (leftIndex == leftSize) {
        mergedElements.add(rightElements.get(rightIndex++));
      }
      else if (rightIndex == rightSize) {
        mergedElements.add(leftElements.get(leftIndex++));
      }
      else {
        E leftElement = leftElements.get(leftIndex);
        E rightElement = rightElements.get(rightIndex);

        if (getOrderBy().compare(leftElement, rightElement) <= 0) {
          mergedElements.add(leftElement);
          leftIndex++;
        }
        else {
          mergedElements.add(rightElement);
          rightIndex++;
        }
      }
    }

    return mergedElements;
  }

}
