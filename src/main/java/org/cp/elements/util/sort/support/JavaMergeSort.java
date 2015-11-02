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

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.cp.elements.util.sort.AbstractSorter;

/**
 * The JavaMergeSort class is an implementation of the Merge Sort algorithm implemented using the Java API.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.sort.AbstractSorter
 * @see java.util.Arrays#sort(Object[], java.util.Comparator)
 * @see java.util.Collections#sort(java.util.List, java.util.Comparator)
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class JavaMergeSort extends AbstractSorter {

  /**
   * Uses Java's modified Merge Sort to sort an array of elements as defined by the Comparator, or as determined
   * by the elements in the array if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   * @see #getOrderBy()
   * @see java.util.Arrays#sort(Object[], java.util.Comparator)
   */
  @Override
  @SuppressWarnings("all")
  public <E> E[] sort(final E... elements) {
    Arrays.sort(elements, getOrderBy());
    return elements;
  }

  /**
   * Uses Java's modified Merge Sort algorithm to sort a List of elements as defined by the Comparator, or as determined
   * by the elements in the collection if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see #getOrderBy()
   * @see java.util.Collections#sort(java.util.List, java.util.Comparator)
   * @see java.util.List
   */
  @Override
  public <E> List<E> sort(final List<E> elements) {
    Collections.sort(elements, getOrderBy());
    return elements;
  }

}
