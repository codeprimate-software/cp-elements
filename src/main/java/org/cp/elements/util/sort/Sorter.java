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

package org.cp.elements.util.sort;

import java.util.Comparator;
import java.util.List;

/**
 * The Sorter interface defines a contract for implementing objects that sort a collection of elements.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Comparable
 * @see java.util.Comparator
 * @see java.util.List
 * @see org.cp.elements.util.sort.Sortable
 * @see org.cp.elements.util.sort.SorterFactory
 * @see org.cp.elements.util.sort.SortType
 * @see org.cp.elements.util.sort.annotation.Sortable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Sorter {

  /**
   * Gets the Comparator used to order the elements in the collection.
   * <p/>
   * @param <E> the type of elements in the collection to compare.
   * @return the Comparator used to order the collection elements.
   * @see java.lang.Comparable
   * @see java.util.Comparator
   */
  <E> Comparator<E> getOrderBy();

  /**
   * Sorts an array of elements as defined by the 'orderBy' Comparator, or as determined by the elements in the array
   * if the elements are Comparable.
   * <p/>
   * @param <E> the Class type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   */
  <E> E[] sort(E... elements);

  /**
   * Sorts a List of elements as defined by the 'orderBy' Comparator, or as determined by the elements in the collection
   * if the elements are Comparable.
   * <p/>
   * @param <E> the Class type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the List of elements sorted.
   * @see java.util.List
   */
  <E> List<E> sort(List<E> elements);

  /**
   * Sorts the List representation of the Sortable implementing object as defined by the 'orderBy' Comparator, or as
   * determined by elements in the Sortable collection if the elements are Comparable.
   * <p/>
   * @param <E> the Class type of elements in the Sortable.
   * @param sortable the Sortable implementing object containing the collection of elements to sort.
   * @return the Sortable implementing object sorted.
   * @see org.cp.elements.util.sort.Sortable
   */
  <E> Sortable<E> sort(Sortable<E> sortable);

  /**
   * Sorts the List representation of the @Sortable annotated object as defined by the 'orderBy' Comparator, or as
   * determined by elements in the Sortable collection if the elements are Comparable.
   * <p/>
   * @param <T> the Class type of object annotated with the @Sortable annotation.
   * @param sortableAnnotatedObject the @Sortable annotated object containing the collection of elements to sort.
   * @return the @Sortable annotated object sorted.
   * @see org.cp.elements.util.sort.annotation.Sortable
   */
  <T> T sort(T sortableAnnotatedObject);

}
