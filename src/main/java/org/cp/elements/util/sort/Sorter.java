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
 * @see java.util.Comparator
 * @see java.util.List
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Sorter {

  /**
   * Gets the Comparator used to order the elements in the collection.
   * <p/>
   * @param <E> the type of elements in the collection to compare.
   * @return the Comparator used to order the collection elements.
   * @see java.util.Comparator
   */
  public <E> Comparator<E> getOrderBy();

  /**
   * Sorts an array of elements as defined by the Comparator, or as determined by the elements in the array
   * if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the array.
   * @param elements the array of elements to sort.
   * @return the array of elements sorted.
   */
  public <E> E[] sort(E... elements);

  /**
   * Sorts a List of elements as defined by the Comparator, or as determined by the elements in the collection
   * if the elements are Comparable.
   * <p/>
   * @param <E> the type of elements in the List.
   * @param elements the List of elements to sort.
   * @return the collection of elements sorted.
   * @see java.util.List
   */
  public <E> List<E> sort(List<E> elements);

}
