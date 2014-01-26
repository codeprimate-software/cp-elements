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

package org.cp.elements.util.search;

import java.util.Collection;
import java.util.Comparator;

/**
 * The Searcher interface defines a contract for implementing classes responsible for searching a collection of objects
 * in order to find a specified match or a collection of matches.
 * <p/>
 * @author John J. Blum
 * @see java.util.Collection
 * @see java.util.Comparator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Searcher {

  /**
   * Gets the Comparator used to match the element, or elements in the collection.
   * <p/>
   * @param <E> the Class type of elements in the collection.
   * @return the Comparator used to compare and match the element, or elements in the collection during
   * the search operation.
   * @see java.util.Comparator
   */
  public <E> Comparator<E> getMatcher();

  /**
   * Searches the array of elements in order to find the element or elements matching the criteria defined
   * by the Comparator (matcher).
   * <p/>
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return the element in the array matching the search criteria defined by the Comparator.
   * @see #getMatcher()
   * @see #search(java.util.Collection)
   */
  public <E> E search(E... array);

  /**
   * Searches the Collection of elements in order to find the element or elements matching the criteria defined
   * by the Comparator (matcher).
   * <p/>
   * @param <E> the Class type of elements in the Collection.
   * @param collection the Collection of elements to search.
   * @return the element in the Collection matching the search criteria defined by the Comparator.
   * @see #getMatcher()
   * @see #search(Object[])
   */
  public <E, T extends Collection<E> > E search(T collection);

}
