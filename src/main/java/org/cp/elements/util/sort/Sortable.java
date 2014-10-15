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
 * The Sortable interface defines a contract for objects of implementing classes to provide a means (java.util.List)
 * by which to sort the object.
 * <p/>
 * @author John J. Blum
 * @param <T> the Class type of the elements in the List.
 * @see org.cp.elements.util.sort.Sorter
 * @see org.cp.elements.util.sort.annotation.Sortable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Sortable<T> {

  /**
   * Returns a List representation of the implementing object in order to perform the sort.
   * <p/>
   * @return a List representation of the implementing object used to perform the sort.
   * @see java.util.List
   */
  List<T> asList();

  /**
   * Property defining the Comparator class to use when sorting and ordering the elements in the collection (List).
   * <p/>
   * @return a Comparator class type used to sort and order the elements in the collection (List) during the sort.
   * @see java.util.Comparator
   */
  Comparator<T> getOrderBy();

}
