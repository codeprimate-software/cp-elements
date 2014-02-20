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

import java.util.List;

/**
 * The Searchable interface defines a contract for objects of implementing classes to provide a means (java.util.List)
 * by which to search the object.
 * <p/>
 * @author John J. Blum
 * @param <T> the Class type of the elements in the List.
 * @see org.cp.elements.util.search.Searcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Searchable<T> {

  /**
   * Returns a List representation of the implementing object in order to perform the search.
   * <p/>
   * @return a List representation of the implementing object used to perform the search.
   * @see java.util.List
   */
  public List<T> asList();

  /**
   * Gets the desired Matcher to use during the search operation performed by the Searcher to match and find elements
   * in the collection.
   * <p/>
   * @return the desired Matcher to match and find elements in the collection during the search operation.
   * @see org.cp.elements.util.search.Matcher
   */
  public Matcher<T> getMatcher();

}
