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

package org.cp.elements.util.search.support;

import java.util.Collection;

import org.cp.elements.util.search.AbstractSearcher;

/**
 * The LinearSearch class is an implementation of the Searcher interface iterating over elements in the collection
 * in a linear manner in search of the first element satisfying the Matcher's criteria.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractSearcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class LinearSearch extends AbstractSearcher {

  /**
   * Searches the Collection of elements in order to find the element or elements matching the criteria defined
   * by the Matcher.
   * <p/>
   * @param <E> the Class type of elements in the Collection.
   * @param collection the Collection of elements to search.
   * @return the element in the Collection matching the search criteria defined by the Matcher.
   * @see #getMatcher()
   */
  @Override
  public <E, T extends Collection<E>> E search(final T collection) {
    for (E element : collection) {
      if (getMatcher().isMatch(element)) {
        return element;
      }
    }

    return null;
  }

}
