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
import java.util.List;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.util.search.AbstractSearcher;

/**
 * The BinarySearch class is an implementation of the Searcher interface iterating over the elements in the collection
 * using a binary algorithm in search of the first element satisfying the Matcher's criteria.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractSearcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class BinarySearch extends AbstractSearcher {

  /**
   * Searches the Collection of elements in order to find the element or elements matching the criteria defined
   * by the Matcher.  The search operation expects the collection of elements to an instance of java.util.List,
   * an ordered collection of elements that are sorted according the natural ordering of the element's Comparable
   * Class type.
   * <p/>
   * @param <E> the Class type of elements in the Collection.
   * @param collection the Collection of elements to search.
   * @return the element in the Collection matching the search criteria defined by the Matcher.
   * @throws IllegalArgumentException if the collection of elements is not an instance of java.util.List.
   * @see #getMatcher()
   * @see #search(Object[])
   * @see #doSearch(java.util.List)
   * @see java.util.List
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> E search(final Collection<E> collection) {
    Assert.isInstanceOf(collection, List.class, "The collection {0} must be an instance of java.util.List!",
      ClassUtils.getClassName(collection));
    return (E) doSearch((List<E>) collection);
  }

  /**
   * Performs the actual (binary) search of an ordered collection (List) of elements in search of a single element
   * matching the criteria defined by the Matcher.
   * <p/>
   * @param <E> the Class type of the elements in the List.
   * @param <T> the Class type of the List of elements.
   * @param list the List of elements to search.
   * @return a single element of the List matching the criteria defined by the Matcher or null if no element in the List
   * matches the criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #search(java.util.Collection)
   * @see java.util.List
   */
  protected <E> E doSearch(final List<E> list) {
    if (!list.isEmpty()) {
      int matchIndex = (list.size() / 2);
      E element = list.get(matchIndex);
      int matchResult = getMatcher().match(element);

      if (matchResult == 0) {
        return element;
      }
      else if (matchResult < 0) {
        return doSearch(list.subList(0, matchIndex));
      }
      else {
        return doSearch(list.subList(matchIndex + 1, list.size()));
      }
    }

    return null;
  }

}
