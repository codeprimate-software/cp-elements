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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.cp.elements.lang.Assert;

/**
 * The AbstractSearcher class is a base class encapsulating functionality common to all Searcher implementations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.Matcher
 * @see org.cp.elements.util.search.Searcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSearcher implements Searcher {

  private Matcher matcher;

  /**
   * Gets the Matcher used to match the element, or elements in the collection.
   * <p/>
   * @param <E> the Class type of elements in the collection.
   * @return the Matcher used to compare and match the element, or elements in the collection during
   * the search operation.
   * @throws IllegalStateException if the Comparator used as the matcher for this Searcher was not configured.
   * @see #setMatcher(Matcher)
   * @see org.cp.elements.util.search.Matcher
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> Matcher<E> getMatcher() {
    Assert.state(matcher != null, "A reference to the Matcher used by this Searcher ({0}) for searching and matching elements in the collection was not properly configured!",
      getClass().getName());
    return matcher;
  }

  /**
   * Gets the Matcher used to match the element, or elements in the collection.
   * <p/>
   * @param matcher the Matcher used to compare and match the element, or elements in the collection during
   * the search operation.
   * @throws NullPointerException if the Matcher reference used by this Searcher is null.
   * @see #getMatcher()
   * @see org.cp.elements.util.search.Matcher
   */
  public void setMatcher(final Matcher matcher) {
    Assert.notNull(matcher, "The Matcher used to match elements in the collection during the search by this Searcher ({0}) cannot be null!",
      getClass().getName());
    this.matcher = matcher;
  }

  /**
   * Searches the array of elements in order to find the element or elements matching the criteria defined
   * by the Matcher.
   * <p/>
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return the element in the array matching the search criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #search(java.util.Collection)
   * @see java.util.Arrays#asList(Object[])
   */
  @Override
  public <E> E search(final E... array) {
    return search(Arrays.asList(array));
  }

  /**
   * Searches an array of elements finding all elements in the array matching the criteria defined by the Matcher.
   * <p/>
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return an Iterable object containing all elements in the array that match the criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #searchForAll(java.util.Collection)
   * @see java.lang.Iterable
   * @see java.util.Arrays#asList(Object[])
   */
  public <E> Iterable<E> searchForAll(final E... array) {
    return searchForAll(Arrays.asList(array));
  }

  /**
   * Searches a collection of elements finding all elements in the collection matching the criteria defined by the Matcher.
   * <p/>
   * @param <E> the Class type of elements in the collection.
   * @param collection the collection of elements to search.
   * @return an Iterable object containing all elements in the collection that match the criteria defined by the Matcher.
   * @see #getMatcher()
   * @see #searchForAll(Object[])
   * @see java.lang.Iterable
   */
  public <E> Iterable<E> searchForAll(final Collection<E> collection) {
    final List<E> results = new ArrayList<E>(collection.size());

    for (E element : collection) {
      if (getMatcher().isMatch(element)) {
        results.add(element);
      }
    }

    return results;
  }

}
