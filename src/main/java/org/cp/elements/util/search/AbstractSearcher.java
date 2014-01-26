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

import java.util.Arrays;
import java.util.Comparator;

import org.cp.elements.lang.Assert;

/**
 * The AbstractSearcher class is a base class encapsulating functionality common to all Searcher implementations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.Searcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractSearcher implements Searcher {

  private Comparator matcher;

  /**
   * Gets the Comparator used to match the element, or elements in the collection.
   * <p/>
   * @param <E> the Class type of elements in the collection.
   * @return the Comparator used to compare and match the element, or elements in the collection during
   * the search operation.
   * @throws IllegalStateException if the Comparator used as the matcher for this Searcher was not configured.
   * @see java.util.Comparator
   */
  @Override
  @SuppressWarnings("unchecked")
  public <E> Comparator<E> getMatcher() {
    Assert.state(matcher != null, "A reference to the Comparator used as the matcher for this Searcher ({0}) for searching elements in the collection was not properly configured!",
      getClass().getName());
    return matcher;
  }

  /**
   * Gets the Comparator used to match the element, or elements in the collection.
   * <p/>
   * @param matcher the Comparator used to compare and match the element, or elements in the collection during
   * the search operation.
   * @throws NullPointerException if the Comparator reference used as the matcher for this Searcher is null.
   * @see java.util.Comparator
   */
  public void setMatcher(final Comparator matcher) {
    Assert.notNull(matcher, "The Comparator used as the matcher for this Searcher ({0}) cannot be null!",
      getClass().getName());
    this.matcher = matcher;
  }

  /**
   * Searches the array of elements in order to find the element or elements matching the criteria defined
   * by the Comparator (matcher).
   * <p/>
   * @param <E> the Class type of elements in the array.
   * @param array the array of elements to search.
   * @return the element in the array matching the search criteria defined by the Comparator.
   * @see #getMatcher()
   * @see #search(java.util.Collection)
   * @see java.util.Arrays#asList(Object[])
   */
  @Override
  public <E> E search(final E... array) {
    return search(Arrays.asList(array));
  }

}
