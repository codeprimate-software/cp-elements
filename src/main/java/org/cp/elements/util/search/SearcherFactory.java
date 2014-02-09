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

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.util.search.support.BinarySearch;
import org.cp.elements.util.search.support.LinearSearch;

/**
 * The SearcherFactory class is a factory for creating instances of different Searcher implementations that implement
 * different searching algorithms.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.SearchType
 * @see org.cp.elements.util.search.support.BinarySearch
 * @see org.cp.elements.util.search.support.LinearSearch
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SearcherFactory {

  /**
   * Creates an instance of the Searcher interface implementing the searching algorithm based on the SearchType.
   * <p/>
   * @param <T> the Class type of the actual Searcher implementation based on the SearchType.
   * @param type the type of searching algorithm Searcher implementation to create.
   * @return a Searcher implementation subclass that implements the searching algorithm based on the SearchType.
   * @see org.cp.elements.util.search.Searcher
   * @see org.cp.elements.util.search.SearchType
   */
  @SuppressWarnings("unchecked")
  public static <T extends Searcher> T createSearcher(final SearchType type) {
    switch (ObjectUtils.defaultIfNull(type, SearchType.UNKNOWN_SEARCH)) {
      case BINARY_SEARCH:
        return (T) new BinarySearch();
      case LINEAR_SEARCH:
        return (T) new LinearSearch();
      default:
        throw new IllegalArgumentException(String.format("The SearchType (%1$s) is not supported by the %2$s!", type,
          SearcherFactory.class.getSimpleName()));
    }
  }

}
