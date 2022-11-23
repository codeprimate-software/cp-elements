/*
 * Copyright 2011-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cp.elements.util.search;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.search.support.BinarySearch;
import org.cp.elements.util.search.support.LinearSearch;

/**
 * The SearcherFactory class is a factory for creating instances of different Searcher implementations that implement
 * different searching algorithms.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.search.Searcher
 * @see org.cp.elements.util.search.SearchType
 * @see org.cp.elements.util.search.support.BinarySearch
 * @see org.cp.elements.util.search.support.LinearSearch
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public final class SearcherFactory {

  /**
   * Creates an instance of the Searcher interface implementing the searching algorithm based on the SearchType.
   *
   * @param <T> the Class type of the actual Searcher implementation based on the SearchType.
   * @param type the type of searching algorithm Searcher implementation to create.
   * @return a Searcher implementation subclass that implements the searching algorithm based on the SearchType.
   * @see org.cp.elements.util.search.Searcher
   * @see org.cp.elements.util.search.SearchType
   */
  @SuppressWarnings("unchecked")
  public static @NotNull <T extends Searcher> T createSearcher(@Nullable SearchType type) {

    SearchType resolvedSearchType = type != null ? type : SearchType.UNKNOWN_SEARCH;

    switch (resolvedSearchType) {
      case BINARY_SEARCH:
        return (T) new BinarySearch();
      case LINEAR_SEARCH:
        return (T) new LinearSearch();
      default:
        throw new IllegalArgumentException(String.format("The SearchType (%1$s) is not supported by the %2$s!", type,
          SearcherFactory.class.getSimpleName()));
    }
  }

  /**
   * Creates an instance of the Searcher interface implementing the searching algorithm based on the SearchType,
   * otherwise returns the provided default Searcher implementation if a Searcher based on the specified SearchType
   * is not available.
   *
   * @param <T> the Class type of the actual Searcher implementation based on the SearchType.
   * @param type the type of searching algorithm Searcher implementation to create.
   * @param defaultSearcher the default Searcher implementation to use if a Searcher based on the specified SearchType
   * is not available.
   * @return a Searcher implementation subclass that implements the searching algorithm based on the SearchType,
   * or the provided default Searcher implementation if the Searcher based on the SearchType is not available.
   * @see #createSearcher(SearchType)
   * @see org.cp.elements.util.search.Searcher
   * @see org.cp.elements.util.search.SearchType
   */
  public static @Nullable <T extends Searcher> T createSearcherElseDefault(@Nullable SearchType type,
      @Nullable T defaultSearcher) {

    try {
      return createSearcher(type);
    }
    catch (IllegalArgumentException ignore) {
      return defaultSearcher;
    }
  }

  private SearcherFactory() { }

}
