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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import org.cp.elements.test.TestUtils;
import org.cp.elements.util.search.support.BinarySearch;
import org.cp.elements.util.search.support.LinearSearch;
import org.junit.Test;

/**
 * Unit Tests for {@link SearcherFactory}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.search.SearcherFactory
 * @see org.cp.elements.util.search.support.BinarySearch
 * @see org.cp.elements.util.search.support.LinearSearch
 * @since 1.0.0
 */
public class SearcherFactoryTests {

  @Test
  public void createSearcher() {

    assertThat(SearcherFactory.<Searcher>createSearcher(SearchType.BINARY_SEARCH)).isInstanceOf(BinarySearch.class);
    assertThat(SearcherFactory.<Searcher>createSearcher(SearchType.LINEAR_SEARCH)).isInstanceOf(LinearSearch.class);
  }

  @Test(expected = IllegalArgumentException.class)
  public void createSearcherWithNull() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> SearcherFactory.createSearcher(null),
      () -> String.format("The SearchType (null) is not supported by the %s!", SearcherFactory.class.getSimpleName()));
  }

  @Test
  public void createSearcherElseDefaultWithKnownSearchAlgorithm() {
    assertThat(SearcherFactory.<Searcher>createSearcherElseDefault(SearchType.BINARY_SEARCH, null))
      .isInstanceOf(BinarySearch.class);
  }

  @Test
  public void createSearcherElseDefaultWithUnknownSearchAlgorithm() {

    Searcher mockDefaultSearcher = mock(Searcher.class);

    assertThat(SearcherFactory.createSearcherElseDefault(SearchType.INDEX_SEARCH, mockDefaultSearcher))
      .isSameAs(mockDefaultSearcher);

    assertThat(SearcherFactory.<Searcher>createSearcherElseDefault(SearchType.UNKNOWN_SEARCH, null))
      .isNull();
  }
}
