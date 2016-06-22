/*
 * Copyright 2016 Author or Authors.
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

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import org.cp.elements.util.search.support.BinarySearch;
import org.cp.elements.util.search.support.LinearSearch;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link SearcherFactory} class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.search.SearcherFactory
 * @since 1.0.0
 */
public class SearcherFactoryTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void createSearcher() {
    assertTrue(SearcherFactory.createSearcher(SearchType.BINARY_SEARCH) instanceof BinarySearch);
    assertTrue(SearcherFactory.createSearcher(SearchType.LINEAR_SEARCH) instanceof LinearSearch);
  }

  @Test
  public void createSearcherWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(String.format("The SearchType (null) is not supported by the %1$s!",
      SearcherFactory.class.getSimpleName()));

    SearcherFactory.createSearcher(null);
  }

  @Test
  public void createSearcherElseDefaultWithKnownSearchAlgorithm() {
    assertTrue(SearcherFactory.createSearcherElseDefault(SearchType.BINARY_SEARCH, null) instanceof BinarySearch);
  }

  @Test
  public void createSearcherElseDefaultWithUnknownSearchAlgorithm() {
    Searcher mockDefaultSearcher = mock(Searcher.class);

    assertSame(mockDefaultSearcher, SearcherFactory.createSearcherElseDefault(SearchType.INDEX_SEARCH, mockDefaultSearcher));
    assertNull(SearcherFactory.createSearcherElseDefault(SearchType.UNKNOWN_SEARCH, null));
  }
}
