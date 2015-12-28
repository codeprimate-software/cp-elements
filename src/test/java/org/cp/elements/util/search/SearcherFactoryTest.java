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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.cp.elements.test.AbstractMockingTestSuite;
import org.cp.elements.util.search.support.BinarySearch;
import org.cp.elements.util.search.support.LinearSearch;
import org.junit.Test;

/**
 * The SearcherFactoryTest class is a test suite of test cases testing the contract and functionality of the
 * SearcherFactory class.
 *
 * @author John J. Blum
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.search.SearcherFactory
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SearcherFactoryTest extends AbstractMockingTestSuite {

  @Test
  public void testCreateSearcher() {
    assertTrue(SearcherFactory.createSearcher(SearchType.BINARY_SEARCH) instanceof BinarySearch);
    assertTrue(SearcherFactory.createSearcher(SearchType.LINEAR_SEARCH) instanceof LinearSearch);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testCreateSearcherWithNull() {
    try {
      SearcherFactory.createSearcher(null);
    }
    catch (IllegalArgumentException expected) {
      assertEquals(String.format("The SearchType (null) is not supported by the %1$s!",
        SearcherFactory.class.getSimpleName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testCreateSearcherElseDefaultWithKnownSearchAlgorithm() {
    assertTrue(SearcherFactory.createSearcherElseDefault(SearchType.BINARY_SEARCH, null) instanceof BinarySearch);
  }

  @Test
  public void testCreateSearcherElseDefaultWithUnknownSearchAlgorithm() {
    Searcher mockDefaultSearcher = mockContext.mock(Searcher.class, "testCreateSearcherElseDefaultWithUnknownSearchAlgorithm");

    assertSame(mockDefaultSearcher, SearcherFactory.createSearcherElseDefault(SearchType.INDEX_SEARCH, mockDefaultSearcher));
    assertNull(SearcherFactory.createSearcherElseDefault(SearchType.UNKNOWN_SEARCH, null));
  }

}
