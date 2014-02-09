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

import static org.junit.Assert.*;

import org.cp.elements.util.search.support.BinarySearch;
import org.cp.elements.util.search.support.LinearSearch;
import org.junit.Test;

/**
 * The SearcherFactoryTest class is a test suite of test cases testing the contract and functionality of the
 * SearcherFactory class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.SearcherFactory
 * @see org.junit.Test
 * @since 1.0.0
 */
public class SearcherFactoryTest {

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

}
