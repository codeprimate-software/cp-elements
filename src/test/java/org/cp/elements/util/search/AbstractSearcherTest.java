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

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.cp.elements.lang.NumberUtils;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.Test;

/**
 * The AbstractSearcherTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractSearcher class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.search.AbstractSearcher
 * @see org.junit.Test
 * @since 1.0.0
 */
public class AbstractSearcherTest extends AbstractMockingTestSuite {

  @Test
  public void testSetAndGetMatcher() {
    AbstractSearcher searcher = new TestSearcher();
    Matcher mockMatcher = mockContext.mock(Matcher.class);

    searcher.setMatcher(mockMatcher);

    assertSame(mockMatcher, searcher.getMatcher());
  }

  @Test(expected = NullPointerException.class)
  public void testSetMatcherWithNull() {
    try {
      new TestSearcher().setMatcher(null);
    }
    catch (NullPointerException expected) {
      assertEquals(String.format("The Matcher used to match elements in the collection during the search by this Searcher (%1$s) cannot be null!",
        TestSearcher.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void testGetMatcherWhenNull() {
    try {
      new TestSearcher().getMatcher();
    }
    catch (IllegalStateException expected) {
      assertEquals(String.format("A reference to the Matcher used by this Searcher (%1$s) for searching and matching elements in the collection was not properly configured!",
        TestSearcher.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testSearchArray() {
    TestSearcher searcher = new TestSearcher();

    assertNull(searcher.search("test", "testing", "tested"));
    assertTrue(searcher.isSearched());
  }

  @Test
  public void testSearchForAllInArray() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher() {
      int matchIndex = 0;
      int value = 1;
      @Override public int match(final Object obj) {
        value *= -1;
        return (NumberUtils.isEven(++matchIndex) ? 0 : value);
      }
    });

    String[] animals = { "aardvark", "baboon", "cat", "dog", "elephant", "ferret", "giraffe", "horse", "iguana",
      "jackal", "kangaroo", "lama", "mouse", "newt", "octopus", "porcupine", "quail", "rabbit", "snake", "turtle",
      "urchin", "viper", "walrus", "x", "yack", "zebra" };

    Iterable<String> searchResults = searcher.searchForAll(animals);

    assertNotNull(searchResults);

    int resultIndex = 1;

    for (String result : searchResults) {
      assertEquals(animals[resultIndex], result);
      resultIndex += 2;
    }
  }

  @Test
  public void testSearchForAllInCollection() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher() {
      int matchIndex = 0;
      int value = 1;
      @Override public int match(final Object obj) {
        value *= -1;
        return (NumberUtils.isOdd(++matchIndex) ? 0 : value);
      }
    });

    List<String> animals = Arrays.asList("aardvark", "baboon", "cat", "dog", "elephant", "ferret", "giraffe", "horse", "iguana",
      "jackal", "kangaroo", "lama", "mouse", "newt", "octopus", "porcupine", "quail", "rabbit", "snake", "turtle",
      "urchin", "viper", "walrus", "x", "yack", "zebra");

    Iterable<String> searchResults = searcher.searchForAll(animals);

    assertNotNull(searchResults);

    int resultIndex = 0;

    for (String result : searchResults) {
      assertEquals(animals.get(resultIndex), result);
      resultIndex += 2;
    }
  }

  @Test
  public void testSearchForAllWithNoMatches() {
    AbstractSearcher searcher = new TestSearcher();

    searcher.setMatcher(new AbstractMatcher() {
      @Override public int match(final Object obj) {
        return -1;
      }
    });

    Iterable<String> searchResults = searcher.searchForAll("test", "testing", "tested");

    assertNotNull(searchResults);
    assertFalse(searchResults.iterator().hasNext());
  }

  @SuppressWarnings("unused")
  protected static final class TestSearcher extends AbstractSearcher {

    private boolean searched = false;

    public boolean isSearched() {
      return searched;
    }

    @Override
    public <E, T extends Collection<E>> E search(final T collection) {
      searched = true;
      return null;
    }
  }

}
