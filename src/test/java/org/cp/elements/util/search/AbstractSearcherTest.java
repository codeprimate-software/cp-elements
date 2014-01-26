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

import java.util.Collection;
import java.util.Comparator;

import org.jmock.Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The AbstractSearcherTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractSearcher class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractSearcher
 * @see org.jmock.Mockery
 * @see org.jmock.lib.legacy.ClassImposteriser
 * @see org.junit.Test
 * @since 1.0.0
 */
public class AbstractSearcherTest {

  private Mockery mockContext;

  @Before
  public void setup() {
    mockContext = new Mockery();
    mockContext.setImposteriser(ClassImposteriser.INSTANCE);
  }

  @After
  public void tearDown() {
    mockContext.assertIsSatisfied();
    mockContext = null;
  }

  @Test
  public void testSetAndGetMatcher() {
    AbstractSearcher searcher = new TestSearcher();
    Comparator mockMatcher = mockContext.mock(Comparator.class);

    searcher.setMatcher(mockMatcher);

    assertSame(mockMatcher, searcher.getMatcher());
  }

  @Test(expected = NullPointerException.class)
  public void testSetMatcherWithNull() {
    try {
      new TestSearcher().setMatcher(null);
    }
    catch (NullPointerException expected) {
      assertEquals(String.format("The Comparator used as the matcher for this Searcher (%1$s) cannot be null!",
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
      assertEquals(String.format("A reference to the Comparator used as the matcher for this Searcher (%1$s) for searching elements in the collection was not properly configured!",
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
