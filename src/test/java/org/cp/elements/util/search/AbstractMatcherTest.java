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

import org.junit.Test;

/**
 * The AbstractMatcherTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractMatcher class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractMatcher
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractMatcherTest {

  @Test
  public void testAccept() {
    TestMatcher<Object> matcher = new TestMatcher<Object>();

    matcher.matchValue = 0;

    assertTrue(matcher.accept("test"));

    matcher.matchValue = -1;

    assertFalse(matcher.accept("testing"));

    matcher.matchValue = 1;

    assertFalse(matcher.accept("tested"));
  }

  @Test
  public void testIsMatch() {
    TestMatcher<Object> matcher = new TestMatcher<Object>();

    matcher.matchValue = 0;

    assertTrue(matcher.isMatch("test"));

    matcher.matchValue = -1;

    assertFalse(matcher.isMatch("testing"));

    matcher.matchValue = 1;

    assertFalse(matcher.isMatch("tested"));
  }

  protected static final class TestMatcher<T> extends AbstractMatcher<T> {

    private int matchValue = 0;

    @Override
    public int match(final T obj) {
      return matchValue;
    }
  }

}
