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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The AbstractMatcherTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractMatcher class.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.search.AbstractMatcher
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractMatcherTest {

  @Test
  public void testAccept() {
    TestMatcher<Object> matcher = new TestMatcher<>();

    matcher.matchValue = 0;

    assertTrue(matcher.accept("test"));

    matcher.matchValue = -1;

    assertFalse(matcher.accept("testing"));

    matcher.matchValue = 1;

    assertFalse(matcher.accept("tested"));
  }

  @Test
  public void testIsMatch() {
    TestMatcher<Object> matcher = new TestMatcher<>();

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
