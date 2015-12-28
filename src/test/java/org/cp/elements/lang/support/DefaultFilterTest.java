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

package org.cp.elements.lang.support;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The DefaultFilterTest class is a test suite of test cases testing the contract and functionality
 * of the DefaultFilter class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.DefaultFilter
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class DefaultFilterTest {

  @Test
  public void testAccept() {
    final DefaultFilter<Object> defaultFilter = new DefaultFilter<Object>();

    assertTrue(defaultFilter.isAccepting());
    assertTrue(defaultFilter.accept(null));
    assertTrue(defaultFilter.accept(""));
    assertTrue(defaultFilter.accept("  "));
    assertTrue(defaultFilter.accept(Boolean.FALSE));
    assertTrue(defaultFilter.accept('\0'));
    assertTrue(defaultFilter.accept(0));
    assertTrue(defaultFilter.accept(-0.0d));
    assertTrue(defaultFilter.accept("test"));
    assertTrue(defaultFilter.accept(new Object()));
  }

  @Test
  public void testReject() {
    final DefaultFilter<Object> defaultFilter = new DefaultFilter<Object>(false);

    assertFalse(defaultFilter.isAccepting());
    assertFalse(defaultFilter.accept(null));
    assertFalse(defaultFilter.accept(""));
    assertFalse(defaultFilter.accept("  "));
    assertFalse(defaultFilter.accept(Boolean.TRUE));
    assertFalse(defaultFilter.accept('a'));
    assertFalse(defaultFilter.accept(1));
    assertFalse(defaultFilter.accept(Math.PI));
    assertFalse(defaultFilter.accept("test"));
    assertFalse(defaultFilter.accept(new Object()));
  }

}
