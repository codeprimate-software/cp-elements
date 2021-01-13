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

package org.cp.elements.lang.support;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The DefaultFilterTests class is a test suite of test cases testing the contract and functionality
 * of the {@link DefaultFilter} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.DefaultFilter
 * @since 1.0.0
 */
public class DefaultFilterTests {

  @Test
  public void getInstanceWithTrueReturnsAccept() {
    assertThat(DefaultFilter.getInstance(true), is(sameInstance(DefaultFilter.ACCEPT)));
  }

  @Test
  public void getInstanceWithFalseReturnsReject() {
    assertThat(DefaultFilter.getInstance(false), is(sameInstance(DefaultFilter.REJECT)));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void accept() {
    DefaultFilter<Object> defaultFilter = DefaultFilter.ACCEPT;

    assertTrue(defaultFilter.isAccepting());
    assertTrue(defaultFilter.accept(null));
    assertTrue(defaultFilter.accept(Boolean.FALSE));
    assertTrue(defaultFilter.accept('\0'));
    assertTrue(defaultFilter.accept(0));
    assertTrue(defaultFilter.accept(-0.0d));
    assertTrue(defaultFilter.accept(""));
    assertTrue(defaultFilter.accept("  "));
    assertTrue(defaultFilter.accept("test"));
    assertTrue(defaultFilter.accept(new Object()));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void reject() {
    DefaultFilter<Object> defaultFilter = DefaultFilter.REJECT;

    assertFalse(defaultFilter.isAccepting());
    assertFalse(defaultFilter.accept(null));
    assertFalse(defaultFilter.accept(Boolean.TRUE));
    assertFalse(defaultFilter.accept('a'));
    assertFalse(defaultFilter.accept(1));
    assertFalse(defaultFilter.accept(Math.PI));
    assertFalse(defaultFilter.accept(""));
    assertFalse(defaultFilter.accept("  "));
    assertFalse(defaultFilter.accept("test"));
    assertFalse(defaultFilter.accept(new Object()));
  }
}
