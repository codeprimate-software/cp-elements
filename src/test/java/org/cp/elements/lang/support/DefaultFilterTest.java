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
 *  <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.support;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The DefaultFilterTest class is a test suite of test cases testing the contract and functionality
 * of the DefaultFilter class.
 * <p/>
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
