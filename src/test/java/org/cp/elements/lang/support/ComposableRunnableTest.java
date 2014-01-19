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

package org.cp.elements.lang.support;

import static org.junit.Assert.*;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * The ComposableRunnableTest class is a test suite of test cases testing the contract and functionality of the
 * ComposableRunnable class.
 * <p/>
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see org.cp.elements.lang.support.ComposableRunnable
 * @see org.jmock.Mockery
 * @see org.jmock.lib.legacy.ClassImposteriser
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableRunnableTest {

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
  public void testCompose() {
    Runnable mockRunnableLeft = mockContext.mock(Runnable.class, "left");
    Runnable mockRunnableRight = mockContext.mock(Runnable.class, "right");

    assertNull(ComposableRunnable.compose(null, null));
    assertSame(mockRunnableLeft, ComposableRunnable.compose(mockRunnableLeft, null));
    assertSame(mockRunnableRight, ComposableRunnable.compose(null, mockRunnableRight));

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableLeft, mockRunnableRight);

    assertTrue(composedRunnable instanceof ComposableRunnable);
  }

  @Test
  public void testComposeRunnableArray() {
    Runnable mockRunnableZero = mockContext.mock(Runnable.class, "zero");
    Runnable mockRunnableOne = mockContext.mock(Runnable.class, "one");
    Runnable mockRunnableTwo = mockContext.mock(Runnable.class, "two");
    Runnable mockRunnableThree = mockContext.mock(Runnable.class, "three");

    assertNull(ComposableRunnable.compose((Runnable[]) null));
    assertSame(mockRunnableZero, ComposableRunnable.compose(mockRunnableZero));

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo,
      mockRunnableThree);

    assertTrue(composedRunnable instanceof ComposableRunnable);
  }

  @Test
  public void testRunOne() {
    final Runnable expectedRunnable = mockContext.mock(Runnable.class, "left");

    mockContext.checking(new Expectations() {{
      oneOf(expectedRunnable).run();
    }});

    Runnable actualRunnable = ComposableRunnable.compose(expectedRunnable);

    assertSame(expectedRunnable, actualRunnable);

    actualRunnable.run();
  }

  @Test
  public void testRunTwo() {
    final Runnable mockRunnableLeft = mockContext.mock(Runnable.class, "left");
    final Runnable mockRunnableRight = mockContext.mock(Runnable.class, "right");

    mockContext.checking(new Expectations() {{
      oneOf(mockRunnableLeft).run();
      oneOf(mockRunnableRight).run();
    }});

    Runnable composedRunnabe = ComposableRunnable.compose(mockRunnableLeft, mockRunnableRight);

    assertTrue(composedRunnabe instanceof ComposableRunnable);

    composedRunnabe.run();
  }

  @Test
  public void testRunFour() {
    final Runnable mockRunnableZero = mockContext.mock(Runnable.class, "zero");
    final Runnable mockRunnableOne = mockContext.mock(Runnable.class, "one");
    final Runnable mockRunnableTwo = mockContext.mock(Runnable.class, "two");
    final Runnable mockRunnableThree = mockContext.mock(Runnable.class, "three");

    mockContext.checking(new Expectations() {{
      oneOf(mockRunnableZero).run();
      oneOf(mockRunnableOne).run();
      oneOf(mockRunnableTwo).run();
      oneOf(mockRunnableThree).run();
    }});

    Runnable composedRunnable = ComposableRunnable.compose(mockRunnableZero, mockRunnableOne, mockRunnableTwo,
      mockRunnableThree);

    assertTrue(composedRunnable instanceof ComposableRunnable);

    composedRunnable.run();
  }

}
