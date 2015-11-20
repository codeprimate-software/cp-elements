/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.cp.elements.lang.LangExtensions.Is;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.LangExtensions.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;

import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ComparatorUtils;
import org.hamcrest.CoreMatchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The LangExtensionsTest class is a test suite of test cases testing the contract and functionality
 * of the LangExtensions class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.LangExtensions
 * @see org.cp.elements.test.TestUtils
 * @see org.junit.Rule
 * @see org.junit.Test
 * @since 1.0.0
 */
public class LangExtensionsTest {

  private final Comparable NULL = null;

  @Rule
  public ExpectedException expectedException = ExpectedException.none();

  private final Object lock = new Object();

  @Test
  public void assertThatClassIsAssignableToClassType() {
    assertThat(Boolean.class).isAssignableTo(Boolean.class);
    assertThat(Boolean.class).isAssignableTo(Object.class);
    assertThat(Character.class).isAssignableTo(Character.class);
    assertThat(Character.class).isAssignableTo(Object.class);
    assertThat(Double.class).isAssignableTo(Double.class);
    assertThat(Double.class).isAssignableTo(Number.class);
    assertThat(Double.class).isAssignableTo(Object.class);
    assertThat(Integer.class).isAssignableTo(Integer.class);
    assertThat(Integer.class).isAssignableTo(Number.class);
    assertThat(Integer.class).isAssignableTo(Object.class);
    assertThat(Number.class).isAssignableTo(Number.class);
    assertThat(Number.class).isAssignableTo(Object.class);
    assertThat(String.class).isAssignableTo(String.class);
    assertThat(String.class).isAssignableTo(Object.class);
    assertThat(Object.class).isAssignableTo(Object.class);
  }

  @Test
  public void assertThatClassIsNotAssignableToClassType() {
    assertThat(Boolean.class).not().isAssignableTo(Boolean.TYPE);
    assertThat(Character.class).not().isAssignableTo(String.class);
    assertThat(Float.class).not().isAssignableTo(Double.class);
    assertThat(Integer.class).not().isAssignableTo(Long.class);
    assertThat(String.class).not().isAssignableTo(Character.class);
    assertThat(Object.class).not().isAssignableTo(String.class);
  }

  @Test
  public void assertThatObjectIsAssignableToClassType() {
    assertThat(true).isAssignableTo(Boolean.class);
    assertThat('c').isAssignableTo(Character.class);
    assertThat(Math.PI).isAssignableTo(Double.class);
    assertThat(123).isAssignableTo(Integer.class);
    assertThat("test").isAssignableTo(String.class);
    assertThat(new Object()).isAssignableTo(Object.class);
  }

  @Test
  public void assertThatObjectIsNotAssignableToClassType() {
    assertThat("false").not().isAssignableTo(Boolean.class);
    assertThat("test").not().isAssignableTo(Character.class);
    assertThat(3.14159d).not().isAssignableTo(Float.class);
    assertThat(123.45f).not().isAssignableTo(Double.class);
    assertThat(123).not().isAssignableTo(Long.class);
    assertThat(123l).not().isAssignableTo(Integer.class);
    assertThat('c').not().isAssignableTo(String.class);
    assertThat(new Object()).not().isAssignableTo(String.class);
  }

  @Test
  public void assertThatDoubleIsAssignableToIntegerThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage(String.format("(%1$s) is not assignable to (java.lang.Integer)", Math.PI));

    assertThat(Math.PI).isAssignableTo(Integer.class);
  }

  @Test
  public void assertThatObjectIsAssignableToStringThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(Object.class).using("This is a %1$s{1}", "test", "!").isAssignableTo(String.class);
  }

  @Test
  public void assertThatCharacterIsAssignableToStringThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(Character.class).throwing(new IllegalArgumentException("test")).isAssignableTo(String.class);
  }

  @Test
  public void assertThatStringIsNotAssignableToObjectThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(class java.lang.String) is assignable to (java.lang.Object)");

    assertThat(String.class).not().isAssignableTo(Object.class);
  }

  @Test
  public void assertThatObjectIsComparableWithObject() {
    assertThat(true).isComparableTo(Boolean.TRUE);
    assertThat('c').isComparableTo('c');
    assertThat(3.14159d).isComparableTo(3.14159d);
    assertThat(123).isComparableTo(123);
    assertThat("test").isComparableTo("test");
  }

  @Test
  public void assertThatObjectIsNotComparableWithObject() {
    assertThat(Boolean.FALSE).not().isComparableTo(true);
    assertThat('c').not().isComparableTo('C');
    assertThat(3.14159d).not().isComparableTo(Math.PI);
    assertThat(123).not().isComparableTo(-123);
    assertThat("test").not().isComparableTo("mock");
  }

  @Test
  public void assertThatObjectsAreNotComparableThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(queue) is not comparable to (Q)");

    assertThat("queue").isComparableTo("Q");
  }

  @Test
  public void assertThatObjectsAreNotComparableThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat("c").using("This is a %1$s{1}", "test", "!").isComparableTo("see");
  }

  @Test
  public void assertThatObjectsAreNotComparableThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat("c").throwing(new IllegalArgumentException("test")).isComparableTo("see");
  }

  @Test
  public void assertThatComparableObjectsAreNotComparableThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is comparable to (test)");

    assertThat("test").not().isComparableTo("test");
  }

  @Test
  @SuppressWarnings("all")
  public void assertThatObjectsIsEqualToObject() {
    assertThat(true).isEqualTo(Boolean.TRUE);
    assertThat('c').isEqualTo(Character.valueOf('c'));
    assertThat(Math.PI).isEqualTo(Double.valueOf(Math.PI));
    assertThat(123).isEqualTo(Integer.valueOf(123));
    assertThat("test").isEqualTo("test");
  }

  @Test
  public void assertThatObjectIsNotEqualToObject() {
    assertThat(false).isNotEqualTo(Boolean.TRUE);
    assertThat('c').isNotEqualTo('C');
    assertThat(3.14159d).isNotEqualTo(Math.PI);
    assertThat(123).isNotEqualTo(-123);
    assertThat("test").isNotEqualTo("TEST");
  }

  @Test
  public void assertThatObjectIsEqualToObjectThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is not equal to (mock)");

    assertThat("test").isEqualTo("mock");
  }

  @Test
  public void assertThatObjectIsEqualToObjectThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat("r").using("This is a %1$s{1}", "test", "!").isEqualTo("are");
  }

  @Test
  public void assertThatObjectIsEqualToObjectThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat("r").throwing(new IllegalArgumentException("test")).isEqualTo("are");
  }

  @Test
  public void assertThatObjectIsNotEqualToObjectThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is equal to (test)");

    assertThat("test").isNotEqualTo("test");
  }

  @Test
  public void assertThatFalseIsFalse() {
    assertThat(false).isFalse();
    assertThat(Boolean.FALSE).isFalse();
  }

  @Test
  public void assertThatTrueIsNotFalse() {
    assertThat(true).not().isFalse();
  }

  @Test
  public void assertThatTrueIsFalseThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(true) is not false");

    assertThat(true).isFalse();
  }

  @Test
  public void assertThatTrueIsFalseThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(true).using("This is a %1$s{1}", "test", "!").isFalse();
  }

  @Test
  public void assertThatTrueIsFalseThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(true).throwing(new IllegalArgumentException("test")).isFalse();
  }

  @Test
  public void assertThatFalseIsNotFalseThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(false) is false");

    assertThat(false).not().isFalse();
  }

  @Test
  public void assertThatIsGreaterThan() {
    assertThat(2).isGreaterThan(-4);
    assertThat(2).isGreaterThan(1);
  }

  @Test
  public void assertThatIsNotGreaterThan() {
    assertThat(2).not().isGreaterThan(2);
    assertThat(2).not().isGreaterThan(3);
    assertThat(-2).not().isGreaterThan(1);
  }

  @Test
  public void assertThatIsGreaterThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not greater than (2)");

    assertThat(2).isGreaterThan(2);
  }

  @Test
  public void assertThatIsGreaterThanThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1$s{1}", "test", "!").isGreaterThan(3);
  }

  @Test
  public void assertThatIsGreaterThanThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThan(3);
  }

  @Test
  public void assertThatIsNotGreaterThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is greater than (1)");

    assertThat(2).not().isGreaterThan(1);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThan() {
    assertThat(2).isGreaterThanAndLessThan(-4, 4);
    assertThat(2).isGreaterThanAndLessThan(1, 3);
  }

  @Test
  public void assertThatIsNotGreaterThanAndLessThan() {
    assertThat(2).not().isGreaterThanAndLessThan(2, 2);
    assertThat(2).not().isGreaterThanAndLessThan(2, 3);
    assertThat(2).not().isGreaterThanAndLessThan(1, 2);
    assertThat(2).not().isGreaterThanAndLessThan(3, 5);
    assertThat(2).not().isGreaterThanAndLessThan(-1, 1);
    assertThat(2).not().isGreaterThanAndLessThan(3, 2);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not greater than (2) and less than (2)");

    assertThat(2).isGreaterThanAndLessThan(2, 2);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1$s{1}", "test", "!").isGreaterThanAndLessThan(2, 2);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThanAndLessThan(2, 2);
  }

  @Test
  public void assertThatIsNotGreaterThanAndLessThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is greater than (1) and less than (3)");

    assertThat(2).not().isGreaterThanAndLessThan(1, 3);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanEqualTo() {
    assertThat(2).isGreaterThanAndLessThanEqualTo(-4, 4);
    assertThat(2).isGreaterThanAndLessThanEqualTo(1, 3);
    assertThat(2).isGreaterThanAndLessThanEqualTo(1, 2);
  }

  @Test
  public void assertThatIsNotGreaterThanAndLessThanEqualTo() {
    assertThat(2).not().isGreaterThanAndLessThanEqualTo(3, 1);
    assertThat(2).not().isGreaterThanAndLessThanEqualTo(2, 1);
    assertThat(2).not().isGreaterThanAndLessThanEqualTo(1, 1);
    assertThat(2).not().isGreaterThanAndLessThanEqualTo(2, 3);
    assertThat(2).not().isGreaterThanAndLessThanEqualTo(3, 4);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not greater than (2) and less than equal to (2)");

    assertThat(2).isGreaterThanAndLessThanEqualTo(2, 2);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1$s{1}", "test", "!").isGreaterThanAndLessThanEqualTo(2, 2);
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThanAndLessThanEqualTo(2, 2);
  }

  @Test
  public void assertThatIsNotGreaterThanAndLessThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is greater than (1) and less than equal to (2)");

    assertThat(2).not().isGreaterThanAndLessThanEqualTo(1, 2);
  }

  @Test
  public void assertThatIsGreaterThanEqualTo() {
    assertThat(2).isGreaterThanEqualTo(-3);
    assertThat(2).isGreaterThanEqualTo(1);
    assertThat(2).isGreaterThanEqualTo(2);
  }

  @Test
  public void assertThatIsNotGreaterThanEqualTo() {
    assertThat(2).not().isGreaterThanEqualTo(3);
    assertThat(-2).not().isGreaterThanEqualTo(-1);
    assertThat(-2).not().isGreaterThanEqualTo(0);
    assertThat(-2).not().isGreaterThanEqualTo(1);
    assertThat(-2).not().isGreaterThanEqualTo(2);
  }

  @Test
  public void assertThatIsGreaterThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not greater than equal to (3)");

    assertThat(2).isGreaterThanEqualTo(3);
  }

  @Test
  public void assertThatIsGreaterThanEqualToThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1$s{1}", "test", "!").isGreaterThanEqualTo(3);
  }

  @Test
  public void assertThatIsGreaterThanEqualToThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThanEqualTo(3);
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is greater than equal to (1)");

    assertThat(2).not().isGreaterThanEqualTo(1);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThan() {
    assertThat(2).isGreaterThanEqualToAndLessThan(1, 3);
    assertThat(2).isGreaterThanEqualToAndLessThan(2, 3);
    assertThat(2).isGreaterThanEqualToAndLessThan(-4, 3);
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToAndLessThan() {
    assertThat(2).not().isGreaterThanEqualToAndLessThan(3, 1);
    assertThat(2).not().isGreaterThanEqualToAndLessThan(3, 4);
    assertThat(2).not().isGreaterThanEqualToAndLessThan(1, 1);
    assertThat(2).not().isGreaterThanEqualToAndLessThan(2, 1);
    assertThat(-2).not().isGreaterThanEqualToAndLessThan(1, -3);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not greater than equal to (3) and less than (1)");

    assertThat(2).isGreaterThanEqualToAndLessThan(3, 1);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1s{1}", "test", "!").isGreaterThanEqualToAndLessThan(3, 1);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThanEqualToAndLessThan(3, 1);
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToAndLessThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is greater than equal to (1) and less than (3)");

    assertThat(2).not().isGreaterThanEqualToAndLessThan(1, 3);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanEqualTo() {
    assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(1, 3);
    assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(2, 3);
    assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(1, 2);
    assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(2, 2);
    assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(-4, 4);
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToAndLessThanEqualTo() {
    assertThat(2).not().isGreaterThanEqualToAndLessThanEqualTo(3, 1);
    assertThat(-2).not().isGreaterThanEqualToAndLessThanEqualTo(1, -3);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not greater than equal to (3) and less than equal to (1)");

    assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(3, 1);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1$s{1}", "test", "!").isGreaterThanEqualToAndLessThanEqualTo(3, 1);
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThanEqualToAndLessThanEqualTo(3, 1);
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToAndLessThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is greater than equal to (1) and less than equal to (3)");

    assertThat(2).not().isGreaterThanEqualToAndLessThanEqualTo(1, 3);
  }

  @Test
  public void assertThatStringsHaveText() {
    assertThat("test").hasText();
    assertThat("null").isNotBlank();
    assertThat("empty").hasText();
    assertThat("123").isNotBlank();
    assertThat("0").hasText();
    assertThat("_").isNotBlank();
  }

  @Test
  public void assertThatStringsDoNotHaveText() {
    assertThat(null).not().hasText();
    assertThat("").not().isNotBlank();
    assertThat("  ").not().hasText();
    assertThat("\t").not().hasText();
    assertThat("\n").not().hasText();
  }

  @Test
  public void assertThatBlankStringHasTextThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("( ) is blank");

    assertThat(" ").hasText();
  }

  @Test
  public void assertThatEmptyStringHasTextThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat("").using("This is a %1$s{1}", "test", "!").isNotBlank();
  }

  @Test
  public void assertThatNullStringHasTextThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(null).throwing(new IllegalArgumentException("test")).hasText();
  }

  @Test
  public void assertThatStringDoesNotHaveTextThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is not blank");

    assertThat("test").not().hasText();
  }

  @Test
  public void assertThatCurrentThreadHoldsLock() {
    synchronized (lock) {
      assertThat(Thread.currentThread()).holdsLock(lock);
    }
  }

  @Test
  public void assertThatCurrentThreadDoesNotHoldLock() {
    assertThat(Thread.currentThread()).not().holdsLock(lock);
  }

  @Test
  public void assertThatCurrentThreadHoldsLockThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage(String.format("(%1$s) does not hold lock (%2$s)", Thread.currentThread(), lock));

    assertThat(Thread.currentThread()).holdsLock(lock);
  }

  @Test
  public void assertThatCurrentThreadHoldsLockThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(Thread.currentThread()).using("This is a %1$s{1}", "test", "!").holdsLock(lock);
  }

  @Test
  public void assertThatCurrentThreadHoldsLockThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(Thread.currentThread()).throwing(new IllegalArgumentException("test")).holdsLock(lock);
  }

  @Test
  public void assertThatCurrentThreadDoesNotHoldLockThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage(String.format("(%1$s) holds lock (%2$s)", Thread.currentThread(), lock));

    synchronized (lock) {
      assertThat(Thread.currentThread()).not().holdsLock(lock);
    }
  }

  @Test
  public void assertThatObjectIsInstanceOfClassType() {
    assertThat(true).isInstanceOf(Boolean.class);
    assertThat('c').isInstanceOf(Character.class);
    assertThat(3.14159d).isInstanceOf(Double.class);
    assertThat(Math.PI).isInstanceOf(Number.class);
    assertThat(2).isInstanceOf(Integer.class);
    assertThat(123).isInstanceOf(Number.class);
    assertThat("test").isInstanceOf(String.class);
    assertThat("test").isInstanceOf(Object.class);
    assertThat(new Object()).isInstanceOf(Object.class);
  }

  @Test
  public void assertThatObjectIsNotInstanceOfClassType() {
    assertThat("false").not().isInstanceOf(Boolean.class);
    assertThat("C").not().isInstanceOf(Character.class);
    assertThat(123).not().isInstanceOf(Double.class);
    assertThat(Math.PI).not().isInstanceOf(Integer.class);
    assertThat('a').not().isInstanceOf(String.class);
    assertThat(null).not().isInstanceOf(Object.class);
  }

  @Test
  public void assertThatStringIsInstanceOfCharacterThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is not an instance of (java.lang.Character)");

    assertThat("test").isInstanceOf(Character.class);
  }

  @Test
  public void assertThatStringIsInstanceOfCharacterThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat("test").using("This is a %1$s{1}", "test", "!").isInstanceOf(Character.class);
  }

  @Test
  public void assertThatStringIsInstanceOfCharacterThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat("test").throwing(new IllegalArgumentException("test")).isInstanceOf(Character.class);
  }

  @Test
  public void assertThatStringIsNotInstanceOfStringThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is an instance of (java.lang.String)");

    assertThat("test").not().isInstanceOf(String.class);
  }

  @Test
  public void assertThatIsLessThan() {
    assertThat(2).isLessThan(3);
    assertThat(-2).isLessThan(1);
  }

  @Test
  public void assertThatIsNotLessThan() {
    assertThat(2).not().isLessThan(2);
    assertThat(2).not().isLessThan(1);
    assertThat(2).not().isLessThan(0);
    assertThat(2).not().isLessThan(-1);
    assertThat(2).not().isLessThan(-2);
    assertThat(2).not().isLessThan(-3);
  }

  @Test
  public void assertThatIsLessThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not less than (1)");

    assertThat(2).isLessThan(1);
  }

  @Test
  public void assertThatIsLessThanThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1$s{1}", "test", "!").isLessThan(1);
  }

  @Test
  public void assertThatIsLessThanThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isLessThan(1);
  }

  @Test
  public void assertThatIsNotLessThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(1) is less than (2)");

    assertThat(1).not().isLessThan(2);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThan() {
    assertThat(2).isLessThanOrGreaterThan(-1, 1);
    assertThat(-2).isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanOrGreaterThan() {
    assertThat(-1).not().isLessThanOrGreaterThan(-1, 1);
    assertThat(0).not().isLessThanOrGreaterThan(-1, 1);
    assertThat(1).not().isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(0) is not less than (-1) or greater than (1)");

    assertThat(0).isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(0).using("This is a %1$s{1}", "test", "!").isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(0).throwing(new IllegalArgumentException("test")).isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanOrGreaterThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(3) is less than (-1) or greater than (1)");

    assertThat(3).not().isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanEqualTo() {
    assertThat(-2).isLessThanOrGreaterThanEqualTo(-1, 1);
    assertThat(1).isLessThanOrGreaterThanEqualTo(-1, 1);
    assertThat(2).isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanOrGreaterThanEqualTo() {
    assertThat(-1).not().isLessThanOrGreaterThanEqualTo(-1, 1);
    assertThat(0).not().isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(0) is not less than (-1) or greater than equal to (1)");

    assertThat(0).isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(0).using("This is a %1$s{1}", "test", "!").isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(0).throwing(new IllegalArgumentException("test")).isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanOrGreaterThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(1) is less than (-1) or greater than equal to (1)");

    assertThat(1).not().isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualTo() {
    assertThat(2).isLessThanEqualTo(3);
    assertThat(2).isLessThanEqualTo(2);
    assertThat(-2).isLessThanEqualTo(-1);
    assertThat(-2).isLessThanEqualTo(-2);
  }

  @Test
  public void assertThatIsNotLessThanEqualTo() {
    assertThat(2).not().isLessThanEqualTo(1);
    assertThat(2).not().isLessThanEqualTo(0);
    assertThat(-2).not().isLessThanEqualTo(-3);
  }

  @Test
  public void assertThatIsLessThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is not less than equal to (-3)");

    assertThat(2).isLessThanEqualTo(-3);
  }

  @Test
  public void assertThatIsLessThanEqualToThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(2).using("This is a %1$s{1}", "test", "!").isLessThanEqualTo(-3);
  }

  @Test
  public void assertThatIsLessThanEqualToThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(2).throwing(new IllegalArgumentException("test")).isLessThanEqualTo(-3);
  }

  @Test
  public void assertThatIsNotLessThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is less than equal to (3)");

    assertThat(2).not().isLessThanEqualTo(3);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThan() {
    assertThat(-2).isLessThanEqualToOrGreaterThan(-1, 1);
    assertThat(-1).isLessThanEqualToOrGreaterThan(-1, 1);
    assertThat(2).isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanEqualToOrGreaterThan() {
    assertThat(0).not().isLessThanEqualToOrGreaterThan(-1, 1);
    assertThat(1).not().isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(0) is not less than equal to (-1) or greater than (1)");

    assertThat(0).isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(0).using("This is a %1$s{1}", "test", "!").isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(0).throwing(new IllegalArgumentException("test")).isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanEqualToOrGreaterThanThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is less than equal to (-1) or greater than (1)");

    assertThat(2).not().isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanEqualTo() {
    assertThat(-2).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
    assertThat(-1).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
    assertThat(1).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
    assertThat(2).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanEqualToOrGreaterThanEqualTo() {
    assertThat(0).not().isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(0) is not less than equal to (-1) or greater than equal to (1)");

    assertThat(0).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(0).using("This is a %1$s{1}", "test", "!").isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(0).throwing(new IllegalArgumentException("test")).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatIsNotLessThanEqualToOrGreaterThanEqualToThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(2) is less than equal to (-1) or greater than equal to (1)");

    assertThat(2).not().isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void assertThatNullObjectIsNull() {
    assertThat(null).isNull();
    assertThat(null).not().isNotNull();
  }

  @Test
  public void assertThatNonNullObjectIsNotNull() {
    assertThat(new Object()).isNotNull();
    assertThat("test").not().isNull();
  }

  @Test
  public void assertThatNonNullObjectIsNullThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is not null");

    assertThat("test").isNull();
  }

  @Test
  public void assertThatNonNullObjectIsNullThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat("test").using("This is a %1$s{1}", "test", "!").isNull();
  }

  @Test
  public void assertThatNonNullObjectIsNullThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat("test").throwing(new IllegalArgumentException("test")).isNull();
  }

  @Test
  public void assertThatNullObjectIsNotNullThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(null) is null");

    assertThat(null).isNotNull();
  }

  @Test
  public void assertThatNonEmptyStringsAreNotEmpty() {
    assertThat("test").isNotEmpty();
    assertThat("null").isNotEmpty();
    assertThat("empty").isNotEmpty();
    assertThat("0").isNotEmpty();
    assertThat("  ").isNotEmpty();
    assertThat("_").isNotEmpty();
    assertThat("-").isNotEmpty();
    assertThat("*").isNotEmpty();
    assertThat("!").isNotEmpty();
    assertThat("\t").isNotEmpty();
    assertThat("\n").isNotEmpty();
  }

  @Test
  public void assertThatEmptyStringIsEmpty() {
    assertThat("").not().isNotEmpty();
  }

  @Test
  public void assertThatEmptyStringIsNotEmptyThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("() is empty");

    assertThat("").isNotEmpty();
  }

  @Test
  public void assertThatEmptyStringIsNotEmptyThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat("").using("This is a %1$s{1}", "test", "!").isNotBlank();
  }

  @Test
  public void assertThatEmptyStringIsNotEmptyThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat("").throwing(new IllegalArgumentException("test")).isNotBlank();
  }

  @Test
  public void assertThatNonEmptyStringIsEnptyThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("( ) is not empty");

    assertThat(" ").not().isNotEmpty();
  }

  @Test
  public void assertThatObjectIsSameAsObject() {
    assertThat(true).isSameAs(Boolean.TRUE);
    assertThat('c').isSameAs('c');
    //assertThat(Math.PI).isSameAs(Math.PI);
    assertThat(1).isSameAs(1);
    assertThat("test").isSameAs("test");
  }

  @Test
  public void assertThatObjectIsNotSameAsObject() {
    assertThat(false).isNotSameAs(Boolean.TRUE);
    assertThat('c').isNotSameAs('C');
    assertThat(3.14159d).isNotSameAs(Math.PI);
    assertThat(123).isNotSameAs(-123);
    assertThat("test").isNotSameAs("TEST");
  }

  @Test
  public void assertThatNonIdenticalObjectsAreTheSameThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is not the same as (TEST)");

    assertThat("test").isSameAs("TEST");
  }

  @Test
  public void assertThatNonIdenticalObjectsAreTheSameThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat("test").using("This is a %1$s{1}", "test", "!").isSameAs("TEST");
  }

  @Test
  public void assertThatNonIdenticalObjectsAreTheSameThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat("test").throwing(new IllegalArgumentException("test")).isSameAs("TEST");
  }

  @Test
  public void assertThatIdenticalObjectsAreNotTheSameThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(test) is the same as (test)");

    assertThat("test").isNotSameAs("test");
  }

  @Test
  public void assertThatTrueIsTrue() {
    assertThat(true).isTrue();
    assertThat(Boolean.TRUE).isTrue();
  }

  @Test
  public void assertThatFalseIsNotTrue() {
    assertThat(false).not().isTrue();
  }

  @Test
  public void assertThatFalseIsTrueThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(false) is not true");

    assertThat(false).isTrue();
  }

  @Test
  public void assertThatFalseIsTrueThrowsAssertionErrorWithCustomMessage() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("This is a test!");

    assertThat(false).using("This is a %1$s{1}", "test", "!").isTrue();
  }

  @Test
  public void assertThatFalseIsTrueThrowsIllegalArgumentException() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(false).throwing(new IllegalArgumentException("test")).isTrue();
  }

  @Test
  public void assertThatTrueIsNotTrueThrowsAssertionError() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(true) is true");

    assertThat(true).not().isTrue();
  }

  @Test
  public void isAssignableTo() {
    assertTrue(is(Object.class).assignableTo(Object.class));
    assertTrue(is(Boolean.class).assignableTo(Object.class));
    assertTrue(is(Character.class).assignableTo(Object.class));
    assertTrue(is(Number.class).assignableTo(Object.class));
    assertTrue(is(String.class).assignableTo(Object.class));
    assertTrue(is(Double.class).assignableTo(Number.class));
    assertTrue(is(Integer.class).assignableTo(Number.class));
    assertTrue(is(java.sql.Date.class).assignableTo(java.util.Date.class));
    assertTrue(is(Object.class).not().assignableTo(String.class));
  }

  @Test
  public void isNotAssignableTo() {
    assertFalse(is(Object.class).assignableTo(String.class));
    assertFalse(is(Boolean.TYPE).assignableTo(Boolean.class));
    assertFalse(is(Double.class).assignableTo(BigDecimal.class));
    assertFalse(is(Integer.class).assignableTo(BigInteger.class));
    assertFalse(is(Integer.class).assignableTo(Long.class));
    assertFalse(is(String.class).assignableTo(BigDecimal.class));
    assertFalse(is(String.class).assignableTo(Character.class));
    assertFalse(is(Character.class).assignableTo(String.class));
  }

  @Test
  public void testIsEqualByComparison() {
    final Person jonDoe1 = new Person(1l, "Jon", "Doe");
    final Person jonDoe2 = new Person(2l, "Jon", "Doe");

    assertTrue(is(jonDoe1).comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).not().comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).equalTo(jonDoe2));
  }

  @Test
  public void testIsNotEqualByComparison() {
    final Person johnBlum = new Person(1l, "John", "Blum");
    final Person jonBloom = new Person(1l, "Jon", "Bloom");

    assertFalse(is(johnBlum).comparableTo(jonBloom));
    assertTrue(is(johnBlum).not().comparableTo(jonBloom));
    assertTrue(is(johnBlum).equalTo(jonBloom));
  }

  @Test
  public void testIsEqualTo() {
    assertTrue(is(true).equalTo(Boolean.TRUE));
    assertTrue(is('c').equalTo('c'));
    assertTrue(is(2).equalTo(2));
    assertTrue(is(Math.PI).equalTo(Math.PI));
    assertTrue(is("test").equalTo("test"));
  }

  @Test
  public void testIsEqualToWithUnequalValues() {
    assertFalse(is(NULL).equalTo(NULL));
    assertFalse(is("null").equalTo(null));
    assertFalse(is("null").equalTo("nil"));
    assertFalse(is(Boolean.FALSE).equalTo(Boolean.TRUE));
    assertFalse(is((Comparable) 'c').equalTo("c"));
    assertFalse(is(-2).equalTo(2));
    assertFalse(is(3.14159d).equalTo(Math.PI));
    assertFalse(is("test").equalTo("TEST"));
  }

  @Test
  public void testIsNotEqualTo() {
    assertTrue((is(NULL).not().equalTo(NULL)));
    assertTrue(is((String) null).not().equalTo("null"));
    assertTrue(is("null").not().equalTo(null));
    assertTrue(is("null").not().equalTo("nil"));
    assertTrue(is(Boolean.FALSE).not().equalTo(Boolean.TRUE));
    assertTrue(is((Comparable) 'c').not().equalTo("c"));
    assertTrue(is(-2).not().equalTo(2));
    assertTrue(is(3.14159d).not().equalTo(Math.PI));
    assertTrue(is("test").not().equalTo("TEST"));
    assertTrue(is(TestUtils.createCalendar(2011, Calendar.OCTOBER, 13)).not().equalTo(Calendar.getInstance()));
  }

  @Test
  public void testIsFalse() {
    assertTrue(is(false).False());
    assertTrue(is(Boolean.FALSE).False());
    assertTrue(is(!Boolean.TRUE).False());
  }

  @Test
  public void testIsNotFalse() {
    assertTrue(is(true).not().False());
    assertTrue(is(Boolean.TRUE).not().False());
    assertTrue(is(!Boolean.FALSE).not().False());
    assertTrue(is((Boolean) null).not().False());
  }

  @Test
  public void testIsGreaterThan() {
    assertTrue(is(3).greaterThan(1));
    assertTrue(is(3).greaterThan(2));
    assertFalse(is(3).greaterThan(3));
    assertFalse(is(3).greaterThan(4));
    assertFalse(is(3).greaterThan(5));
  }

  @Test
  public void testIsNotGreaterThan() {
    assertFalse(is(3).not().greaterThan(1));
    assertFalse(is(3).not().greaterThan(2));
    assertTrue(is(3).not().greaterThan(3));
    assertTrue(is(3).not().greaterThan(4));
    assertTrue(is(3).not().greaterThan(5));
  }

  @Test
  public void testIsGreaterThanAndLessThan() {
    assertFalse(is(1).greaterThanAndLessThan(2, 4));
    assertFalse(is(2).greaterThanAndLessThan(2, 4));
    assertTrue(is(3).greaterThanAndLessThan(2, 4));
    assertFalse(is(4).greaterThanAndLessThan(2, 4));
    assertFalse(is(5).greaterThanAndLessThan(2, 4));
  }

  @Test
  public void testIsNotGreaterThanAndLessThan() {
    assertTrue(is(1).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(2).not().greaterThanAndLessThan(2, 4));
    assertFalse(is(3).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(4).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(5).not().greaterThanAndLessThan(2, 4));
  }

  @Test
  public void testIsGreaterThanAndLessThanEqualTo() {
    assertFalse(is(1).greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(2).greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(3).greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(4).greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(5).greaterThanAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotGreaterThanAndLessThanEqualTo() {
    assertTrue(is(1).not().greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(2).not().greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(3).not().greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(4).not().greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(5).not().greaterThanAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsGreaterThanEqualTo() {
    assertTrue(is(3).greaterThanEqualTo(1));
    assertTrue(is(3).greaterThanEqualTo(2));
    assertTrue(is(3).greaterThanEqualTo(3));
    assertFalse(is(3).greaterThanEqualTo(4));
    assertFalse(is(3).greaterThanEqualTo(5));
  }

  @Test
  public void testIsNotGreaterThanEqualTo() {
    assertFalse(is(3).not().greaterThanEqualTo(1));
    assertFalse(is(3).not().greaterThanEqualTo(2));
    assertFalse(is(3).not().greaterThanEqualTo(3));
    assertTrue(is(3).not().greaterThanEqualTo(4));
    assertTrue(is(3).not().greaterThanEqualTo(5));
  }

  @Test
  public void testIsGreaterThanEqualToAndLessThan() {
    assertFalse(is(1).greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(2).greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(3).greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(4).greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(5).greaterThanEqualToAndLessThan(2, 4));
  }

  @Test
  public void testIsNotGreaterThanEqualToAndLessThan() {
    assertTrue(is(1).not().greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(2).not().greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(3).not().greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(4).not().greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(5).not().greaterThanEqualToAndLessThan(2, 4));
  }

  @Test
  public void testIsGreaterThanEqualToAndLessThanEqualTo() {
    assertFalse(is(1).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(2).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(3).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(4).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(5).greaterThanEqualToAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotGreaterThanEqualToAndLessThanEqualTo() {
    assertTrue(is(1).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(2).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(3).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(4).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(5).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
  }

  @Test
  public void testIsInstanceOf() {
    assertTrue(is(false).instanceOf(Boolean.class));
    assertTrue(is(Boolean.TRUE).instanceOf(Boolean.class));
    assertTrue(is(Calendar.getInstance()).instanceOf(Calendar.class));
    assertTrue(is('c').instanceOf(Character.class));
    assertTrue(is('0').instanceOf(Character.class));
    assertTrue(is(2).instanceOf(Number.class));
    assertTrue(is(0).instanceOf(Integer.class));
    assertTrue(is(Math.PI).instanceOf(Number.class));
    assertTrue(is(0.0d).instanceOf(Double.class));
    assertTrue(is("null").instanceOf(String.class));
    assertTrue(is("test").instanceOf(Object.class));
  }

  @Test
  public void testIsNotInstanceOf() {
    assertTrue(is("false").not().instanceOf(Boolean.class));
    assertTrue(is(Calendar.getInstance()).not().instanceOf(Date.class));
    assertTrue(is("c").not().instanceOf(Character.class));
    assertTrue(is(Math.PI).not().instanceOf(Integer.class));
    assertTrue(is(2).not().instanceOf(Double.class));
    assertTrue(is('c').not().instanceOf(String.class));
    assertTrue(is(NULL).not().instanceOf(Object.class));
    assertTrue(is("test").not().instanceOf(null));
  }

  @Test
  public void testIsLessThan() {
    assertFalse(is(3).lessThan(1));
    assertFalse(is(3).lessThan(2));
    assertFalse(is(3).lessThan(3));
    assertTrue(is(3).lessThan(4));
    assertTrue(is(3).lessThan(5));
  }

  @Test
  public void testIsNotLessThan() {
    assertTrue(is(3).not().lessThan(1));
    assertTrue(is(3).not().lessThan(2));
    assertTrue(is(3).not().lessThan(3));
    assertFalse(is(3).not().lessThan(4));
    assertFalse(is(3).not().lessThan(5));
  }

  @Test
  public void testIsLessThanOrGreaterThan() {
    assertTrue(is(1).lessThanOrGreaterThan(2, 4));
    assertFalse(is(2).lessThanOrGreaterThan(2, 4));
    assertFalse(is(3).lessThanOrGreaterThan(2, 4));
    assertFalse(is(4).lessThanOrGreaterThan(2, 4));
    assertTrue(is(5).lessThanOrGreaterThan(2, 4));
  }

  @Test
  public void testIsNotLessThanOrGreaterThan() {
    assertFalse(is(1).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(2).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(3).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(4).not().lessThanOrGreaterThan(2, 4));
    assertFalse(is(5).not().lessThanOrGreaterThan(2, 4));
  }

  @Test
  public void testIsLessThanOrGreaterThanEqualTo() {
    assertTrue(is(1).lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(2).lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(3).lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(4).lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(5).lessThanOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotLessThanOrGreaterThanEqualTo() {
    assertFalse(is(1).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(2).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(3).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(4).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(5).not().lessThanOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsLessThanEqualTo() {
    assertFalse(is(3).lessThanEqualTo(1));
    assertFalse(is(3).lessThanEqualTo(2));
    assertTrue(is(3).lessThanEqualTo(3));
    assertTrue(is(3).lessThanEqualTo(4));
    assertTrue(is(3).lessThanEqualTo(5));
  }

  @Test
  public void testIsNotLessThanEqualTo() {
    assertTrue(is(3).not().lessThanEqualTo(1));
    assertTrue(is(3).not().lessThanEqualTo(2));
    assertFalse(is(3).not().lessThanEqualTo(3));
    assertFalse(is(3).not().lessThanEqualTo(4));
    assertFalse(is(3).not().lessThanEqualTo(5));
  }

  @Test
  public void testIsLessThanEqualToOrGreaterThan() {
    assertTrue(is(1).lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(2).lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(3).lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(4).lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(5).lessThanEqualToOrGreaterThan(2, 4));
  }

  @Test
  public void testIsNotLessThanEqualToOrGreaterThan() {
    assertFalse(is(1).not().lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(2).not().lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(3).not().lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(4).not().lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(5).not().lessThanEqualToOrGreaterThan(2, 4));
  }

  @Test
  public void testIsLessThanEqualToOrGreaterThanEqualTo() {
    assertTrue(is(1).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(2).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(3).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(4).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(5).lessThanEqualToOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotLessThanEqualToOrGreaterThanEqualTo() {
    assertFalse(is(1).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(2).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(3).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(4).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(5).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void testIsNotNull() {
    assertTrue(is(Boolean.FALSE).notNull());
    assertTrue(is(Calendar.getInstance()).notNull());
    assertTrue(is('\0').notNull());
    assertTrue(is(Integer.MIN_VALUE).notNull());
    assertTrue(is(Double.MAX_VALUE).notNull());
    assertTrue(is("null").notNull());
  }

  @Test
  public void testIsNull() {
    assertTrue(is(NULL).Null());
  }

  @Test
  public void testIsSameAs() {
    assertTrue(is(NULL).sameAs(NULL));
    assertTrue(is(Boolean.TRUE).sameAs(Boolean.TRUE));
    assertTrue(is("test").sameAs("test"));
  }

  @Test
  @SuppressWarnings("all")
  public void testIsNotSameAs() {
    assertTrue(is(NULL).notSameAs("null"));
    assertTrue(is(Boolean.TRUE).notSameAs(Boolean.FALSE));
    assertTrue(is("test").notSameAs(new String("test")));
  }

  @Test
  public void testIsTrue() {
    assertTrue(is(true).True());
    assertTrue(is(Boolean.TRUE).True());
    assertTrue(is(!Boolean.FALSE).True());
  }

  @Test
  public void testIsNotTrue() {
    assertTrue(is(false).not().True());
    assertTrue(is(Boolean.FALSE).not().True());
    assertTrue(is(!Boolean.TRUE).not().True());
    assertTrue(is((Boolean) null).not().True());
  }

  @Test
  public void testIsReuse() {
    final Is<String> isOperator = is("test");

    assertNotNull(isOperator);
    assertTrue(isOperator.equalTo("test"));
    assertFalse(isOperator.equalTo("testing"));
    assertTrue(isOperator.not().equalTo("testing"));
    assertTrue(isOperator.equalTo("test"));
    assertFalse(isOperator.equalTo("TEST"));
    assertTrue(isOperator.not().equalTo("TEST"));
    assertTrue(isOperator.equalTo("test"));
  }

  @Test
  public void testIsDoubleNegative() {
    assertTrue(is(true).not().not().True());
    assertTrue(is(false).not().not().False());
  }

  protected static class Person implements Comparable<Person> {

    private final Long id;

    private final String firstName;
    private final String lastName;

    public Person(final Long id, final String firstName, final String lastName) {
      this.id = id;
      this.firstName = firstName;
      this.lastName = lastName;
    }

    public Long getId() {
      return id;
    }

    public String getFirstName() {
      return firstName;
    }

    public String getLastName() {
      return lastName;
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(final Person person) {
      int compareValue = ComparatorUtils.compareIgnoreNull(getFirstName(), person.getFirstName());
      return (compareValue != 0 ? compareValue : ComparatorUtils.compareIgnoreNull(getLastName(), person.getLastName()));
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      final Person that = (Person) obj;

      return (ObjectUtils.equalsIgnoreNull(this.getId(), that.getId()));
        //&& ObjectUtils.equals(this.getFirstName(), that.getFirstName())
        //&& ObjectUtils.equals(this.getLastName(), that.getLastName()));
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getId());
      //hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getFirstName());
      //hashValue = 37 * hashValue + ObjectUtils.hashCode(this.getLastName());
      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("{ id = %1$s, firstName = %2$s, lastName = %3$s }", getId(), getFirstName(), getLastName());
    }

  }

}
