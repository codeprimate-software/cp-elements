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

package org.cp.elements.lang;

import static org.cp.elements.lang.LangExtensions.AssertThat;
import static org.cp.elements.lang.LangExtensions.AssertThatWrapper;
import static org.cp.elements.lang.LangExtensions.Is;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.LangExtensions.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import java.util.concurrent.atomic.AtomicInteger;

import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ComparatorUtils;
import org.hamcrest.CoreMatchers;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.Matchers;

/**
 * The LangExtensionsTest class is a test suite of test cases testing the contract and functionality
 * of the LangExtensions class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.LangExtensions
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
public class LangExtensionsTest {

  private final Comparable NULL = null;

  private final Condition ENABLE_DISABLE_CONDITION = Condition.FALSE_CONDITION;

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
  public void assertionTransformationIgnoresCondition() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("(false) is not true");

    assertThat(false).transform(new Transformer<AssertThat<Boolean>>() {
      @Override public AssertThat<Boolean> transform(final AssertThat<Boolean> assertion) {
        return new AssertThatWrapper<Boolean>(assertion) {
          @Override public AssertThat<Boolean> when(final Condition condition) {
            assertion.when(Condition.TRUE_CONDITION);
            return this;
          }
        };
      }
    }).when(ENABLE_DISABLE_CONDITION).isTrue();
  }

  @Test
  public void disabledAssertThatIsAssignableToSuppressesAssertionError() {
    assertThat(1).when(ENABLE_DISABLE_CONDITION).isAssignableTo(Boolean.class);
  }

  @Test
  public void disabledAssertThatIsComparableToSuppressesAssertionError() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isComparableTo("TEST");
  }

  @Test
  public void disabledAssertThatIsEqualToSuppressesAssertionError() {
    assertThat(3.1459d).when(ENABLE_DISABLE_CONDITION).isEqualTo(Math.PI);
  }

  @Test
  public void disabledAssertThatIsNotEqualToSuppressesAssertionError() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isNotEqualTo("test");
  }

  @Test
  public void disabledAssertThatIsFalseSuppressesAssertionError() {
    assertThat(true).when(ENABLE_DISABLE_CONDITION).isFalse();
  }

  @Test
  public void disabledAssertThatIsGreaterThanSuppressesAssertionError() {
    assertThat(-2).when(ENABLE_DISABLE_CONDITION).isGreaterThan(1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanAndLessThanSuppressesAssertionError() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanAndLessThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanAndLessThanEqualToSuppressesAssertionError() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanAndLessThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanEqualToSuppressesAssertionError() {
    assertThat(-2).when(ENABLE_DISABLE_CONDITION).isGreaterThanEqualTo(1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanEqualToAndLessThanSuppressesAssertionError() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanEqualToAndLessThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanEqualToAndLessThanEqualToSuppressesAssertionError() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanEqualToAndLessThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatHasTextSuppressesAssertionError() {
    assertThat("  ").when(ENABLE_DISABLE_CONDITION).hasText();
  }

  @Test
  public void disabledAssertThatHoldsLockSuppressesAssertionError() {
    assertThat(Thread.currentThread()).when(ENABLE_DISABLE_CONDITION).holdsLock(lock);
  }

  @Test
  public void disabledAssertThatIsInstanceOfSuppressesAssertionError() {
    assertThat(null).when(ENABLE_DISABLE_CONDITION).isInstanceOf(Object.class);
  }

  @Test
  public void disabledAssertThatIsLessThanSuppressesAssertionError() {
    assertThat(1).when(ENABLE_DISABLE_CONDITION).isLessThan(-2);
  }

  @Test
  public void disabledAssertThatIsLessThanOrGreaterThanSuppressesAssertinError() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsLessThanOrGreaterThanEqualToSuppressesAssertinError() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatIsLessThanEqualToSuppressesAssertionError() {
    assertThat(1).when(ENABLE_DISABLE_CONDITION).isLessThanEqualTo(-1);
  }

  @Test
  public void disabledAssertThatIsLessThanEqualToOrGreaterThanSuppressesAssertionError() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsLessThanEqualToOrGreaterThanEqualToSuppressesAssertionError() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatIsNotBlankSuppressesAssertionError() {
    assertThat(" ").when(ENABLE_DISABLE_CONDITION).isNotBlank();
  }

  @Test
  public void disabledAssertThatIsNotEmptySuppressesAssertionError() {
    assertThat("").when(ENABLE_DISABLE_CONDITION).isNotEmpty();
  }

  @Test
  public void disabledAssertThatIsNotNullSuppressesAssertoinError() {
    assertThat(null).when(ENABLE_DISABLE_CONDITION).isNotNull();
  }

  @Test
  public void disabledAssertThatIsNullSuppressesAssertionError() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isNull();
  }

  @Test
  public void disabledAssertThatIsSameAsSuppressesAssertionError() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isSameAs("TEST");
  }

  @Test
  public void disabledAssertThatIsNotSameAsSuppressesAssertionError() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isNotSameAs("test");
  }

  @Test
  public void disabledAssertThatIsTrueSuppressesAssertionError() {
    assertThat(false).when(ENABLE_DISABLE_CONDITION).isTrue();
  }

  @Test
  public void negatedAssertThatRetainsThrowable() {
    expectedException.expect(IllegalArgumentException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("test");

    assertThat(true).throwing(new IllegalArgumentException("test")).not().isTrue();
  }

  @Test
  public void negatedAssertThatRetainsTransformer() {
    AtomicInteger holdsLockCallCount = new AtomicInteger(0);

    Object lock = new Object();

    Transformer<AssertThat<Thread>> assertThatTransformer = (AssertThat<Thread> assertion) ->
      new AssertThatWrapper<Thread>(assertion) {
        @Override public void holdsLock(final Object lock) {
          holdsLockCallCount.incrementAndGet();
          assertion.holdsLock(lock);
        }
      };

    assertThat(Thread.currentThread()).transform(assertThatTransformer).not().holdsLock(lock);

    assertEquals(1, holdsLockCallCount.get());
  }

  @Test
  public void negatedAssertThatRetainsExceptionMessageAndArgs() {
    expectedException.expect(AssertionFailedException.class);
    expectedException.expectCause(CoreMatchers.is(nullValue(Throwable.class)));
    expectedException.expectMessage("Object cannot be null");

    assertThat(null).using("%1$s cannot be {1}", "Object", "null").isNotNull();
  }

  @Test
  public void negatedAssertThatRetainsCondition() {
    assertThat(false).when(Condition.FALSE_CONDITION).not().isFalse();
  }

  @Test
  public void wrappedAssertThatIsAssignableToDelegatesToWrappedAssertion() {
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isAssignableTo(Object.class);
    verify(mockAssertion, times(1)).isAssignableTo(eq(Object.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsComparableToDelegatesToWrappedAssertion() {
    Comparable mockComparable = mock(Comparable.class);
    AssertThat<Comparable> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isComparableTo(mockComparable);
    verify(mockAssertion, times(1)).isComparableTo(eq(mockComparable));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsEqualToDelegatesToWrappedAssertion() {
    Object obj = new Object();
    AssertThat<Object> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isEqualTo(obj);
    verify(mockAssertion, times(1)).isEqualTo(eq(obj));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsNotEqualToDelegatesToWrappedAssertion() {
    Object obj = new Object();
    AssertThat<Object> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isNotEqualTo(obj);
    verify(mockAssertion, times(1)).isNotEqualTo(eq(obj));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsFalseDelegatesToWrappedAssertion() {
    AssertThat<Boolean> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isFalse();
    verify(mockAssertion, times(1)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isGreaterThan(0);
    verify(mockAssertion, times(1)).isGreaterThan(eq(0));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanOrLessThanDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isGreaterThanAndLessThan(-1, 1);
    verify(mockAssertion, times(1)).isGreaterThanAndLessThan(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanOrLessThanEqualToDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isGreaterThanAndLessThanEqualTo(-1, 1);
    verify(mockAssertion, times(1)).isGreaterThanAndLessThanEqualTo(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanEqualToDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isGreaterThanEqualTo(0);
    verify(mockAssertion, times(1)).isGreaterThanEqualTo(eq(0));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanEqualToAndLessThanDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isGreaterThanEqualToAndLessThan(-1, 1);
    verify(mockAssertion, times(1)).isGreaterThanEqualToAndLessThan(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanEqualToAndLessThanEqualToDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isGreaterThanEqualToAndLessThanEqualTo(-1, 1);
    verify(mockAssertion, times(1)).isGreaterThanEqualToAndLessThanEqualTo(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatHasTextDelegatesToWrappedAssertion() {
    AssertThat<String> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).hasText();
    verify(mockAssertion, times(1)).hasText();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatHoldsLockDelegatesToWrappedAssertion() {
    AssertThat<Thread> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).holdsLock(lock);
    verify(mockAssertion, times(1)).holdsLock(eq(lock));
  }

  @Test
  public void wrappedAssertThatIsInstanceOfDelegatesToWrappedAssertion() {
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isInstanceOf(Object.class);
    verify(mockAssertion, times(1)).isInstanceOf(eq(Object.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isLessThan(0);
    verify(mockAssertion, times(1)).isLessThan(eq(0));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanOrGreaterThanDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isLessThanOrGreaterThan(-1, 1);
    verify(mockAssertion, times(1)).isLessThanOrGreaterThan(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanOrGreaterThanEqualToDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isLessThanOrGreaterThanEqualTo(-1, 1);
    verify(mockAssertion, times(1)).isLessThanOrGreaterThanEqualTo(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanEqualToDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isLessThanEqualTo(0);
    verify(mockAssertion, times(1)).isLessThanEqualTo(eq(0));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanEqualToOrGreaterThanDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isLessThanEqualToOrGreaterThan(-1, 1);
    verify(mockAssertion, times(1)).isLessThanEqualToOrGreaterThan(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanEqualToOrGreaterThanEqualToDelegatesToWrappedAssertion() {
    AssertThat<Integer> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
    verify(mockAssertion, times(1)).isLessThanEqualToOrGreaterThanEqualTo(eq(-1), eq(1));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsNotBlankDelegatesToWrappedAssertion() {
    AssertThat<String> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isNotBlank();
    verify(mockAssertion, times(1)).isNotBlank();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsNotEmptyDelegatesToWrappedAssertion() {
    AssertThat<String> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isNotEmpty();
    verify(mockAssertion, times(1)).isNotEmpty();
  }

  @Test
  public void wrappedAssertThatIsNotNullDelegatesToWrappedAssertion() {
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isNotNull();
    verify(mockAssertion, times(1)).isNotNull();
  }

  @Test
  public void wrappedAssertThatIsNullDelegatesToWrappedAssertion() {
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isNull();
    verify(mockAssertion, times(1)).isNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsNotSameAsDelegatesToWrappedAssertion() {
    Object obj = new Object();
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isNotSameAs(obj);
    verify(mockAssertion, times(1)).isNotSameAs(eq(obj));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsSameAsDelegatesToWrappedAssertion() {
    Object obj = new Object();
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isSameAs(obj);
    verify(mockAssertion, times(1)).isSameAs(eq(obj));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsTrueDelegatesToWrappedAssertion() {
    AssertThat<Boolean> mockAssertion = mock(AssertThat.class);
    AssertThatWrapper.wrap(mockAssertion).isTrue();
    verify(mockAssertion, times(1)).isTrue();
  }

  @Test
  public void wrappedAssertThatNotDelegatesToWrappedAssertion() {
    AssertThat mockAssertion = mock(AssertThat.class);

    when(mockAssertion.not()).thenReturn(mockAssertion);

    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).not();

    assertNotSame(mockAssertion, wrappedAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).not();
  }

  @Test
  public void wrappedAssertThatThrowingDelegatesToWrappedAssertion() {
    RuntimeException illegalArgument = new IllegalArgumentException("test");

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).throwing(illegalArgument);

    assertNotSame(mockAssertion, wrappedAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).throwing(eq(illegalArgument));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatTransformDelegatesToWrappedAssertion() {
    Transformer<AssertThat> mockTransformer = mock(Transformer.class);

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion);

    when(mockTransformer.transform(Matchers.any(AssertThat.class))).thenReturn(wrappedAssertion);

    wrappedAssertion = wrappedAssertion.transform(mockTransformer);

    assertNotSame(mockAssertion, wrappedAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockTransformer, times(1)).transform(eq(mockAssertion));
  }

  @Test
  public void wrappedAssertThatUsingDelegatesToWrappedAssertion() {
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).using("message", "args");

    assertNotSame(mockAssertion, wrappedAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).using(eq("message"), eq("args"));
  }

  @Test
  public void wrappedAssertThatWhenDelegatesToWrappedAssertion() {
    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).when(ENABLE_DISABLE_CONDITION);

    assertNotSame(mockAssertion, wrappedAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).when(eq(ENABLE_DISABLE_CONDITION));
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
  }

  @Test
  public void isAssignableToIsFalse() {
    assertFalse(is(Object.class).assignableTo(String.class));
    assertFalse(is(Boolean.TYPE).assignableTo(Boolean.class));
    assertFalse(is(Double.class).assignableTo(BigDecimal.class));
    assertFalse(is(Integer.class).assignableTo(BigInteger.class));
    assertFalse(is(Float.class).assignableTo(Double.class));
    assertFalse(is(Integer.class).assignableTo(Long.class));
    assertFalse(is(String.class).assignableTo(BigDecimal.class));
    assertFalse(is(String.class).assignableTo(Character.class));
    assertFalse(is(Character.class).assignableTo(String.class));
  }

  @Test
  public void isNotAssignableTo() {
    assertTrue(is(Boolean.TYPE).not().assignableTo(Boolean.class));
    assertTrue(is(Character.class).not().assignableTo(String.class));
    assertTrue(is(Double.TYPE).not().assignableTo(Double.class));
    assertTrue(is(Integer.TYPE).not().assignableTo(Integer.class));
    assertTrue(is(Object.class).not().assignableTo(String.class));
  }

  @Test
  public void isComparableTo() {
    Person jonDoe1 = new Person(1l, "Jon", "Doe");
    Person jonDoe2 = new Person(2l, "Jon", "Doe");

    assertTrue(is(jonDoe1).comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).not().comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).equalTo(jonDoe2));
  }

  @Test
  public void isNotComparableTo() {
    Person johnBlum = new Person(1l, "John", "Blum");
    Person jonBloom = new Person(1l, "Jon", "Bloom");

    assertFalse(is(johnBlum).comparableTo(jonBloom));
    assertTrue(is(johnBlum).not().comparableTo(jonBloom));
    assertTrue(is(johnBlum).equalTo(jonBloom));
  }

  @Test
  public void isEqualTo() {
    assertTrue(is(true).equalTo(Boolean.TRUE));
    assertTrue(is('c').equalTo('c'));
    assertTrue(is(2).equalTo(2));
    assertTrue(is(Math.PI).equalTo(Math.PI));
    assertTrue(is("test").equalTo("test"));
  }

  @Test
  public void isEqualToWithUnequalValues() {
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
  public void isNotEqualTo() {
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
  public void isFalse() {
    assertTrue(is(false).False());
    assertTrue(is(Boolean.FALSE).False());
    assertTrue(is(!Boolean.TRUE).False());
  }

  @Test
  public void isNotFalse() {
    assertTrue(is(true).not().False());
    assertTrue(is(Boolean.TRUE).not().False());
    assertTrue(is(!Boolean.FALSE).not().False());
    assertTrue(is((Boolean) null).not().False());
  }

  @Test
  public void isGreaterThan() {
    assertTrue(is(3).greaterThan(1));
    assertTrue(is(3).greaterThan(2));
    assertFalse(is(3).greaterThan(3));
    assertFalse(is(3).greaterThan(4));
    assertFalse(is(3).greaterThan(5));
  }

  @Test
  public void isNotGreaterThan() {
    assertFalse(is(3).not().greaterThan(1));
    assertFalse(is(3).not().greaterThan(2));
    assertTrue(is(3).not().greaterThan(3));
    assertTrue(is(3).not().greaterThan(4));
    assertTrue(is(3).not().greaterThan(5));
  }

  @Test
  public void isGreaterThanAndLessThan() {
    assertFalse(is(1).greaterThanAndLessThan(2, 4));
    assertFalse(is(2).greaterThanAndLessThan(2, 4));
    assertTrue(is(3).greaterThanAndLessThan(2, 4));
    assertFalse(is(4).greaterThanAndLessThan(2, 4));
    assertFalse(is(5).greaterThanAndLessThan(2, 4));
  }

  @Test
  public void isNotGreaterThanAndLessThan() {
    assertTrue(is(1).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(2).not().greaterThanAndLessThan(2, 4));
    assertFalse(is(3).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(4).not().greaterThanAndLessThan(2, 4));
    assertTrue(is(5).not().greaterThanAndLessThan(2, 4));
  }

  @Test
  public void isGreaterThanAndLessThanEqualTo() {
    assertFalse(is(1).greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(2).greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(3).greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(4).greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(5).greaterThanAndLessThanEqualTo(2, 4));
  }

  @Test
  public void isNotGreaterThanAndLessThanEqualTo() {
    assertTrue(is(1).not().greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(2).not().greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(3).not().greaterThanAndLessThanEqualTo(2, 4));
    assertFalse(is(4).not().greaterThanAndLessThanEqualTo(2, 4));
    assertTrue(is(5).not().greaterThanAndLessThanEqualTo(2, 4));
  }

  @Test
  public void isGreaterThanEqualTo() {
    assertTrue(is(3).greaterThanEqualTo(1));
    assertTrue(is(3).greaterThanEqualTo(2));
    assertTrue(is(3).greaterThanEqualTo(3));
    assertFalse(is(3).greaterThanEqualTo(4));
    assertFalse(is(3).greaterThanEqualTo(5));
  }

  @Test
  public void isNotGreaterThanEqualTo() {
    assertFalse(is(3).not().greaterThanEqualTo(1));
    assertFalse(is(3).not().greaterThanEqualTo(2));
    assertFalse(is(3).not().greaterThanEqualTo(3));
    assertTrue(is(3).not().greaterThanEqualTo(4));
    assertTrue(is(3).not().greaterThanEqualTo(5));
  }

  @Test
  public void isGreaterThanEqualToAndLessThan() {
    assertFalse(is(1).greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(2).greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(3).greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(4).greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(5).greaterThanEqualToAndLessThan(2, 4));
  }

  @Test
  public void isNotGreaterThanEqualToAndLessThan() {
    assertTrue(is(1).not().greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(2).not().greaterThanEqualToAndLessThan(2, 4));
    assertFalse(is(3).not().greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(4).not().greaterThanEqualToAndLessThan(2, 4));
    assertTrue(is(5).not().greaterThanEqualToAndLessThan(2, 4));
  }

  @Test
  public void isGreaterThanEqualToAndLessThanEqualTo() {
    assertFalse(is(1).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(2).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(3).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(4).greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(5).greaterThanEqualToAndLessThanEqualTo(2, 4));
  }

  @Test
  public void isNotGreaterThanEqualToAndLessThanEqualTo() {
    assertTrue(is(1).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(2).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(3).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertFalse(is(4).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
    assertTrue(is(5).not().greaterThanEqualToAndLessThanEqualTo(2, 4));
  }

  @Test
  public void isInstanceOf() {
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
  public void isNotInstanceOf() {
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
  public void isLessThan() {
    assertFalse(is(3).lessThan(1));
    assertFalse(is(3).lessThan(2));
    assertFalse(is(3).lessThan(3));
    assertTrue(is(3).lessThan(4));
    assertTrue(is(3).lessThan(5));
  }

  @Test
  public void isNotLessThan() {
    assertTrue(is(3).not().lessThan(1));
    assertTrue(is(3).not().lessThan(2));
    assertTrue(is(3).not().lessThan(3));
    assertFalse(is(3).not().lessThan(4));
    assertFalse(is(3).not().lessThan(5));
  }

  @Test
  public void isLessThanOrGreaterThan() {
    assertTrue(is(1).lessThanOrGreaterThan(2, 4));
    assertFalse(is(2).lessThanOrGreaterThan(2, 4));
    assertFalse(is(3).lessThanOrGreaterThan(2, 4));
    assertFalse(is(4).lessThanOrGreaterThan(2, 4));
    assertTrue(is(5).lessThanOrGreaterThan(2, 4));
  }

  @Test
  public void isNotLessThanOrGreaterThan() {
    assertFalse(is(1).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(2).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(3).not().lessThanOrGreaterThan(2, 4));
    assertTrue(is(4).not().lessThanOrGreaterThan(2, 4));
    assertFalse(is(5).not().lessThanOrGreaterThan(2, 4));
  }

  @Test
  public void isLessThanOrGreaterThanEqualTo() {
    assertTrue(is(1).lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(2).lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(3).lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(4).lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(5).lessThanOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void isNotLessThanOrGreaterThanEqualTo() {
    assertFalse(is(1).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(2).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertTrue(is(3).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(4).not().lessThanOrGreaterThanEqualTo(2, 4));
    assertFalse(is(5).not().lessThanOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void isLessThanEqualTo() {
    assertFalse(is(3).lessThanEqualTo(1));
    assertFalse(is(3).lessThanEqualTo(2));
    assertTrue(is(3).lessThanEqualTo(3));
    assertTrue(is(3).lessThanEqualTo(4));
    assertTrue(is(3).lessThanEqualTo(5));
  }

  @Test
  public void isNotLessThanEqualTo() {
    assertTrue(is(3).not().lessThanEqualTo(1));
    assertTrue(is(3).not().lessThanEqualTo(2));
    assertFalse(is(3).not().lessThanEqualTo(3));
    assertFalse(is(3).not().lessThanEqualTo(4));
    assertFalse(is(3).not().lessThanEqualTo(5));
  }

  @Test
  public void isLessThanEqualToOrGreaterThan() {
    assertTrue(is(1).lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(2).lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(3).lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(4).lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(5).lessThanEqualToOrGreaterThan(2, 4));
  }

  @Test
  public void isNotLessThanEqualToOrGreaterThan() {
    assertFalse(is(1).not().lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(2).not().lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(3).not().lessThanEqualToOrGreaterThan(2, 4));
    assertTrue(is(4).not().lessThanEqualToOrGreaterThan(2, 4));
    assertFalse(is(5).not().lessThanEqualToOrGreaterThan(2, 4));
  }

  @Test
  public void isLessThanEqualToOrGreaterThanEqualTo() {
    assertTrue(is(1).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(2).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(3).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(4).lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(5).lessThanEqualToOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void isNotLessThanEqualToOrGreaterThanEqualTo() {
    assertFalse(is(1).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(2).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertTrue(is(3).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(4).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
    assertFalse(is(5).not().lessThanEqualToOrGreaterThanEqualTo(2, 4));
  }

  @Test
  public void isNotNull() {
    assertTrue(is(Boolean.FALSE).notNull());
    assertTrue(is(Calendar.getInstance()).notNull());
    assertTrue(is('\0').notNull());
    assertTrue(is(Integer.MIN_VALUE).notNull());
    assertTrue(is(Double.MAX_VALUE).notNull());
    assertTrue(is("null").notNull());
  }

  @Test
  public void isNull() {
    assertTrue(is(NULL).Null());
  }

  @Test
  public void isSameAs() {
    assertTrue(is(NULL).sameAs(NULL));
    assertTrue(is(Boolean.TRUE).sameAs(Boolean.TRUE));
    assertTrue(is("test").sameAs("test"));
  }

  @Test
  @SuppressWarnings("all")
  public void isNotSameAs() {
    assertTrue(is(NULL).notSameAs("null"));
    assertTrue(is(Boolean.TRUE).notSameAs(Boolean.FALSE));
    assertTrue(is("test").notSameAs(new String("test")));
  }

  @Test
  public void isTrue() {
    assertTrue(is(true).True());
    assertTrue(is(Boolean.TRUE).True());
    assertTrue(is(!Boolean.FALSE).True());
  }

  @Test
  public void isNotTrue() {
    assertTrue(is(false).not().True());
    assertTrue(is(Boolean.FALSE).not().True());
    assertTrue(is(!Boolean.TRUE).not().True());
    assertTrue(is((Boolean) null).not().True());
  }

  @Test
  public void isReuse() {
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
  public void isDoubleNegative() {
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

    public String getName() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
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

      Person that = (Person) obj;

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
