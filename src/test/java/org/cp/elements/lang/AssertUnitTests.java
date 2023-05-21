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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.Month;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Objects;
import java.util.function.Supplier;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import org.assertj.core.api.ThrowableAssert.ThrowingCallable;
import org.cp.elements.lang.annotation.NotNull;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit Tests for {@link Assert}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Assert
 * @version 1.0.0
 */
public class AssertUnitTests {

  private static final Object LOCK = new Object();

  private static Object returnNull() {
    return null;
  }

  private static void assertExceptionMessageAndNoCause(Throwable exception, String message, Object... arguments) {

    assertThat(exception).isNotNull();
    assertThat(exception).hasMessage(message, arguments);
    assertThat(exception).hasNoCause();
  }

  @Test
  public void assertArgumentIsValid() {
    Assert.argument("test", argument -> true);
  }

  @Test
  public void assertArgumentIsInvalid() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.argument("test", argument -> false));

    assertExceptionMessageAndNoCause(exception, "Argument is not valid");
  }

  @Test
  public void assertArgumentFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.argument("mock", argument -> false,
        "[%1$s] is a {1} argument, a %2$s, {1} argument; {2}!",
        "mock", "bad", "not good"));

    assertExceptionMessageAndNoCause(exception, "[mock] is a bad argument, a bad, bad argument; not good!");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertArgumentWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.argument("mock", argument -> true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertArgumentUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.argument("mock", argument -> false, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertArgumentThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.argument("spy", argument -> false, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertArgumentWithNullArgumentIsNullSafe() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.argument(null, Objects::nonNull));

    assertExceptionMessageAndNoCause(exception, "Argument is not valid");
  }

  @Test
  public void assertArgumentWithNullPredicateIsNullSafe() {
    Assert.argument("test", null);
  }

  @Test
  public void assertComparable() {

    Assert.comparable(true, true);
    Assert.comparable('c', 'c');
    Assert.comparable(1, 1);
    Assert.comparable(Math.PI, Math.PI);
    Assert.comparable("test", "test");
  }

  @Test
  public void assertNotComparable() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.comparable(3.14159d, Math.PI));

    assertExceptionMessageAndNoCause(exception, "[3.14159] is not comparable to [%s]", Math.PI);
  }

  @Test
  public void assertComparableWithNullArguments() {

    ComparisonException exception = Assertions.assertThrows(ComparisonException.class,
      () -> Assert.comparable(null, null));

    assertExceptionMessageAndNoCause(exception, "[null] is not comparable to [null]");
  }

  @Test
  public void assertComparableWithNullFirstArgument() {

    ComparisonException exception = Assertions.assertThrows(ComparisonException.class,
      () -> Assert.comparable(null, "test"));

    assertExceptionMessageAndNoCause(exception, "[null] is not comparable to [test]");
  }

  @Test
  public void assertComparableWithNullSecondArgument() {

    ComparisonException exception = Assertions.assertThrows(ComparisonException.class,
      () -> Assert.comparable("test", null));

    assertExceptionMessageAndNoCause(exception, "[test] is not comparable to [null]");
  }

  @Test
  public void assertComparableFormatsMessageWithPlaceholderValues() {

    ComparisonException exception = Assertions.assertThrows(ComparisonException.class,
      () -> Assert.comparable("null", "nil","[%s] is NOT comparable with [{1}]",
        "null", "nil"));

    assertExceptionMessageAndNoCause(exception, "[null] is NOT comparable with [nil]");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertComparableWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.comparable("test", "test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertComparableUsesSuppliedMessageThrowsComparisonException() {

    ComparisonException exception = Assertions.assertThrows(ComparisonException.class,
      () -> Assert.comparable("test", "mock", () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertComparableThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.comparable(-1, 1, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  @SuppressWarnings("all")
  public void assertEqualsWithEqualValues() {

    Assert.equals(Boolean.TRUE, true);
    Assert.equals(LocalDate.of(2011, Month.OCTOBER, 4),
      LocalDate.of(2011, Month.OCTOBER, 4));
    Assert.equals(LocalDate.of(2016, Month.JANUARY, 5),
      LocalDate.of(2016, Month.JANUARY, 5));
    Assert.equals(LocalDate.of(2018, Month.FEBRUARY, 11),
      LocalDate.of(2018, Month.FEBRUARY, 11));
    Assert.equals("c".charAt(0), 'c');
    Assert.equals(Double.valueOf(String.valueOf(Math.PI)), Math.PI);
    Assert.equals(Integer.valueOf("2"), 2);
    Assert.equals("test", "test");
    Assert.equals(TestEnum.valueOf("ONE"), TestEnum.ONE);
  }

  @Test
  public void assertEqualsWithUnequalBooleanValues() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals(Boolean.TRUE, false));

    assertExceptionMessageAndNoCause(exception, "[true] is not equal to [false]");
  }

  @Test
  public void assertEqualsWithUnequalCharacters() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals('x', 'X'));

    assertExceptionMessageAndNoCause(exception, "[x] is not equal to [X]");
  }

  @Test
  public void assertEqualsWithUnequalDateTimes() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals(LocalDateTime.of(2011, Month.OCTOBER, 4, 12, 30),
        LocalDateTime.now(), "Date/Times are not equal"));

    assertExceptionMessageAndNoCause(exception, "Date/Times are not equal");
  }

  @Test
  public void assertEqualsWithUnequalDoubleValues() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals(3.14159d, Math.PI));

    assertExceptionMessageAndNoCause(exception, "[3.14159] is not equal to [%s]", Math.PI);
  }

  @Test
  public void assertEqualsWithUnequalIntegerValues() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals(-1, 1));

    assertExceptionMessageAndNoCause(exception, "[-1] is not equal to [1]");
  }

  @Test
  public void assertEqualsWithUnequalStrings() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals("test", "TEST"));

    assertExceptionMessageAndNoCause(exception, "[test] is not equal to [TEST]");
  }

  @Test
  public void assertEqualsWithNullValues() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals(null, null));

    assertExceptionMessageAndNoCause(exception, "[null] is not equal to [null]");
  }

  @Test
  public void assertEqualsWithNullValueAndNullStringLiteral() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals(null, "null"));

    assertExceptionMessageAndNoCause(exception, "[null] is not equal to [null]");
  }

  @Test
  public void assertEqualsWithNullAndNilStringLiterals() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals("null", "nil"));

    assertExceptionMessageAndNoCause(exception, "[null] is not equal to [nil]");
  }

  @Test
  public void assertEqualsFormatsMessageWithPlaceholderValues() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals(true, false, "exception %1$s; but was {1}",
        true, false));

    assertExceptionMessageAndNoCause(exception, "exception true; but was false");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertEqualsWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.equals("test", "test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertEqualsUsesSuppliedMessageThrowsEqualityException() {

    EqualityException exception = Assertions.assertThrows(EqualityException.class,
      () -> Assert.equals("test", "mock", () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertEqualsThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.equals(new Object(), new Object(), new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertHasTextWithStrings() {

    Assert.hasText("test");
    Assert.hasText("TEXT");
    Assert.hasText("blank");
    Assert.hasText("empty");
    Assert.hasText("null");
    Assert.hasText("space");
    Assert.hasText("_");
    Assert.hasText("--");
    Assert.hasText("0");
    Assert.hasText("! ");
    Assert.hasText(" !");
    Assert.hasText(" _ ");
  }

  @Test
  public void assertHasTextWithNoString() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.hasText(null));

    assertExceptionMessageAndNoCause(exception, "Argument [null] is blank");
  }

  @Test
  public void assertHasTextWithEmptyString() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.hasText("", "Empty String is blank"));

    assertExceptionMessageAndNoCause(exception, "Empty String is blank");
  }

  @Test
  public void assertHasTextWithNullCharacter() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.hasText("\0", "Null Character is blank"));

    assertExceptionMessageAndNoCause(exception, "Null Character is blank");
  }

  @Test
  public void assertHasTextWithNullString() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.hasText(null, "Null String is blank"));

    assertExceptionMessageAndNoCause(exception, "Null String is blank");
  }

  @Test
  public void assertHasTextWithSpaces() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.hasText("  ", "Spaces are blank"));

    assertExceptionMessageAndNoCause(exception, "Spaces are blank");
  }

  @Test
  public void assertHasTextWithTabSpaceCarriageReturnAndNewLineFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.hasText("\t \r \n", "%s, spaces, carriage returns and {1} are blank",
        "tabs", "newlines"));

    assertExceptionMessageAndNoCause(exception, "tabs, spaces, carriage returns and newlines are blank");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertHasTextWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.hasText("test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertHasTextUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.hasText("   ", () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertHasTextThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.hasText("\n", new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertIsHoldingLock() {
    synchronized (LOCK) {
      Assert.holdsLock(LOCK);
    }
  }

  @Test
  public void assertIsNotHoldingLock() {

    IllegalMonitorStateException exception = Assertions.assertThrows(IllegalMonitorStateException.class,
      () -> Assert.holdsLock(LOCK));

    assertExceptionMessageAndNoCause(exception, "The current thread [%1s] does not hold lock [%2$s]",
        Thread.currentThread().getName(), LOCK);
  }

  @Test
  public void assertHoldsLockWithNullLock() {

    IllegalMonitorStateException exception = Assertions.assertThrows(IllegalMonitorStateException.class,
      () -> Assert.holdsLock(null));

    assertExceptionMessageAndNoCause(exception, "The current thread [%s] does not hold lock [null]",
        Thread.currentThread().getName());
  }

  @Test
  public void assertHoldsLockFormatsMessageWithPlaceholderValues() {

    IllegalMonitorStateException exception = Assertions.assertThrows(IllegalMonitorStateException.class,
      () -> Assert.holdsLock(LOCK, "lock [%1$s] is not held by {1} thread",
        LOCK, "loose"));

    assertExceptionMessageAndNoCause(exception,"lock [%1$s] is not held by loose thread", LOCK);
      assertThat(exception).hasNoCause();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertHoldsLockWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    synchronized (LOCK) {
      Assert.holdsLock(LOCK, mockSupplier);
    }

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertHoldsLockUsesSuppliedMessageThrowsIllegalMonitorStateException() {

    IllegalMonitorStateException exception = Assertions.assertThrows(IllegalMonitorStateException.class,
      () -> Assert.holdsLock(LOCK, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertHoldsLockThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.holdsLock(LOCK, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertIsAssignableToWithAssignableClassTypes() {

    Assert.isAssignableTo(Boolean.class, Boolean.class);
    Assert.isAssignableTo(Character.class, Object.class);
    Assert.isAssignableTo(java.sql.Date.class, java.util.Date.class);
    Assert.isAssignableTo(Double.class, Number.class);
    Assert.isAssignableTo(Integer.class, Number.class);
    Assert.isAssignableTo(String.class, Object.class);
    Assert.isAssignableTo(TestEnum.class, Enum.class);
  }

  @Test
  public void assertIsAssignableToWithNonAssignableNumericClassTypes() {

    ClassCastException exception = Assertions.assertThrows(ClassCastException.class,
      () -> Assert.isAssignableTo(Double.class, Integer.class));

    assertExceptionMessageAndNoCause(exception, "[%1$s] is not assignable to [%2$s]",
      Double.class, Integer.class);
  }

  @Test
  public void assertIsAssignableToWithNonAssignableTextualClassTypes() {

    ClassCastException exception = Assertions.assertThrows(ClassCastException.class,
      () -> Assert.isAssignableTo(Character.class, String.class));

    assertExceptionMessageAndNoCause(exception, "[%1$s] is not assignable to [%2$s]",
      Character.class, String.class);
  }

  @Test
  public void assertIsAssignableToWithNullFromClassType() {
    Assert.isAssignableTo(null, Object.class, "Null is assignable to Object");
  }

  @Test
  public void assertIsAssignableToWithNullToClassType() {

    ClassCastException exception = Assertions.assertThrows(ClassCastException.class,
      () -> Assert.isAssignableTo(Object.class, null, "Object is not assignable to null"));

    assertExceptionMessageAndNoCause(exception, "Object is not assignable to null");
  }

  @Test
  public void assertIsAssignableToFormatsMessageWithPlaceholderValues() {

    ClassCastException exception = Assertions.assertThrows(ClassCastException.class,
      () -> Assert.isAssignableTo(Object.class, String.class, "%1$s is not assignable to {1}",
        "Object", "String"));

    assertExceptionMessageAndNoCause(exception, "Object is not assignable to String");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsAssignableToWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isAssignableTo(String.class, Object.class, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertIsAssignableToUsesSuppliedMessageThrowsClassCastException() {

    ClassCastException exception = Assertions.assertThrows(ClassCastException.class,
      () -> Assert.isAssignableTo(Integer.class, Long.class, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertIsAssignableToThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.isAssignableTo(Object.class, String.class, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  @SuppressWarnings("all")
  public void assertIsFalseWithFalse() {

    Assert.isFalse(false);
    Assert.isFalse(Boolean.FALSE);
    Assert.isFalse(!Boolean.TRUE);
    Assert.isFalse(new Object().equals(new Object()));
    Assert.isFalse(new Object() == new Object());
  }

  @Test
  public void assertIsFalseWithTrue() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isFalse(true));

    assertExceptionMessageAndNoCause(exception, "Condition [true] is not false");
  }

  @Test
  public void assertIsFalseWithNull() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isFalse(null, "null is not false"));

    assertExceptionMessageAndNoCause(exception, "null is not false");
  }

  @Test
  public void assertIsFalseFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isFalse(true, "exception %s; but was {1}", false, true));

    assertExceptionMessageAndNoCause(exception, "exception false; but was true");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsFalseWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isFalse(false, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertIsFalseUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isFalse(true, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertIsFalseThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.isFalse(Boolean.TRUE, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertIsInstanceOf() {

    Assert.isInstanceOf(true, Boolean.class);
    Assert.isInstanceOf('c', Character.class);
    Assert.isInstanceOf(Math.PI, Double.class);
    Assert.isInstanceOf(3.14159f, Float.class);
    Assert.isInstanceOf(0, Integer.class);
    Assert.isInstanceOf(9876543210L, Long.class);
    Assert.isInstanceOf("mock", CharSequence.class);
    Assert.isInstanceOf("spy", Serializable.class);
    Assert.isInstanceOf("test", String.class);
    Assert.isInstanceOf(new Object(), Object.class);
    Assert.isInstanceOf(Object.class, Class.class);
    Assert.isInstanceOf(TestEnum.ONE, Enum.class);
  }

  @Test
  public void assertIsNotInstanceOf() {

    IllegalTypeException exception = Assertions.assertThrows(IllegalTypeException.class,
      () -> Assert.isInstanceOf("0123456789", Long.class));

    assertExceptionMessageAndNoCause(exception, "[0123456789] is not an instance of [class java.lang.Long]");
  }

  @Test
  public void assertIsInstanceOfCloneableWithNull() {

    IllegalTypeException exception = Assertions.assertThrows(IllegalTypeException.class,
      () -> Assert.isInstanceOf(null, Cloneable.class, "null is not Cloneable"));

    assertExceptionMessageAndNoCause(exception, "null is not Cloneable");
  }

  @Test
  public void assertIsInstanceOfCloneableWithObject() {

    IllegalTypeException exception = Assertions.assertThrows(IllegalTypeException.class,
      () -> Assert.isInstanceOf(new Object(), Cloneable.class, "Object is not Cloneable"));

    assertExceptionMessageAndNoCause(exception, "Object is not Cloneable");
  }

  @Test
  public void assertIsInstanceOfWithNull() {

    IllegalTypeException exception = Assertions.assertThrows(IllegalTypeException.class,
      () -> Assert.isInstanceOf(null, Object.class, "null is not an instance of Object",
        "unused", "args"));

    assertExceptionMessageAndNoCause(exception, "null is not an instance of Object");
  }

  @Test
  public void assertIsInstanceOfWithNullClassType() {

    IllegalTypeException exception = Assertions.assertThrows(IllegalTypeException.class,
      () -> Assert.isInstanceOf(new Object(), null, "Object is not an instance of null",
        "unused", "args"));

    assertExceptionMessageAndNoCause(exception, "Object is not an instance of null");
  }

  @Test
  public void assertIsInstanceOfFormatsMessageWithPlaceholderValues() {

    IllegalTypeException exception = Assertions.assertThrows(IllegalTypeException.class,
      () -> Assert.isInstanceOf(null, Object.class, "%s is not an instance of {1}",
        null, Object.class.getSimpleName()));

    assertExceptionMessageAndNoCause(exception, "null is not an instance of Object");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsInstanceOfWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isInstanceOf("test", String.class, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertIsInstanceOfUsesSuppliedMessageThrowsIllegalTypeException() {

    IllegalTypeException exception = Assertions.assertThrows(IllegalTypeException.class,
      () -> Assert.isInstanceOf('x', String.class, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertIsInstanceOfThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.isInstanceOf(new Object(), Class.class, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  @SuppressWarnings("all")
  public void assertIsTrueWithTrue() {

    Assert.isTrue(true);
    Assert.isTrue(Boolean.TRUE);
    Assert.isTrue(!Boolean.FALSE);
    Assert.isTrue("test".equals("test"));
    Assert.isTrue("test".equalsIgnoreCase("TEST"));
    Assert.isTrue(LOCK.equals(LOCK));
    Assert.isTrue(LOCK == LOCK);
  }

  @Test
  public void assertIsTrueWithFalse() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isTrue(false));

    assertExceptionMessageAndNoCause(exception, "Condition [false] is not true");
  }

  @Test
  public void assertIsTrueWithNull() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isTrue(null, "null is not true"));

    assertExceptionMessageAndNoCause(exception, "null is not true");
  }

  @Test
  public void assertIsTrueFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isTrue(false, "exception %1$s; but was {1}", true, false));

    assertExceptionMessageAndNoCause(exception, "exception true; but was false");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsTrueWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isTrue(true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertIsTrueUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.isTrue(false, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertIsTrueThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.isTrue(Boolean.FALSE, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyStringWithNonEmptyStrings() {

    Assert.notEmpty("text");
    Assert.notEmpty("test");
    Assert.notEmpty("blank");
    Assert.notEmpty("empty");
    Assert.notEmpty("nil");
    Assert.notEmpty("null");
    Assert.notEmpty("_");
    Assert.notEmpty("___");
    Assert.notEmpty("\0");
    Assert.notEmpty("\n");
    Assert.notEmpty("\r");
    Assert.notEmpty("\t");
  }

  @Test
  public void assertNotEmptyStringWithSpaces() {

    Assert.notEmpty(" ");
    Assert.notEmpty("  ");
    Assert.notEmpty("   ");
  }

  @Test
  public void assertNotEmptyStringWithNullString() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty((String) null, "Null is an empty String"));

    assertExceptionMessageAndNoCause(exception, "Null is an empty String");
  }

  @Test
  public void assertNotEmptyStringWithEmptyString() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(""));

    assertExceptionMessageAndNoCause(exception, "String value is empty");
  }

  @Test
  public void assertNotEmptyStringWithBlankStrings() {

    Assert.notEmpty(" ");
    Assert.notEmpty("  ");
    Assert.notEmpty("   ");
  }

  @Test
  public void assertNotEmptyStringFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty("", "%1$s is {1}", "String", "empty"));

    assertExceptionMessageAndNoCause(exception, "String is empty");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyStringWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty("test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertNotEmptyStringUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty("", () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyStringThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.notEmpty("", new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyArrayWithUninitializedSizedArray() {
    Assert.notEmpty(new Object[1]);
  }

  @Test
  public void assertNotEmptyArrayWithSingletonArray() {
    Assert.notEmpty(new Object[] { "test" });
  }

  @Test
  public void assertNotEmptyArrayWithInitializedArray() {
    Assert.notEmpty(new String[] { "one", "two", "three" });
  }

  @Test
  public void assertNotEmptyArrayWithNullArray() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty((Object[]) null));

    assertExceptionMessageAndNoCause(exception, "Array is empty");
  }

  @Test
  public void assertNotEmptyArrayWithEmptyArray() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(new Object[0]));

    assertExceptionMessageAndNoCause(exception, "Array is empty");
  }

  @Test
  public void assertNotEmptyArrayFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(new Object[0], "%1$s is {1}", "Object array", "empty"));

    assertExceptionMessageAndNoCause(exception, "Object array is empty");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyArrayWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty(new Object[1], mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertNotEmptyArrayUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(new Object[0], () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyArrayThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.notEmpty(new Object[0], new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyCollection() {
    Assert.notEmpty(Arrays.asList("ant", "bat", "cat", "dog", "eel"));
  }

  @Test
  public void assertNotEmptyCollectionWithSingletonCollection() {

    Assert.notEmpty(Collections.singleton(1));
    Assert.notEmpty(Collections.singletonList("test"));
  }

  @Test
  public void assertNotEmptyCollectionWithNullCollection() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty((Collection<?>) null));

    assertExceptionMessageAndNoCause(exception, "Collection is empty");
  }

  private void testAssertNotEmptyCollectionWithEmptyCollection(Collection<?> collection) {

    try {
      Assert.notEmpty(collection);
    }
    catch (IllegalArgumentException exception) {

      assertThat(exception).hasMessage("Collection is empty");
      assertThat(exception).hasNoCause();

      throw exception;
    }
  }

  @Test
  public void assertNotEmptyCollectionWithEmptyList() {

    Assertions.assertThrows(IllegalArgumentException.class,
      () -> testAssertNotEmptyCollectionWithEmptyCollection(Collections.emptyList()));
  }

  @Test
  public void assertNotEmptyCollectionWithEmptySet() {

    Assertions.assertThrows(IllegalArgumentException.class,
      () -> testAssertNotEmptyCollectionWithEmptyCollection(Collections.emptySet()));
  }

  @Test
  public void assertNotEmptyCollectionFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(Collections.emptySet(), "%1$s is {1}", "Set", "empty"));

    assertExceptionMessageAndNoCause(exception, "Set is empty");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyCollectionWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty(Collections.singleton(1), mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertNotEmptyCollectionUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(Collections.emptySet(), () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyCollectionThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.notEmpty((Collection<?>) null, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyIterable() {

    Iterable<Object> iterable = Collections.singletonList("test");

    Assert.notEmpty(iterable);
  }

  @Test
  public void assertNotEmptyIterableWithNullIterable() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty((Iterable<?>) null));

    assertExceptionMessageAndNoCause(exception, "Iterable is empty");
  }

  @Test
  public void assertNotEmptyIterableWithNullIterator() {

    Iterable<?> mockIterable = mock(Iterable.class);

    doReturn(null).when(mockIterable).iterator();

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(mockIterable));

    assertExceptionMessageAndNoCause(exception, "Iterable is empty");

    verify(mockIterable, times(1)).iterator();
    verifyNoMoreInteractions(mockIterable);
  }

  @Test
  public void assertNotEmptyIterableWithEmptyIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);
    Iterator<?> mockIterator = mock(Iterator.class);

    doReturn(mockIterator).when(mockIterable).iterator();
    doReturn(false).when(mockIterator).hasNext();

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(mockIterable));

    assertExceptionMessageAndNoCause(exception, "Iterable is empty");

    verify(mockIterable, times(1)).iterator();
    verify(mockIterator, times(1)).hasNext();
    verifyNoMoreInteractions(mockIterable, mockIterator);
  }

  @Test
  public void assertNotEmptyIterableWithFormatsMessageWithPlaceholderValues() {

    Iterable<?> mockIterable = mock(Iterable.class);

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(mockIterable, "%s is {1}", mockIterable, "empty"));

    assertExceptionMessageAndNoCause(exception, "%s is empty", mockIterable);

    verify(mockIterable, times(1)).iterator();
    verifyNoMoreInteractions(mockIterable);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyIterableWithSuppliedMessage() {

    Iterable<Object> iterable = Collections.singleton("test");

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty(iterable, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertNotEmptyIterableUsesSuppliedMessageThrowsIllegalArgumentException() {

    Iterable<?> mockIterable = mock(Iterable.class);

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(mockIterable, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");

    verify(mockIterable, times(1)).iterator();
    verifyNoMoreInteractions(mockIterable);
  }

  @Test
  public void assertNotEmptyIterableThrowsAssertionException() {

    Iterable<?> mockIterable = mock(Iterable.class);

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.notEmpty(mockIterable, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");

    verify(mockIterable, times(1)).iterator();
    verifyNoMoreInteractions(mockIterable);
  }

  @Test
  public void assertNotEmptyMap() {

    Map<Integer, String> map = new HashMap<>(2);

    map.put(1, "one");
    map.put(2, "two");

    Assert.notEmpty(map);
  }

  @Test
  public void assertNotEmptyMapWithSingletonMap() {
    Assert.notEmpty(Collections.singletonMap("key", "value"));
  }

  @Test
  public void assertNotEmptyMapWithNullMap() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty((Map<?, ?>) null));

    assertExceptionMessageAndNoCause(exception, "Map is empty");
  }

  @Test
  public void assertNotEmptyMapWithEmptyMap() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(Collections.emptyMap()));

    assertExceptionMessageAndNoCause(exception, "Map is empty");
  }

  @Test
  public void assertNotEmptyMapFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(Collections.emptyMap(), "%1$s is {1}", "Map", "empty"));

    assertExceptionMessageAndNoCause(exception, "Map is empty");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyMapWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty(Collections.singletonMap("key", "value"), mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertNotEmptyMapUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notEmpty(Collections.emptyMap(), () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotEmptyMapThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.notEmpty(Collections.emptyMap(), new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotInterruptedIsSuccessful() throws InterruptedException {
    Assert.notInterrupted();
  }

  @Test
  public void assertNotInterruptedWhenCurrentThreadIsInterrupted() throws Throwable {
    TestFramework.runOnce(new InterruptedThreadCallingAssertNotInterruptedTestCase(Assert::notInterrupted,
      "Thread [Interrupted Thread calling Assert.notInterrupted(..)] was interrupted"));
  }

  @Test
  public void assertNotInterruptedWhenCurrentThreadIsInterruptedWithFormattedMessage() throws Throwable {
    TestFramework.runOnce(new InterruptedThreadCallingAssertNotInterruptedTestCase(
      () -> Assert.notInterrupted("%s Thread was interrupted", "User"),
      "User Thread was interrupted"));
  }

  @Test
  public void assertNotInterruptedWhenCurrentThreadIsInterruptedWithRuntimeException() throws Throwable {
    TestFramework.runOnce(new InterruptedThreadCallingAssertNotInterruptedWithRuntimeExceptionTestCase());
  }

  @Test
  public void assertNotNull() {

    Assert.notNull(false);
    Assert.notNull('\0');
    Assert.notNull(0);
    Assert.notNull(0.0d);
    Assert.notNull("nil");
    Assert.notNull("null");
    Assert.notNull("test");
    Assert.notNull(new Object());
    Assert.notNull(new Object[0]);
    Assert.notNull(Collections.emptySet());
    Assert.notNull(Void.class);
    Assert.notNull(TestEnum.ONE);
  }

  @Test
  public void assertNotNullWithNull() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notNull(null));

    assertExceptionMessageAndNoCause(exception, "Argument is null");
  }

  @Test
  public void assertNotNullWithNullReturningMethod() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notNull(returnNull()));

    assertExceptionMessageAndNoCause(exception, "Argument is null");
  }

  @Test
  public void assertNotNullFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notNull(null, "%1$s is {1}", "Object reference", null));

    assertExceptionMessageAndNoCause(exception, "Object reference is null");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotNullWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notNull("test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertNotNullUsesSuppliedMessageThrowsIllegalArgumentException() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.notNull(null, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotNullThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.notNull(null, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotSameWithNonIdenticalObjects() {

    Assert.notSame(null, "null");
    Assert.notSame(true, false);
    Assert.notSame('c', "C");
    Assert.notSame(1, -1);
    Assert.notSame(3.14159d, Math.PI);
    Assert.notSame("test", "TEST");
    Assert.notSame(new Object(), new Object());
  }

  @Test
  public void assertNotSameWithIdenticalBooleanValues() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame(false, Boolean.FALSE));

    assertExceptionMessageAndNoCause(exception, "[false] is the same as [false]");
  }

  @Test
  public void assertNotSameWithIdenticalCharacterValues() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame('x', 'x'));

    assertExceptionMessageAndNoCause(exception, "[x] is the same as [x]");
  }

  @Test
  public void assertNotSameWithIdenticalDoubleValues() {

    Double value = 2.0d;

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame(value, value));

    assertExceptionMessageAndNoCause(exception, "[2.0] is the same as [2.0]");
  }

  @Test
  public void assertNotSameWithIdenticalIntegerValues() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame(2, 2));

    assertExceptionMessageAndNoCause(exception, "[2] is the same as [2]");
  }

  @Test
  public void assertNotSameWithIdenticalStringValues() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame("test", "test"));

    assertExceptionMessageAndNoCause(exception, "[test] is the same as [test]");
  }

  @Test
  public void assertNotSameWithIdenticalObjectValues() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame(LOCK, LOCK));

    assertExceptionMessageAndNoCause(exception, "[%1$s] is the same as [%1$s]", LOCK);
  }

  @Test
  public void assertNotSameFormatsMessageWithPlaceholderValues() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame(1, 1, "%1$s are the {1}", "Integers", "same"));

    assertExceptionMessageAndNoCause(exception, "Integers are the same");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotSameWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notSame(1, 1.0, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertNotSameUsesSuppliedMessageThrowsIdentityException() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.notSame(1, 1, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertNotSameThrowsAssertException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.notSame(1, 1, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertSameWithIdenticalObjects() {

    Double pi = Math.PI;

    Assert.same(null, null);
    Assert.same(true, Boolean.TRUE);
    Assert.same('c', 'c');
    Assert.same(1, 1);
    Assert.same(pi, pi);
    Assert.same("test", "test");
    Assert.same(LOCK, LOCK);
  }

  @Test
  public void assertSameWithCharacterAndString() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.same('x', "x"));

    assertExceptionMessageAndNoCause(exception, "[x] is not the same as [x]");
  }

  @Test
  public void assertSameWithDoubleAndInteger() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.same(1.0d, 1));

    assertExceptionMessageAndNoCause(exception, "[1.0] is not the same as [1]");
  }

  @Test
  public void assetSameWithNullAndObject() {

    Object value = new Object();

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.same(null, value));

    assertExceptionMessageAndNoCause(exception, "[null] is not the same as [%s]", value);
  }

  @Test
  public void assertSameFormatsMessageWithPlaceholderValues() {

    IllegalArgumentException exception = Assertions.assertThrows(IllegalArgumentException.class,
      () -> Assert.same("test", "TEST", "%1$s are not {1}",
        "Strings", "identical"));

    assertExceptionMessageAndNoCause(exception, "Strings are not identical");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertSameWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.same("test", "test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertSameUsesSuppliedMessageThrowsIdentityException() {

    IdentityException exception = Assertions.assertThrows(IdentityException.class,
      () -> Assert.same(1, -1, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertSameThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.same(new Object(), new Object(), new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertStateIsValid() {
    Assert.state(true);
  }

  @Test
  public void assertStateIsInvalid() {

    IllegalStateException exception = Assertions.assertThrows(IllegalStateException.class,
      () -> Assert.state(false));

    assertExceptionMessageAndNoCause(exception, "State is invalid");
  }

  @Test
  public void assertStateWithNullIsNullSafe() {

    IllegalStateException exception = Assertions.assertThrows(IllegalStateException.class,
      () -> Assert.state(null));

    assertExceptionMessageAndNoCause(exception, "State is invalid");
  }

  @Test
  public void assertStateFormatsMessageWithPlaceholderValues() {

    IllegalStateException exception = Assertions.assertThrows(IllegalStateException.class,
      () -> Assert.state(Boolean.FALSE, "%1$s not {1}", "Object", "initialized"));

    assertExceptionMessageAndNoCause(exception, "Object not initialized");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertStateWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.state(true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertStateUsesSuppliedMessageThrowsIllegalStateException() {

    IllegalStateException exception = Assertions.assertThrows(IllegalStateException.class,
      () -> Assert.state(false, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertStateThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.state(false, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertSupportedForSupportedOperation() {
    Assert.supported(true);
  }

  @Test
  public void assertSupportedForUnsupportedOperation() {

    UnsupportedOperationException exception = Assertions.assertThrows(UnsupportedOperationException.class,
      () -> Assert.supported(false));

    assertExceptionMessageAndNoCause(exception, "Operation not supported");
  }

  @Test
  public void assertSupportedWithNullIsNullSafe() {

    UnsupportedOperationException exception = Assertions.assertThrows(UnsupportedOperationException.class,
      () -> Assert.supported(null));

    assertExceptionMessageAndNoCause(exception, "Operation not supported");
  }

  @Test
  public void assertSupportedFormatsMessageWithPlaceholderValues() {

    UnsupportedOperationException exception = Assertions.assertThrows(UnsupportedOperationException.class,
      () -> Assert.supported(Boolean.FALSE, "%1$s is {1}",
        "Write operation", "unsupported"));

    assertExceptionMessageAndNoCause(exception, "Write operation is unsupported");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertSupportedWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.supported(true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void assertSupportedUsesSuppliedMessageThrowsUnsupportedOperationException() {

    UnsupportedOperationException exception = Assertions.assertThrows(UnsupportedOperationException.class,
      () -> Assert.supported(false, () -> "test"));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @Test
  public void assertSupportedThrowsAssertionException() {

    AssertionException exception = Assertions.assertThrows(AssertionException.class,
      () -> Assert.supported(false, new AssertionException("test")));

    assertExceptionMessageAndNoCause(exception, "test");
  }

  @SuppressWarnings("unused")
  private static final class InterruptedThreadCallingAssertNotInterruptedTestCase extends MultithreadedTestCase {

    private final String exceptionMessage;

    private final ThrowingCallable throwingCallable;

    private InterruptedThreadCallingAssertNotInterruptedTestCase(@NotNull ThrowingCallable throwingCallable,
        @NotNull String exceptionMessage) {

      this.throwingCallable = ObjectUtils.requireObject(throwingCallable, "ThrowingCallable is required");
      this.exceptionMessage = StringUtils.requireText(exceptionMessage, "exception message is required");
    }

    private @NotNull String getExceptionMessage() {
      return this.exceptionMessage;
    }

    private @NotNull ThrowingCallable getThrowingCallable() {
      return this.throwingCallable;
    }

    public void thread1() {

      Thread.currentThread().setName("Interrupted Thread calling Assert.notInterrupted(..)");

      assertTick(0);

      Thread.currentThread().interrupt();

      assertThatExceptionOfType(InterruptedException.class)
        .isThrownBy(getThrowingCallable())
        .withMessage(getExceptionMessage())
        .withNoCause();
    }
  }

  @SuppressWarnings("unused")
  private static final class InterruptedThreadCallingAssertNotInterruptedWithRuntimeExceptionTestCase
      extends MultithreadedTestCase {

    public void thread1() {

      Thread.currentThread().setName("Interrupted Thread calling Assert.notInterrupted(..)");

      assertTick(0);

      Thread.currentThread().interrupt();

      assertThatIllegalStateException()
        .isThrownBy(() -> Assert.notInterrupted(new IllegalStateException("test")))
        .withMessage("test")
        .withNoCause();
    }
  }

  private enum TestEnum {
    ONE,
    TWO
  }
}
