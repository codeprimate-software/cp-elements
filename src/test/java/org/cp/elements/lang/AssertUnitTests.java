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

import org.junit.Test;

/**
 * Unit Tests for {@link Assert}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Assert
 * @version 1.0.0
 */
public class AssertUnitTests {

  private static final Object LOCK = new Object();

  private static Object returnNull() {
    return null;
  }

  @Test
  public void assertArgumentIsValid() {
    Assert.argument("test", argument -> true);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertArgumentIsInvalid() {

    try {
      Assert.argument("test", argument -> false);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Argument is not valid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertArgumentFormatsMessageWithPlaceholderValues() {

    try {
      Assert.argument("mock", argument -> false,
        "[%1$s] is a {1} argument, a %2$s, {1} argument; {2}!",
        "mock", "bad", "not good");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("[mock] is a bad argument, a bad, bad argument; not good!");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertArgumentWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.argument("mock", argument -> true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertArgumentUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.argument("mock", argument -> false, () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertArgumentThrowsAssertionException() {

    try {
      Assert.argument("spy", argument -> false, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertArgumentWithNullArgumentIsNullSafe() {

    try {
      Assert.argument(null, Objects::nonNull);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Argument is not valid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotComparable() {

    try {
      Assert.comparable(3.14159d, Math.PI);
    }
    catch (ComparisonException expected) {

      assertThat(expected).hasMessage("[3.14159] is not comparable to [%s]", Math.PI);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ComparisonException.class)
  public void assertComparableWithNullArguments() {

    try {
      Assert.comparable(null, null);
    }
    catch (ComparisonException expected) {

      assertThat(expected).hasMessage("[null] is not comparable to [null]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ComparisonException.class)
  public void assertComparableWithNullFirstArgument() {

    try {
      Assert.comparable(null, "test");
    }
    catch (ComparisonException expected) {

      assertThat(expected).hasMessage("[null] is not comparable to [test]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ComparisonException.class)
  public void assertComparableWithNullSecondArgument() {

    try {
      Assert.comparable("test", null);
    }
    catch (ComparisonException expected) {

      assertThat(expected).hasMessage("[test] is not comparable to [null]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ComparisonException.class)
  public void assertComparableFormatsMessageWithPlaceholderValues() {

    try {
      Assert.comparable("null", "nil",
        "[%s] is NOT comparable with [{1}]", "null", "nil");
    }
    catch (ComparisonException expected) {

      assertThat(expected).hasMessage("[null] is NOT comparable with [nil]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertComparableWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.comparable("test", "test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = ComparisonException.class)
  public void assertComparableUsesSuppliedMessageThrowsComparisonException() {

    try {
      Assert.comparable("test", "mock", () -> "test");
    }
    catch (ComparisonException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertComparableThrowsAssertionException() {

    try {
      Assert.comparable(-1, 1, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
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

  @Test(expected = EqualityException.class)
  public void assertEqualsWithUnequalBooleanValues() {

    try {
      Assert.equals(Boolean.TRUE, false);
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[true] is not equal to [false]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithUnequalCharacters() {

    try {
      Assert.equals('x', 'X');
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[x] is not equal to [X]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithUnequalDateTimes() {

    try {
      Assert.equals(LocalDateTime.of(2011, Month.OCTOBER, 4, 12, 30),
        LocalDateTime.now(), "Date/Times are not equal");
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("Date/Times are not equal");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithUnequalDoubleValues() {

    try {
      Assert.equals(3.14159d, Math.PI);
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[3.14159] is not equal to [%s]", Math.PI);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithUnequalIntegerValues() {

    try {
      Assert.equals(-1, 1);
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[-1] is not equal to [1]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithUnequalStrings() {

    try {
      Assert.equals("test", "TEST");
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[test] is not equal to [TEST]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithNullValues() {

    try {
      Assert.equals(null, null);
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[null] is not equal to [null]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithNullValueAndNullStringLiteral() {

    try {
      Assert.equals(null, "null");
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[null] is not equal to [null]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsWithNullAndNilStringLiterals() {

    try {
      Assert.equals("null", "nil");
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("[null] is not equal to [nil]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsFormatsMessageWithPlaceholderValues() {

    try {
      Assert.equals(true, false, "Expected %1$s; but was {1}", true, false);
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("Expected true; but was false");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertEqualsWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.equals("test", "test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = EqualityException.class)
  public void assertEqualsUsesSuppliedMessageThrowsEqualityException() {

    try {
      Assert.equals("test", "mock", () -> "test");
    }
    catch (EqualityException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertEqualsThrowsAssertionException() {

    try {
      Assert.equals(new Object(), new Object(), new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertHasTextWithNoString() {

    try {
      Assert.hasText(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Argument [null] is blank");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertHasTextWithEmptyString() {
    Assert.hasText("", "Empty String is blank");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertHasTextWithNullCharacter() {
    Assert.hasText("\0", "Null Character is blank");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertHasTextWithNullString() {
    Assert.hasText(null, "Null String is blank");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertHasTextWithSpaces() {

    try {
      Assert.hasText("  ", "Spaces are blank");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Spaces are blank");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertHasTextWithTabSpaceCarriageReturnAndNewLineFormatsMessageWithPlaceholderValues() {

    try {
      Assert.hasText("\t \r \n", "%s, spaces, carriage returns and {1} are blank",
        "tabs", "newlines");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("tabs, spaces, carriage returns and newlines are blank");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertHasTextWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.hasText("test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertHasTextUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.hasText("   ", () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertHasTextThrowsAssertionException() {

    try {
      Assert.hasText("\n", new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertIsHoldingLock() {
    synchronized (LOCK) {
      Assert.holdsLock(LOCK);
    }
  }

  @Test(expected = IllegalMonitorStateException.class)
  public void assertIsNotHoldingLock() {

    try {
      Assert.holdsLock(LOCK);
    }
    catch (IllegalMonitorStateException expected) {

      assertThat(expected).hasMessage("The current thread [%1s] does not hold lock [%2$s]",
        Thread.currentThread().getName(), LOCK);

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalMonitorStateException.class)
  public void assertHoldsLockWithNullLock() {

    try {
      Assert.holdsLock(null);
    }
    catch (IllegalMonitorStateException expected) {

      assertThat(expected).hasMessage("The current thread [%s] does not hold lock [null]",
        Thread.currentThread().getName());

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalMonitorStateException.class)
  public void assertHoldsLockFormatsMessageWithPlaceholderValues() {

    try {
      Assert.holdsLock(LOCK, "lock [%1$s] is not held by {1} thread", LOCK, "loose");
    }
    catch (IllegalMonitorStateException expected) {

      assertThat(expected).hasMessage("lock [%1$s] is not held by loose thread", LOCK);
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalMonitorStateException.class)
  public void assertHoldsLockUsesSuppliedMessageThrowsIllegalMonitorStateException() {

    try {
      Assert.holdsLock(LOCK, () -> "test");
    }
    catch (IllegalMonitorStateException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertHoldsLockThrowsAssertionException() {

    try {
      Assert.holdsLock(LOCK, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = ClassCastException.class)
  public void assertIsAssignableToWithNonAssignableNumericClassTypes() {

    try {
      Assert.isAssignableTo(Double.class, Integer.class);
    }
    catch (ClassCastException expected) {

      assertThat(expected).hasMessage("[%1$s] is not assignable to [%2$s]", Double.class, Integer.class);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ClassCastException.class)
  public void assertIsAssignableToWithNonAssignableTextualClassTypes() {

    try {
      Assert.isAssignableTo(Character.class, String.class);
    }
    catch (ClassCastException expected) {

      assertThat(expected).hasMessage("[%1$s] is not assignable to [%2$s]", Character.class, String.class);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertIsAssignableToWithNullFromClassType() {
    Assert.isAssignableTo(null, Object.class, "Null is assignable to Object");
  }

  @Test(expected = ClassCastException.class)
  public void assertIsAssignableToWithNullToClassType() {

    try {
      Assert.isAssignableTo(Object.class, null, "Object is not assignable to null");
    }
    catch (ClassCastException expected) {

      assertThat(expected).hasMessage("Object is not assignable to null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = ClassCastException.class)
  public void assertIsAssignableToFormatsMessageWithPlaceholderValues() {

    try {
      Assert.isAssignableTo(Object.class, String.class, "%1$s is not assignable to {1}",
        "Object", "String");
    }
    catch (ClassCastException expected) {

      assertThat(expected).hasMessage("Object is not assignable to String");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsAssignableToWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isAssignableTo(String.class, Object.class, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = ClassCastException.class)
  public void assertIsAssignableToUsesSuppliedMessageThrowsClassCastException() {

    try {
      Assert.isAssignableTo(Integer.class, Long.class, () -> "test");
    }
    catch (ClassCastException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertIsAssignableToThrowsAssertionException() {

    try {
      Assert.isAssignableTo(Object.class, String.class, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertIsFalseWithTrue() {

    try {
      Assert.isFalse(true);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Condition [true] is not false");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertIsFalseWithNull() {

    try {
      Assert.isFalse(null, "null is not false");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("null is not false");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertIsFalseFormatsMessageWithPlaceholderValues() {

    try {
      Assert.isFalse(true, "expected %s; but was {1}", false, true);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("expected false; but was true");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsFalseWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isFalse(false, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertIsFalseUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.isFalse(true, () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertIsFalseThrowsAssertionException() {

    try {
      Assert.isFalse(Boolean.TRUE, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalTypeException.class)
  public void assertIsNotInstanceOf() {

    try {
      Assert.isInstanceOf("0123456789", Long.class);
    }
    catch (IllegalTypeException expected) {

      assertThat(expected).hasMessage("[0123456789] is not an instance of [class java.lang.Long]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalTypeException.class)
  public void assertIsInstanceOfWithNull() {

    try {
      Assert.isInstanceOf(null, Object.class, "null is not an instance of Object",
        "unused", "args");
    }
    catch (IllegalTypeException expected) {

      assertThat(expected).hasMessage("null is not an instance of Object");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalTypeException.class)
  public void assertIsInstanceOfWithNullClassType() {

    try {
      Assert.isInstanceOf(new Object(), null, "Object is not an instance of null",
        "unused", "args");
    }
    catch (IllegalTypeException expected) {

      assertThat(expected).hasMessage("Object is not an instance of null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalTypeException.class)
  public void assertIsInstanceOfFormatsMessageWithPlaceholderValues() {

    try {
      Assert.isInstanceOf(null, Object.class, "%s is not an instance of {1}",
        null, Object.class.getSimpleName());
    }
    catch (IllegalTypeException expected) {

      assertThat(expected).hasMessage("null is not an instance of Object");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsInstanceOfWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isInstanceOf("test", String.class, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalTypeException.class)
  public void assertIsInstanceOfUsesSuppliedMessageThrowsIllegalTypeException() {

    try {
      Assert.isInstanceOf('x', String.class, () -> "test");
    }
    catch (IllegalTypeException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertIsInstanceOfThrowsAssertionException() {

    try {
      Assert.isInstanceOf(new Object(), Class.class, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertIsTrueWithFalse() {

    try {
      Assert.isTrue(false);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Condition [false] is not true");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertIsTrueWithNull() {

    try {
      Assert.isTrue(null, "null is not true");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("null is not true");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertIsTrueFormatsMessageWithPlaceholderValues() {

    try {
      Assert.isTrue(false, "Expected %1$s; but was {1}", true, false);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Expected true; but was false");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertIsTrueWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.isTrue(true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertIsTrueUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.isTrue(false, () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertIsTrueThrowsAssertionException() {

    try {
      Assert.isTrue(Boolean.FALSE, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyStringWithNullString() {

    try {
      Assert.notEmpty((String) null, "Null is an empty String");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Null is an empty String");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyStringWithEmptyString() {

    try {
      Assert.notEmpty("");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("String value is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertNotEmptyStringWithBlankStrings() {

    Assert.notEmpty(" ");
    Assert.notEmpty("  ");
    Assert.notEmpty("   ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyStringFormatsMessageWithPlaceholderValues() {

    try {
      Assert.notEmpty("", "%1$s is {1}", "String", "empty");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("String is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyStringWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty("test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyStringUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.notEmpty("", () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertNotEmptyStringThrowsAssertionException() {

    try {
      Assert.notEmpty("", new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyArrayWithNullArray() {

    try {
      Assert.notEmpty((Object[]) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Array is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyArrayWithEmptyArray() {

    try {
      Assert.notEmpty(new Object[0]);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Array is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyArrayFormatsMessageWithPlaceholderValues() {

    try {
      Assert.notEmpty(new Object[0], "%1$s is {1}", "Object array", "empty");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Object array is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyArrayWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty(new Object[1], mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyArrayUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.notEmpty(new Object[0], () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertNotEmptyArrayThrowsAssertionException() {

    try {
      Assert.notEmpty(new Object[0], new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyCollectionWithNullCollection() {

    try {
      Assert.notEmpty((Collection<?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Collection is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  private void testAssertNotEmptyCollectionWithEmptyCollection(Collection<?> collection) {

    try {
      Assert.notEmpty(collection);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Collection is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyCollectionWithEmptyList() {
    testAssertNotEmptyCollectionWithEmptyCollection(Collections.emptyList());
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyCollectionWithEmptySet() {
    testAssertNotEmptyCollectionWithEmptyCollection(Collections.emptySet());
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyCollectionFormatsMessageWithPlaceholderValues() {

    try {
      Assert.notEmpty(Collections.emptySet(), "%1$s is {1}", "Set", "empty");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Set is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyCollectionWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty(Collections.singleton(1), mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyCollectionUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.notEmpty(Collections.emptySet(), () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertNotEmptyCollectionThrowsAssertionException() {

    try {
      Assert.notEmpty((Collection<?>) null, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertNotEmptyIterable() {

    Iterable<Object> iterable = Collections.singletonList("test");

    Assert.notEmpty(iterable);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyIterableWithNullIterable() {

    try {
      Assert.notEmpty((Iterable<?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Iterable is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyIterableWithNullIterator() {

    Iterable<?> mockIterable = mock(Iterable.class);

    doReturn(null).when(mockIterable).iterator();

    try {
      Assert.notEmpty(mockIterable);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Iterable is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockIterable, times(1)).iterator();
      verifyNoMoreInteractions(mockIterable);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyIterableWithEmptyIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);
    Iterator<?> mockIterator = mock(Iterator.class);

    doReturn(mockIterator).when(mockIterable).iterator();
    doReturn(false).when(mockIterator).hasNext();

    try {
      Assert.notEmpty(mockIterable);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Iterable is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockIterable, times(1)).iterator();
      verify(mockIterator, times(1)).hasNext();
      verifyNoMoreInteractions(mockIterable, mockIterator);
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyIterableWithFormatsMessageWithPlaceholderValues() {

    Iterable<?> mockIterable = mock(Iterable.class);

    try {
      Assert.notEmpty(mockIterable, "%s is {1}", mockIterable, "empty");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("%s is empty", mockIterable);
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockIterable, times(1)).iterator();
      verifyNoMoreInteractions(mockIterable);
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyIterableUsesSuppliedMessageThrowsIllegalArgumentException() {

    Iterable<?> mockIterable = mock(Iterable.class);

    try {
      Assert.notEmpty(mockIterable, () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockIterable, times(1)).iterator();
      verifyNoMoreInteractions(mockIterable);
    }
  }

  @Test(expected = AssertionException.class)
  public void assertNotEmptyIterableThrowsAssertionException() {

    Iterable<?> mockIterable = mock(Iterable.class);

    try {
      Assert.notEmpty(mockIterable, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockIterable, times(1)).iterator();
      verifyNoMoreInteractions(mockIterable);
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyMapWithNullMap() {

    try {
      Assert.notEmpty((Map<?, ?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyMapWithEmptyMap() {

    try {
      Assert.notEmpty(Collections.emptyMap());
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyMapFormatsMessageWithPlaceholderValues() {

    try {
      Assert.notEmpty(Collections.emptyMap(), "%1$s is {1}", "Map", "empty");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map is empty");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotEmptyMapWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notEmpty(Collections.singletonMap("key", "value"), mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotEmptyMapUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.notEmpty(Collections.emptyMap(), () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertNotEmptyMapThrowsAssertionException() {

    try {
      Assert.notEmpty(Collections.emptyMap(), new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IllegalArgumentException.class)
  public void assertNotNullWithNull() {

    try {
      Assert.notNull(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Argument is null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotNullWithNullReturningMethod() {

    try {
      Assert.notNull(returnNull());
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Argument is null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotNullFormatsMessageWithPlaceholderValues() {

    try {
      Assert.notNull(null, "%1$s is {1}", "Object reference", null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Object reference is null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotNullWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notNull("test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertNotNullUsesSuppliedMessageThrowsIllegalArgumentException() {

    try {
      Assert.notNull(null, () -> "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertNotNullThrowsAssertionException() {

    try {
      Assert.notNull(null, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IdentityException.class)
  public void assertNotSameWithIdenticalBooleanValues() {

    try {
      Assert.notSame(false, Boolean.FALSE);
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[false] is the same as [false]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assertNotSameWithIdenticalCharacterValues() {

    try {
      Assert.notSame('x', 'x');
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[x] is the same as [x]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assertNotSameWithIdenticalDoubleValues() {

    Double value = 2.0d;

    try {
      Assert.notSame(value, value);
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[2.0] is the same as [2.0]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assertNotSameWithIdenticalIntegerValues() {

    try {
      Assert.notSame(2, 2);
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[2] is the same as [2]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assertNotSameWithIdenticalStringValues() {

    try {
      Assert.notSame("test", "test");
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[test] is the same as [test]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assertNotSameWithIdenticalObjectValues() {

    try {
      Assert.notSame(LOCK, LOCK);
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[%1$s] is the same as [%1$s]", LOCK);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assertNotSameFormatsMessageWithPlaceholderValues() {

    try{
      Assert.notSame(1, 1, "%1$s are the {1}", "Integers", "same");
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("Integers are the same");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertNotSameWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.notSame(1, 1.0, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IdentityException.class)
  public void assertNotSameUsesSuppliedMessageThrowsIdentityException() {

    try {
      Assert.notSame(1, 1, () -> "test");
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertNotSameThrowsAssertException() {

    try {
      Assert.notSame(1, 1, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
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

  @Test(expected = IdentityException.class)
  public void assertSameWithCharacterAndString() {

    try {
      Assert.same('x', "x");
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[x] is not the same as [x]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assertSameWithDoubleAndInteger() {

    try {
      Assert.same(1.0d, 1);
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[1.0] is not the same as [1]");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IdentityException.class)
  public void assetSameWithNullAndObject() {

    Object value = new Object();

    try {
      Assert.same(null, value);
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("[null] is not the same as [%s]", value);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertSameFormatsMessageWithPlaceholderValues() {

    try {
      Assert.same("test", "TEST", "%1$s are not {1}", "Strings", "identical");
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("Strings are not identical");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertSameWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.same("test", "test", mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IdentityException.class)
  public void assertSameUsesSuppliedMessageThrowsIdentityException() {

    try {
      Assert.same(1, -1, () -> "test");
    }
    catch (IdentityException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertSameThrowsAssertionException() {

    try {
      Assert.same(new Object(), new Object(), new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertStateIsValid() {
    Assert.state(true);
  }

  @Test(expected = IllegalStateException.class)
  public void assertStateIsInvalid() {

    try {
      Assert.state(false);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("State is invalid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void assertStateWithNullIsNullSafe() {

    try {
      Assert.state(null);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("State is invalid");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void assertStateFormatsMessageWithPlaceholderValues() {

    try {
      Assert.state(Boolean.FALSE, "%1$s not {1}", "Object", "initialized");
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("Object not initialized");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertStateWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.state(true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = IllegalStateException.class)
  public void assertStateUsesSuppliedMessageThrowsIllegalStateException() {

    try {
      Assert.state(false, () -> "test");
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertStateThrowsAssertionException() {

    try {
      Assert.state(false, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertSupportedForSupportedOperation() {
    Assert.supported(true);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void assertSupportedForUnsupportedOperation() {

    try {
      Assert.supported(false);
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Operation not supported");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void assertSupportedWithNullIsNullSafe() {

    try {
      Assert.supported(null);
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Operation not supported");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = UnsupportedOperationException.class)
  public void assertSupportedFormatsMessageWithPlaceholderValues() {

    try {
      Assert.supported(Boolean.FALSE, "%1$s is {1}", "Write operation", "unsupported");
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Write operation is unsupported");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void assertSupportedWithSuppliedMessage() {

    Supplier<String> mockSupplier = mock(Supplier.class);

    Assert.supported(true, mockSupplier);

    verify(mockSupplier, never()).get();
    verifyNoInteractions(mockSupplier);
  }

  @Test(expected = UnsupportedOperationException.class)
  public void assertSupportedUsesSuppliedMessageThrowsUnsupportedOperationException() {

    try {
      Assert.supported(false, () -> "test");
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertSupportedThrowsAssertionException() {

    try {
      Assert.supported(false, new AssertionException("test"));
    }
    catch (AssertionException expected) {

      assertThat(expected).hasMessage("test");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  private enum TestEnum {
    ONE,
    TWO
  }
}
