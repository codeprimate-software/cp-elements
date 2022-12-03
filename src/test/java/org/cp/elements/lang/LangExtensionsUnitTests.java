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

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.LangExtensions.$;
import static org.cp.elements.lang.LangExtensions.AssertThat;
import static org.cp.elements.lang.LangExtensions.AssertThatWrapper;
import static org.cp.elements.lang.LangExtensions.Is;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.LangExtensions.from;
import static org.cp.elements.lang.LangExtensions.given;
import static org.cp.elements.lang.LangExtensions.is;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.CollectionUtils.nullSafeList;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Predicate;

import org.junit.Test;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.LangExtensions.Given;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ComparatorUtils;

import org.assertj.core.api.Assertions;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link LangExtensions}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.LangExtensions
 * @see org.cp.elements.test.TestUtils
 * @see lombok
 * @since 1.0.0
 */
public class LangExtensionsUnitTests {

  @SuppressWarnings("rawtypes")
  private final Comparable NULL = null;

  private final Condition ENABLE_DISABLE_CONDITION = Condition.FALSE_CONDITION;

  private final Object lock = new Object();

  @Test
  public void assertThatAsTypedAssertion() {

    Object jonDoe = User.as("jonDoe");

    assertThat(jonDoe).as(User.class).isComparableTo(User.as("jonDoe"));
  }

  @Test
  public void assertThatAsStringAssertion() {

    User jonDoe = User.as("jonDoe");

    assertThat(jonDoe).isNotNull();
    assertThat(jonDoe).asString().isEqualTo("jonDoe");
  }

  @Test
  public void assertThatAsConvertedTypedAssertion() {

    Person jonDoe = new Person(1L, "Jon", "Doe");

    Function<Person, User> toUserFunction = person ->
      User.as(person.getFirstName().toLowerCase() + person.getLastName());

    assertThat(jonDoe).asType(toUserFunction).isEqualTo(User.as("jonDoe"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatAsWithNullConverterFunction() {

    try {
      assertThat("mock").asType(null).isEqualTo("test");
    }
    catch (IllegalArgumentException expected) {

      Assertions.assertThat(expected).hasMessage("The Function used to convert the target (subject) is required");
      Assertions.assertThat(expected).hasNoCause();

      throw expected;
    }
  }

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
    assertThat("mock").isAssignableTo(Object.class);
    assertThat(new Object()).isAssignableTo(Object.class);
  }

  @Test
  public void assertThatObjectIsNotAssignableToClassType() {

    assertThat("false").not().isAssignableTo(Boolean.class);
    assertThat("test").not().isAssignableTo(Character.class);
    assertThat(123.45f).not().isAssignableTo(Double.class);
    assertThat(3.14159d).not().isAssignableTo(Float.class);
    assertThat(123L).not().isAssignableTo(Integer.class);
    assertThat(123).not().isAssignableTo(Long.class);
    assertThat('c').not().isAssignableTo(String.class);
    assertThat(new Object()).not().isAssignableTo(String.class);
  }

  @Test(expected = AssertionException.class)
  public void assertThatObjectIsAssignableToStringThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(Object.class)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isAssignableTo(String.class),
        () -> "This is a test!");
  }

  @Test(expected = AssertionException.class)
  public void assertThatDoubleIsAssignableToIntegerThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(Math.PI).isAssignableTo(Integer.class),
      () -> String.format("[%1$s] is not assignable to [java.lang.Integer]", Math.PI));
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatCharacterIsAssignableToStringThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(Character.class).throwing(new IllegalArgumentException("test")).isAssignableTo(String.class),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatStringIsNotAssignableToObjectThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(String.class).not().isAssignableTo(Object.class),
      () -> "[class java.lang.String] is assignable to [java.lang.Object]");
  }

  @Test
  public void assertThatObjectIsComparableToObject() {

    assertThat(true).isComparableTo(Boolean.TRUE);
    assertThat('c').isComparableTo('c');
    assertThat(3.14159d).isComparableTo(3.14159d);
    assertThat(123).isComparableTo(123);
    assertThat("test").isComparableTo("test");
  }

  @Test
  public void assertThatObjectIsNotComparableToObject() {

    assertThat(Boolean.FALSE).isNotComparableTo(true);
    assertThat('c').isNotComparableTo('C');
    assertThat(3.14159d).isNotComparableTo(Math.PI);
    assertThat(123).isNotComparableTo(-123);
    assertThat("test").isNotComparableTo("mock");
  }

  @Test(expected = AssertionException.class)
  public void assertThatObjectsAreNotComparableThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("queue").isComparableTo("Q"),
      () -> "[queue] is not comparable to [Q]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatObjectsAreNotComparableThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat("c").describedAs("This is a %1$s{1}", "test", "!").isComparableTo("see"),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatObjectsAreNotComparableThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat("c").throwing(new IllegalArgumentException("test")).isComparableTo("see"),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatComparableObjectsAreNotComparableThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").not().isComparableTo("test"),
      () -> "[test] is comparable to [test]");
  }

  @Test
  @SuppressWarnings("all")
  public void assertThatObjectIsEqualToObject() {

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

  @Test(expected = AssertionException.class)
  public void assertThatObjectIsEqualToObjectThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").isEqualTo("mock"),
      () -> "[test] is not equal to [mock]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatObjectIsEqualToObjectThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat("r").describedAs("This is a %1$s{1}", "test", "!").isEqualTo("are"),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatObjectIsEqualToObjectThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat("r").throwing(new IllegalArgumentException("test")).isEqualTo("are"),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatObjectIsNotEqualToObjectThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").isNotEqualTo("test"),
      () -> "[test] is equal to [test]");
  }

  @Test
  public void assertThatFalseIsFalse() {

    assertThat(false).isFalse();
    assertThat(Boolean.FALSE).isFalse();
  }

  @Test
  public void assertThatTrueIsNotFalse() {

    assertThat(true).not().isFalse();
    assertThat(Boolean.TRUE).not().isFalse();
  }

  @Test(expected = AssertionException.class)
  public void assertThatTrueIsFalseThrowsAssertionException() {
    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(true).isFalse(), () -> "[true] is not false");
  }

  @Test(expected = AssertionException.class)
  public void assertThatTrueIsFalseThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(true).describedAs("This is a %1$s{1}", "test", "!").isFalse(),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatTrueIsFalseThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(true).throwing(new IllegalArgumentException("test")).isFalse(),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatFalseIsNotFalseThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(false).not().isFalse(),
      () -> "[false] is false");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(2).isGreaterThan(2),
      () -> "[2] is not greater than [2]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).describedAs("This is a %1$s{1}", "test", "!").isGreaterThan(3),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsGreaterThanThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThan(3),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotGreaterThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(2).not().isGreaterThan(1),
      () -> "[2] is greater than [1]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanAndLessThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).isGreaterThanAndLessThan(2, 2),
        () -> "[2] is not greater than [2] and less than [2]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanAndLessThanThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThanAndLessThan(2, 2),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsGreaterThanAndLessThanThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2)
        .throwing(new IllegalArgumentException("test"))
        .isGreaterThanAndLessThan(2, 2),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotGreaterThanAndLessThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).not().isGreaterThanAndLessThan(1, 3),
        () -> "[2] is greater than [1] and less than [3]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).isGreaterThanAndLessThanEqualTo(2, 2),
        () -> "[2] is not greater than [2] and less than equal to [2]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThanAndLessThanEqualTo(2, 2),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2)
        .throwing(new IllegalArgumentException("test"))
        .isGreaterThanAndLessThanEqualTo(2, 2),
      () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotGreaterThanAndLessThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).not().isGreaterThanAndLessThanEqualTo(1, 2),
        () -> "[2] is greater than [1] and less than equal to [2]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(2).isGreaterThanEqualTo(3),
      () -> "[2] is not greater than equal to [3]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).describedAs("This is a %1$s{1}", "test", "!").isGreaterThanEqualTo(3),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsGreaterThanEqualToThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2).throwing(new IllegalArgumentException("test")).isGreaterThanEqualTo(3),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotGreaterThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(2).not().isGreaterThanEqualTo(1),
      () -> "[2] is greater than equal to [1]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).isGreaterThanEqualToAndLessThan(3, 1),
        () -> "[2] is not greater than equal to [3] and less than [1]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2)
        .describedAs("This is a %1s{1}", "test", "!")
        .isGreaterThanEqualToAndLessThan(3, 1),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2)
        .throwing(new IllegalArgumentException("test"))
        .isGreaterThanEqualToAndLessThan(3, 1),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotGreaterThanEqualToAndLessThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).not().isGreaterThanEqualToAndLessThan(1, 3),
        () -> "[2] is greater than equal to [1] and less than [3]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(3, 1),
        () -> "[2] is not greater than equal to [3] and less than equal to [1]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThanEqualToAndLessThanEqualTo(3, 1),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2)
        .throwing(new IllegalArgumentException("test"))
        .isGreaterThanEqualToAndLessThanEqualTo(3, 1),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotGreaterThanEqualToAndLessThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).not().isGreaterThanEqualToAndLessThanEqualTo(1, 3),
        () -> "[2] is greater than equal to [1] and less than equal to [3]");
  }

  @Test
  public void assertThatStringsHaveText() {

    assertThat("test").hasText();
    assertThat("null").isNotBlank();
    assertThat("empty").hasText();
    assertThat("blank").hasText();
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

  @Test(expected = AssertionException.class)
  public void assertThatBlankStringHasTextThrowsAssertionException() {
    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(" ").hasText(), () -> "[ ] is blank");
  }

  @Test(expected = AssertionException.class)
  public void assertThatEmptyStringHasTextThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat("").describedAs("This is a %1$s{1}", "test", "!").isNotBlank(),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatNullStringHasTextThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(null).throwing(new IllegalArgumentException("test")).hasText(),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatStringDoesNotHaveTextThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").not().hasText(),
      () -> "[test] is not blank");
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

  @Test(expected = AssertionException.class)
  public void assertThatCurrentThreadHoldsLockThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(Thread.currentThread()).holdsLock(lock),
      () -> String.format("[%1$s] does not hold lock [%2$s]", Thread.currentThread(), lock));
  }

  @Test(expected = AssertionException.class)
  public void assertThatCurrentThreadHoldsLockThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(Thread.currentThread())
        .describedAs("This is a %1$s{1}", "test", "!")
        .holdsLock(lock),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatCurrentThreadHoldsLockThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(Thread.currentThread())
        .throwing(new IllegalArgumentException("test"))
        .holdsLock(lock),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatCurrentThreadDoesNotHoldLockThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> {
      synchronized (lock) {
        assertThat(Thread.currentThread()).not().holdsLock(lock);
      }
    }, () -> String.format("[%1$s] holds lock [%2$s]", Thread.currentThread(), lock));
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
    assertThat("mock").isInstanceOf(Object.class);
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

  @Test(expected = AssertionException.class)
  public void assertThatStringIsInstanceOfCharacterThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").isInstanceOf(Character.class),
      () -> "[test] is not an instance of [java.lang.Character]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatStringIsInstanceOfCharacterThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat("test")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isInstanceOf(Character.class),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatStringIsInstanceOfCharacterThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat("test")
        .throwing(new IllegalArgumentException("test"))
        .isInstanceOf(Character.class),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatStringIsNotInstanceOfStringThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").not().isInstanceOf(String.class),
      () -> "[test] is an instance of [java.lang.String]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(2).isLessThan(1),
      () -> "[2] is not less than [1]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).describedAs("This is a %1$s{1}", "test", "!").isLessThan(1),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsLessThanThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2).throwing(new IllegalArgumentException("test")).isLessThan(1),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotLessThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(1).not().isLessThan(2),
      () -> "[1] is less than [2]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanOrGreaterThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0).isLessThanOrGreaterThan(-1, 1),
        () -> "[0] is not less than [-1] or greater than [1]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanOrGreaterThanThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanOrGreaterThan(-1, 1),
        ()  -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsLessThanOrGreaterThanThrowsIllegalArgumentException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0)
        .throwing(new IllegalArgumentException("test"))
        .isLessThanOrGreaterThan(-1, 1),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotLessThanOrGreaterThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(3).not().isLessThanOrGreaterThan(-1, 1),
        () -> "[3] is less than [-1] or greater than [1]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () ->  assertThat(0).isLessThanOrGreaterThanEqualTo(-1, 1),
        () -> "[0] is not less than [-1] or greater than equal to [1]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanOrGreaterThanEqualTo(-1, 1),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(0)
        .throwing(new IllegalArgumentException("test"))
        .isLessThanOrGreaterThanEqualTo(-1, 1),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotLessThanOrGreaterThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(1).not().isLessThanOrGreaterThanEqualTo(-1, 1),
        () -> "[1] is less than [-1] or greater than equal to [1]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(2).isLessThanEqualTo(-3),
      () -> "[2] is not less than equal to [-3]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).describedAs("This is a %1$s{1}", "test", "!").isLessThanEqualTo(-3),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsLessThanEqualToThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(2).throwing(new IllegalArgumentException("test")).isLessThanEqualTo(-3),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotLessThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(2).not().isLessThanEqualTo(3),
      () -> "[2] is less than equal to [3]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0).isLessThanEqualToOrGreaterThan(-1, 1),
        () -> "[0] is not less than equal to [-1] or greater than [1]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanEqualToOrGreaterThan(-1, 1),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(0)
        .throwing(new IllegalArgumentException("test"))
        .isLessThanEqualToOrGreaterThan(-1, 1),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotLessThanEqualToOrGreaterThanThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).not().isLessThanEqualToOrGreaterThan(-1, 1),
        () -> "[2] is less than equal to [-1] or greater than [1]");
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

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0).isLessThanEqualToOrGreaterThanEqualTo(-1, 1),
        () -> "[0] is not less than equal to [-1] or greater than equal to [1]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanEqualToOrGreaterThanEqualTo(-1, 1),
        ()  -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(0)
        .throwing(new IllegalArgumentException("test"))
        .isLessThanEqualToOrGreaterThanEqualTo(-1, 1),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotLessThanEqualToOrGreaterThanEqualToThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(2).not().isLessThanEqualToOrGreaterThanEqualTo(-1, 1),
        () -> "[2] is less than equal to [-1] or greater than equal to [1]");
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

  @Test(expected = AssertionException.class)
  public void assertThatNonNullObjectIsNullThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").isNull(), () -> "[test] is not null");
  }

  @Test(expected = AssertionException.class)
  public void assertThatNonNullObjectIsNullThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat("test").describedAs("This is a %1$s{1}", "test", "!").isNull(),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatNonNullObjectIsNullThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat("test").throwing(new IllegalArgumentException("test")).isNull(),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatNullObjectIsNotNullThrowsAssertionException() {
    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(null).isNotNull(), () -> "[null] is null");
  }

  @Test
  public void assertThatNonEmptyStringsAreNotEmpty() {

    assertThat("test").isNotEmpty();
    assertThat("null").isNotEmpty();
    assertThat("empty").isNotEmpty();
    assertThat("blank").isNotEmpty();
    assertThat("0").isNotEmpty();
    assertThat("  ").isNotEmpty();
    assertThat(" ").isNotEmpty();
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

  @Test(expected = AssertionException.class)
  public void assertThatEmptyStringIsNotEmptyThrowsAssertionException() {
    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("").isNotEmpty(), () -> "[] is empty");
  }

  @Test(expected = AssertionException.class)
  public void assertThatEmptyStringIsNotEmptyThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat("").describedAs("This is a %1$s{1}", "test", "!").isNotBlank(),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatEmptyStringIsNotEmptyThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat("").throwing(new IllegalArgumentException("test")).isNotBlank(),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatNonEmptyStringIsEmptyThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(" ").not().isNotEmpty(),
      () -> "[ ] is not empty");
  }

  @Test
  public void assertThatNonBlankStringsAreNotBlank() {

    assertThat("test").isNotBlank();
    assertThat("mock").isNotBlank();
    assertThat("empty").isNotBlank();
    assertThat("blank").isNotBlank();
    assertThat("_").isNotBlank();
    assertThat("0").isNotBlank();
    assertThat("nil").isNotBlank();
    assertThat("null").isNotBlank();
  }

  @Test
  public void assertThatBlankStringsAreBlank() {

    assertThat(null).not().isNotBlank();
    assertThat("").not().isNotBlank();
    assertThat("  ").not().isNotBlank();
    assertThat("\t").not().isNotBlank();
    assertThat("\n").not().isNotBlank();
    assertThat("\0").not().isNotBlank();
  }

  @Test(expected = AssertionException.class)
  public void assertThatBlankStringThrowsAssertionException() {
    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("  ").isNotBlank(), () -> "[  ] is blank");
  }

  @Test(expected = AssertionException.class)
  public void assertThatBlankStringThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("  ")
      .describedAs("This is a %1$s{1}", "test", "!")
      .isNotBlank(),
      () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatBlankStringThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> assertThat("  ")
      .throwing(new IllegalArgumentException("test"))
      .isNotBlank(), () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatNonBlankStringIsBlankThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("nonBlank").not().isNotBlank(),
      () -> "[nonBlank] is not blank");
  }

  @Test
  public void assertThatObjectIsSameAsObject() {

    Double pi = Math.PI;

    assertThat(true).isSameAs(Boolean.TRUE);
    assertThat('c').isSameAs('c');
    assertThat(pi).isSameAs(pi);
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

  @Test(expected = AssertionException.class)
  public void assertThatNonIdenticalObjectsAreTheSameThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").isSameAs("TEST"),
      () -> "[test] is not the same as [TEST]");
  }

  @Test(expected = AssertionException.class)
  public void assertThatNonIdenticalObjectsAreTheSameThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat("test").describedAs("This is a %1$s{1}", "test", "!").isSameAs("TEST"),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatNonIdenticalObjectsAreTheSameThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat("test").throwing(new IllegalArgumentException("test")).isSameAs("TEST"),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatIdenticalObjectsAreNotTheSameThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").isNotSameAs("test"),
      () -> "[test] is the same as [test]");
  }

  @Test
  public void assertThatTrueIsTrue() {

    assertThat(true).isTrue();
    assertThat(Boolean.TRUE).isTrue();
  }

  @Test
  public void assertThatFalseIsNotTrue() {

    assertThat(false).not().isTrue();
    assertThat(Boolean.FALSE).not().isTrue();
  }

  @Test(expected = AssertionException.class)
  public void assertThatFalseIsTrueThrowsAssertionException() {
    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(false).isTrue(), () -> "[false] is not true");
  }

  @Test(expected = AssertionException.class)
  public void assertThatFalseIsTrueThrowsAssertionExceptionWithCustomMessage() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(false).describedAs("This is a %1$s{1}", "test", "!").isTrue(),
        () -> "This is a test!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatFalseIsTrueThrowsIllegalArgumentException() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(false).throwing(new IllegalArgumentException("test")).isTrue(),
        () -> "test");
  }

  @Test(expected = AssertionException.class)
  public void assertThatTrueIsNotTrueThrowsAssertionException() {
    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(true).not().isTrue(), () -> "[true] is true");
  }

  @Test
  public void assertThatIsValidIsValid() {
    assertThat("test").isValid(argument -> true);
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsValidIsNotValid() {

    try {
      assertThat("test").isValid(argument -> false);
    }
    catch (AssertionException expected) {

      Assertions.assertThat(expected).hasMessage("[test] is not valid");
      Assertions.assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsValidWithNullPredicate() {

    try {
      assertThat("test").isValid(null);
    }
    catch (IllegalArgumentException expected) {

      Assertions.assertThat(expected).hasMessage("Predicate is required");
      Assertions.assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsValidUsingCustomMessage() {

    try {
      assertThat("test")
        .describedAs("[%s] is invalid", "test")
        .isValid(argument -> false);
    }
    catch (AssertionException expected) {

      Assertions.assertThat(expected).hasMessage("[test] is invalid");
      Assertions.assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void assertThatIsValidThrowingIllegalArgumentException() {

    try {
      assertThat("test")
        .throwing(new IllegalArgumentException("[test] is invalid"))
        .isValid(argument -> false);
    }
    catch (IllegalArgumentException expected) {

      Assertions.assertThat(expected).hasMessage("[test] is invalid");
      Assertions.assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void assertThatIsValidConditionally() {

    assertThat("test")
      .when(Condition.FALSE_CONDITION)
      .isValid(argument -> false);
  }

  @Test
  public void assertThatIsNotValidWithNonValidValue() {
    assertThat("test").not().isValid(argument -> false);
  }

  @Test(expected = AssertionException.class)
  public void assertThatIsNotValidWithValidValueThrowsAssertionException() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat("test").not().isValid(argument -> true),
      () -> "[test] is valid");
  }

  @Test(expected = AssertionException.class)
  public void assertionTransformationIgnoresCondition() {

    TestUtils.doAssertionExceptionThrowingOperation(() ->
      assertThat(false).transform(new Transformer<AssertThat<Boolean>>() {
        @Override public AssertThat<Boolean> transform(final AssertThat<Boolean> assertion) {
          return new AssertThatWrapper<Boolean>(assertion) {
            @Override public AssertThat<Boolean> when(final Condition condition) {
              assertion.when(Condition.TRUE_CONDITION);
              return this;
            }
          };
        }
      }).when(ENABLE_DISABLE_CONDITION).isTrue(), () -> "[false] is not true");
  }

  @Test(expected = AssertionException.class)
  public void assertionDescribeAsUsingStringAndArgumentsIsCorrect() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(true).describedAs("This is a %s {1}!", "boolean", "test").isFalse(),
        () -> "This is a boolean test!");
  }

  @Test(expected = AssertionException.class)
  public void assertionDescribeAsUsingSupplierIsCorrect() {

    TestUtils.doAssertionExceptionThrowingOperation(() -> assertThat(true).describedAs(() -> "test").isFalse(),
      () -> "test");
  }

  @Test
  public void disabledAssertThatIsAssignableToSuppressesAssertionException() {
    assertThat(1).when(ENABLE_DISABLE_CONDITION).isAssignableTo(Boolean.class);
  }

  @Test
  public void disabledAssertThatIsComparableToSuppressesAssertionException() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isComparableTo("TEST");
  }

  @Test
  public void disabledAssertThatIsNotComparableToSuppressesAssertionException() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isNotComparableTo("test");
  }

  @Test
  public void disabledAssertThatIsEqualToSuppressesAssertionException() {
    assertThat(3.1459d).when(ENABLE_DISABLE_CONDITION).isEqualTo(Math.PI);
  }

  @Test
  public void disabledAssertThatIsNotEqualToSuppressesAssertionException() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isNotEqualTo("test");
  }

  @Test
  public void disabledAssertThatIsFalseSuppressesAssertionException() {
    assertThat(true).when(ENABLE_DISABLE_CONDITION).isFalse();
  }

  @Test
  public void disabledAssertThatIsGreaterThanSuppressesAssertionException() {
    assertThat(-2).when(ENABLE_DISABLE_CONDITION).isGreaterThan(1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanAndLessThanSuppressesAssertionException() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanAndLessThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanAndLessThanEqualToSuppressesAssertionException() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanAndLessThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanEqualToSuppressesAssertionException() {
    assertThat(-2).when(ENABLE_DISABLE_CONDITION).isGreaterThanEqualTo(1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanEqualToAndLessThanSuppressesAssertionException() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanEqualToAndLessThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsGreaterThanEqualToAndLessThanEqualToSuppressesAssertionException() {
    assertThat(2).when(ENABLE_DISABLE_CONDITION).isGreaterThanEqualToAndLessThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatHasTextSuppressesAssertionException() {
    assertThat("  ").when(ENABLE_DISABLE_CONDITION).hasText();
  }

  @Test
  public void disabledAssertThatHoldsLockSuppressesAssertionException() {
    assertThat(Thread.currentThread()).when(ENABLE_DISABLE_CONDITION).holdsLock(lock);
  }

  @Test
  public void disabledAssertThatIsInstanceOfSuppressesAssertionException() {
    assertThat(null).when(ENABLE_DISABLE_CONDITION).isInstanceOf(Object.class);
  }

  @Test
  public void disabledAssertThatIsLessThanSuppressesAssertionException() {
    assertThat(1).when(ENABLE_DISABLE_CONDITION).isLessThan(-2);
  }

  @Test
  public void disabledAssertThatIsLessThanOrGreaterThanSuppressesAssertionException() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanOrGreaterThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsLessThanOrGreaterThanEqualToSuppressesAssertionException() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatIsLessThanEqualToSuppressesAssertionException() {
    assertThat(1).when(ENABLE_DISABLE_CONDITION).isLessThanEqualTo(-1);
  }

  @Test
  public void disabledAssertThatIsLessThanEqualToOrGreaterThanSuppressesAssertionException() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanEqualToOrGreaterThan(-1, 1);
  }

  @Test
  public void disabledAssertThatIsLessThanEqualToOrGreaterThanEqualToSuppressesAssertionException() {
    assertThat(0).when(ENABLE_DISABLE_CONDITION).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);
  }

  @Test
  public void disabledAssertThatIsNotBlankSuppressesAssertionException() {
    assertThat(" ").when(ENABLE_DISABLE_CONDITION).isNotBlank();
  }

  @Test
  public void disabledAssertThatIsNotEmptySuppressesAssertionException() {
    assertThat("").when(ENABLE_DISABLE_CONDITION).isNotEmpty();
  }

  @Test
  public void disabledAssertThatIsNotNullSuppressesAssertionException() {
    assertThat(null).when(ENABLE_DISABLE_CONDITION).isNotNull();
  }

  @Test
  public void disabledAssertThatIsNullSuppressesAssertionException() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isNull();
  }

  @Test
  public void disabledAssertThatIsSameAsSuppressesAssertionException() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isSameAs("TEST");
  }

  @Test
  public void disabledAssertThatIsNotSameAsSuppressesAssertionException() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isNotSameAs("test");
  }

  @Test
  public void disabledAssertThatIsTrueSuppressesAssertionException() {
    assertThat(false).when(ENABLE_DISABLE_CONDITION).isTrue();
  }

  @Test
  public void disableAssertThatIsValidSuppressesAssertionException() {
    assertThat("test").when(ENABLE_DISABLE_CONDITION).isValid(argument -> false);
  }

  @Test(expected = IllegalArgumentException.class)
  public void negatedAssertThatRetainsThrowable() {

    TestUtils.doIllegalArgumentExceptionThrowingOperation(
      () -> assertThat(true).throwing(new IllegalArgumentException("test")).not().isTrue(),
        () -> "test");
  }

  @Test
  public void negatedAssertThatRetainsTransformer() {

    AtomicInteger holdsLockCallCount = new AtomicInteger(0);

    Object lock = new Object();

    Transformer<AssertThat<Thread>> assertThatTransformer = (AssertThat<Thread> assertion) ->
      new AssertThatWrapper<Thread>(assertion) {
        @Override public AssertThat<Thread> holdsLock(final Object lock) {
          holdsLockCallCount.incrementAndGet();
          assertion.holdsLock(lock);
          return this;
        }
      };

    assertThat(Thread.currentThread()).transform(assertThatTransformer).not().holdsLock(lock);

    Assertions.assertThat(holdsLockCallCount.get()).isOne();
  }

  @Test(expected = AssertionException.class)
  public void negatedAssertThatRetainsExceptionMessageAndArgs() {

    TestUtils.doAssertionExceptionThrowingOperation(
      () -> assertThat(null).describedAs("%1$s cannot be {1}", "Object", "null").isNotNull(),
        () -> "Object cannot be null");
  }

  @Test
  public void negatedAssertThatRetainsCondition() {
    assertThat(false).when(Condition.FALSE_CONDITION).not().isFalse();
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatAsTypedAssertionDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).as(User.class);

    verify(mockAssertion, times(1)).as(eq(User.class));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatAsStringAssertionDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).asString();

    verify(mockAssertion, times(1)).asString();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatAsConvertedTypedAssertionDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    Function<Person, User> mockFunction = mock(Function.class);

    AssertThatWrapper.wrap(mockAssertion).asType(mockFunction);

    verify(mockAssertion, times(1)).asType(mockFunction);
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsAssignableToDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isAssignableTo(Object.class);

    verify(mockAssertion, times(1)).isAssignableTo(eq(Object.class));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsComparableToDelegatesToWrappedAssertion() {

    Comparable mockComparable = mock(Comparable.class);

    AssertThat<Comparable> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isComparableTo(mockComparable);

    verify(mockAssertion, times(1)).isComparableTo(eq(mockComparable));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsNotComparableToDelegatesToWrappedAssertion() {

    Comparable mockComparable = mock(Comparable.class);

    AssertThat<Comparable> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isNotComparableTo(mockComparable);

    verify(mockAssertion, times(1)).isNotComparableTo(eq(mockComparable));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsEqualToDelegatesToWrappedAssertion() {

    Object obj = new Object();

    AssertThat<Object> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isEqualTo(obj);

    verify(mockAssertion, times(1)).isEqualTo(eq(obj));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsNotEqualToDelegatesToWrappedAssertion() {

    Object obj = new Object();

    AssertThat<Object> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isNotEqualTo(obj);

    verify(mockAssertion, times(1)).isNotEqualTo(eq(obj));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsFalseDelegatesToWrappedAssertion() {

    AssertThat<Boolean> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isFalse();

    verify(mockAssertion, times(1)).isFalse();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isGreaterThan(0);

    verify(mockAssertion, times(1)).isGreaterThan(eq(0));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanOrLessThanDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isGreaterThanAndLessThan(-1, 1);

    verify(mockAssertion, times(1)).isGreaterThanAndLessThan(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanOrLessThanEqualToDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isGreaterThanAndLessThanEqualTo(-1, 1);

    verify(mockAssertion, times(1)).isGreaterThanAndLessThanEqualTo(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanEqualToDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isGreaterThanEqualTo(0);

    verify(mockAssertion, times(1)).isGreaterThanEqualTo(eq(0));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanEqualToAndLessThanDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isGreaterThanEqualToAndLessThan(-1, 1);

    verify(mockAssertion, times(1)).isGreaterThanEqualToAndLessThan(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsGreaterThanEqualToAndLessThanEqualToDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isGreaterThanEqualToAndLessThanEqualTo(-1, 1);

    verify(mockAssertion, times(1))
      .isGreaterThanEqualToAndLessThanEqualTo(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatHasTextDelegatesToWrappedAssertion() {

    AssertThat<String> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).hasText();

    verify(mockAssertion, times(1)).hasText();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatHoldsLockDelegatesToWrappedAssertion() {

    AssertThat<Thread> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).holdsLock(lock);

    verify(mockAssertion, times(1)).holdsLock(eq(lock));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsInstanceOfDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isInstanceOf(Object.class);

    verify(mockAssertion, times(1)).isInstanceOf(eq(Object.class));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isLessThan(0);

    verify(mockAssertion, times(1)).isLessThan(eq(0));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanOrGreaterThanDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isLessThanOrGreaterThan(-1, 1);

    verify(mockAssertion, times(1)).isLessThanOrGreaterThan(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanOrGreaterThanEqualToDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isLessThanOrGreaterThanEqualTo(-1, 1);

    verify(mockAssertion, times(1)).isLessThanOrGreaterThanEqualTo(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanEqualToDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isLessThanEqualTo(0);

    verify(mockAssertion, times(1)).isLessThanEqualTo(eq(0));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanEqualToOrGreaterThanDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isLessThanEqualToOrGreaterThan(-1, 1);

    verify(mockAssertion, times(1)).isLessThanEqualToOrGreaterThan(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsLessThanEqualToOrGreaterThanEqualToDelegatesToWrappedAssertion() {

    AssertThat<Integer> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isLessThanEqualToOrGreaterThanEqualTo(-1, 1);

    verify(mockAssertion, times(1))
      .isLessThanEqualToOrGreaterThanEqualTo(eq(-1), eq(1));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsNotBlankDelegatesToWrappedAssertion() {

    AssertThat<String> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isNotBlank();

    verify(mockAssertion, times(1)).isNotBlank();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsNotEmptyDelegatesToWrappedAssertion() {

    AssertThat<String> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isNotEmpty();

    verify(mockAssertion, times(1)).isNotEmpty();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsNotNullDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isNotNull();

    verify(mockAssertion, times(1)).isNotNull();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsNullDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isNull();

    verify(mockAssertion, times(1)).isNull();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsNotSameAsDelegatesToWrappedAssertion() {

    Object obj = new Object();

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isNotSameAs(obj);

    verify(mockAssertion, times(1)).isNotSameAs(eq(obj));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatIsSameAsDelegatesToWrappedAssertion() {

    Object obj = new Object();

    AssertThat mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isSameAs(obj);

    verify(mockAssertion, times(1)).isSameAs(eq(obj));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsTrueDelegatesToWrappedAssertion() {

    AssertThat<Boolean> mockAssertion = mock(AssertThat.class);

    AssertThatWrapper.wrap(mockAssertion).isTrue();

    verify(mockAssertion, times(1)).isTrue();
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void wrappedAssertThatIsValidDelegatesToWrappedAssertion() {

    AssertThat<Object> mockAssertion = mock(AssertThat.class);

    Predicate<Object> mockPredicate = mock(Predicate.class);

    AssertThatWrapper.wrap(mockAssertion).isValid(mockPredicate);

    verify(mockAssertion, times(1)).isValid(eq(mockPredicate));
    verifyNoMoreInteractions(mockAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatNotDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    when(mockAssertion.not()).thenReturn(mockAssertion);

    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).not();

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);

    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).not();
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatThrowingDelegatesToWrappedAssertion() {

    RuntimeException illegalArgument = new IllegalArgumentException("test");

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).throwing(illegalArgument);

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).throwing(eq(illegalArgument));
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatTransformDelegatesToWrappedAssertion() {

    Transformer<AssertThat> mockTransformer = mock(Transformer.class);

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion);

    when(mockTransformer.transform(any(AssertThat.class))).thenReturn(wrappedAssertion);

    wrappedAssertion = wrappedAssertion.transform(mockTransformer);

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockTransformer, times(1)).transform(eq(mockAssertion));
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatUsingDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).describedAs("message", "args");

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).stating(eq("message"), eq("args"));
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatWhenDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).when(ENABLE_DISABLE_CONDITION);

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    assertTrue(wrappedAssertion instanceof AssertThatWrapper);

    verify(mockAssertion, times(1)).when(eq(ENABLE_DISABLE_CONDITION));
  }

  @Test
  public void assertThatWithCompoundActionsIsCorrect() {

    assertThat(2)
      .isGreaterThanAndLessThan(1, 3)
      .isLessThanOrGreaterThan(10, 0)
      .isValid(argument -> argument % 2 == 0)
      .isNotNull();
  }

  @Test(expected = AssertionException.class)
  public void assertThatWithCompoundActionsShortCircuitsAndThrowsAssertionException() {

    AssertThat<Integer> assertThat = spy(assertThat(2));

    try {

      Assertions.assertThat(assertThat).isNotNull();
      Assertions.assertThat(ObjectUtils.invoke(assertThat, "getTarget", Integer.class)).isEqualTo(2);

      assertThat
        .isInstanceOf(Integer.class)
        .isGreaterThanAndLessThan(1, 3)
        .isValid(argument -> false)
        .isLessThanOrGreaterThan(3, 1);
    }
    catch (AssertionException expected) {

      Assertions.assertThat(expected).hasMessage("[2] is not valid");
      Assertions.assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(assertThat, times(1)).isInstanceOf(eq(Integer.class));
      verify(assertThat, times(1)).isGreaterThanAndLessThan(eq(1), eq(3));
      verify(assertThat, times(1)).isValid(any());
      verify(assertThat, never()).isLessThanOrGreaterThan(eq(3), eq(1));
      verifyNoMoreInteractions(assertThat);
    }
  }

  @Test
  public void fromObjectCastToInteger() {

    Object target = 2;
    Integer result = from(target).castTo(Integer.class);

    assertThat(result).isEqualTo(2);
  }

  @Test
  public void fromObjectCastToString() {

    Object target = "test";
    String result = from(target).castTo(String.class);

    assertThat(result).isEqualTo("test");
  }

  @Test
  public void fromObjectToCastIncompatibleType() {

    assertThatExceptionOfType(IllegalTypeException.class)
      .isThrownBy(() -> from("mock").castTo(Integer.class))
      .withMessage("Object [mock] is not an instance of Class [java.lang.Integer]")
      .withNoCause();
  }

  @Test
  public void fromObjectCastToNullType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> from(2).castTo(null))
      .withMessage("The Class type used to cast is required")
      .withNoCause();
  }

  @Test
  public void fromNullConvertToNumberType() {

    assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> from(null).convertTo(Byte.class))
      .havingMessage("Cannot convert [null] into a Byte")
      .causedBy(NumberFormatException.class)
      .withNoCause();
  }

  @Test
  public void fromNullConvertToStringIsNullSafe() {
    assertThat(from(null).convertTo(String.class)).isEqualTo("null");
  }

  @Test
  public void fromObjectConvertToNumber() {

    assertThat(from("2").convertTo(Integer.class)).isEqualTo(2);
    assertThat(from("3.14159").convertTo(Double.class)).isEqualTo(3.14159d);
  }

  @Test
  public void fromObjectConvertToString() {

    assertThat(from(true).convertTo(String.class)).isEqualTo("true");
    assertThat(from('x').convertTo(String.class)).isEqualTo("x");
    assertThat(from(2).convertTo(String.class)).isEqualTo("2");
    assertThat(from(3.14159d).convertTo(String.class)).isEqualTo("3.14159");
    assertThat(from("test").convertTo(String.class)).isEqualTo("test");
  }

  @Test
  public void fromObjectConvertToIncompatibleType() {

    assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> from("test").convertTo(Integer.class))
      .havingMessage("Cannot convert [test] into an Integer")
      .causedBy(NumberFormatException.class)
      .havingMessageContaining("test")
      .withNoCause();
  }

  @Test
  public void fromObjectConvertToNullType() {

    assertThatThrowableOfType(IllegalArgumentException.class)
      .isThrownBy(args -> from("MockUser").convertTo(null))
      .havingMessage("No SimpleTypeConversion exists for target type [null]")
      .withNoCause();
  }

  @Test
  public void fromStringConvertToUser() {

    assertThatThrowableOfType(IllegalArgumentException.class)
      .isThrownBy(args -> from("TestUser").convertTo(User.class))
      .havingMessage("No SimpleTypeConversion exists for target type [%s]", User.class.getName())
      .withNoCause();
  }

  @Test
  public void fromUserConvertToString() {
    assertThat(from(User.as("MockUser")).convertTo(String.class)).isEqualTo("MockUser");
  }

  @Test
  public void givenSimpleObjectTestReturnsTrue() {
    assertThat(given("test").expectThat("test"::equals).result()).isTrue();
  }

  @Test
  public void givenSimpleObjectTestReturnsFalse() {
    assertThat(given("test").expectThat("mock"::equals).result()).isFalse();
  }

  @Test
  public void givenSimpleObjectTestUsingThrowOnFailedExpectationsReturnsTrue() {

    assertThat(given(2)
      .expectThat(Objects::nonNull)
      .expectThat(Integers::isGreaterThanZero)
      .expectThat(NumberUtils::isEven)
      .throwOnFailedExpectations()
      .result()).isTrue();
  }

  @Test
  public void givenObjectWithNullExtractionFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> given("mock").thenGiven(null).result())
      .withMessage("Function used to extract a collaborator from target [mock] is required")
      .withNoCause();
  }

  @Test
  public void givenObjectWithNullTestPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> given("test").expectThat(null).result())
      .withMessage("Predicate used to test the target [test] is required")
      .withNoCause();
  }

  @Test
  public void givenNullObjectIsNullSafeTestReturnsFalse() {

    Given<Invoice> givenInvoice = given((Invoice) null)
      .expectThat(target -> target.getTotal().compareTo(BigDecimal.valueOf(100)) > 0);

    assertThat(givenInvoice).isNotNull();
    assertThat(givenInvoice.getTarget()).isNull();
    assertThat(givenInvoice.result()).isFalse();
  }

  @Test
  public void givenNullObjectTestUsingThrowOnFailedExpectations() {

    assertThatThrowableOfType(ExpectationException.class)
      .isThrownBy(args -> assertThat(given(null).throwOnFailedExpectations().result()).isTrue())
      .havingMessage("Target [null] has failed expectation(s)")
      .withNoCause();
  }

  @Test
  public void givenComposedObjectTestReturnsTrue() {

    Invoice invoice = TestInvoice.of(Collections.singletonList((TestLineItem.newLineItem(TestProduct
      .newProduct("Junk", BigDecimal.valueOf(16.69)), 1))));

    Given<?> givenInvoice = given(invoice)
      .expectThat(targetInvoice -> targetInvoice.getTotal().compareTo(BigDecimal.valueOf(50)) < 0)
      .thenGiven(targetInvoice -> targetInvoice.findBy("Junk"))
      .expectThat(targetLineItem -> targetLineItem.getQuantity() == 1)
      .thenGiven(LineItem::getProduct)
      .expectThat(targetProduct -> "Junk".equals(targetProduct.getName()))
      .expectThat(targetProduct -> targetProduct.getPrice().compareTo(BigDecimal.valueOf(16.69)) == 0);

    assertThat(givenInvoice.getTarget()).isInstanceOf(Product.class);
    assertThat(givenInvoice.result()).isTrue();
  }

  @Test
  public void givenComposedObjectTestReturnsFalse() {

    Invoice invoice = TestInvoice.of(Collections.singletonList((TestLineItem.newLineItem(TestProduct
      .newProduct("Junk", BigDecimal.valueOf(16.69)), 1))));

    Given<?> givenInvoice = given(invoice)
      .expectThat(targetInvoice -> targetInvoice.getTotal().compareTo(BigDecimal.valueOf(50.0d)) < 0)
      .thenGiven(targetInvoice -> targetInvoice.findBy("Junk"))
      .expectThat(targetLineItem -> targetLineItem.getQuantity() < 1)
      .thenGiven(LineItem::getProduct)
      .expectThat(targetProduct -> "Junk".equals(targetProduct.getName()))
      .expectThat(targetProduct -> targetProduct.getPrice().compareTo(BigDecimal.valueOf(16.69d)) == 0);

    assertThat(givenInvoice.getTarget()).isInstanceOf(Product.class);
    assertThat(givenInvoice.result()).isFalse();
  }

  @Test
  public void givenNullComposedObjectTestReturnsFalse() {

    Invoice invoice = TestInvoice.of(Collections.singletonList((TestLineItem.newLineItem(TestProduct
      .newProduct("Junk", BigDecimal.valueOf(16.69)), 1))));

    Given<?> givenInvoice = given(invoice)
      .expectThat(targetInvoice -> targetInvoice.getTotal().compareTo(BigDecimal.valueOf(50.0d)) < 0)
      .thenGiven(targetInvoice -> targetInvoice.findBy("NonJunk"))
      .expectThat(targetLineItem -> targetLineItem.getQuantity() == 1)
      .thenGiven(LineItem::getProduct)
      .expectThat(targetProduct -> "Junk".equals(targetProduct.getName()))
      .expectThat(targetProduct -> targetProduct.getPrice().compareTo(BigDecimal.valueOf(16.69d)) == 0);

    assertThat(givenInvoice.getTarget()).isNull();
    assertThat(givenInvoice.result()).isFalse();
  }

  @Test
  public void givenComplexObjectTestUsingThrowOnFailedExpectationsShortcircuits() {

    Invoice invoice = TestInvoice.of(Arrays.asList(
      TestLineItem.newLineItem(TestProduct.newProduct("ProductOne", BigDecimal.valueOf(20.0d)), 2),
      TestLineItem.newLineItem(TestProduct.newProduct("ProductTwo", BigDecimal.valueOf(10.0d)), 1)
    ));

    assertThatThrowableOfType(ExpectationException.class)
      .isThrownBy(args -> given(invoice)
        .expectThat(it -> BigDecimal.valueOf(100.0d).compareTo(it.getTotal()) > 0)
        .throwOnFailedExpectations()
        .thenGiven(it -> it.findBy("ProductOne"))
        .expectThat(lineItemOne -> Integers.TWO.equals(lineItemOne.getQuantity()))
        .expectThat(lineItemOne -> BigDecimal.valueOf(20.0d).equals(lineItemOne.getCost()))
        .throwOnFailedExpectations()
        .thenGiven(LineItem::getProduct)
        .expectThat(product -> BigDecimal.valueOf(10.0d).equals(product.getPrice()))
        .throwOnFailedExpectations())
      .havingMessage("Target [ProductOne(20.0)|2] has failed expectation(s)")
      .withNoCause();
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
    assertFalse(is(Character.class).assignableTo(String.class));
    assertFalse(is(Double.class).assignableTo(BigDecimal.class));
    assertFalse(is(Float.class).assignableTo(Double.class));
    assertFalse(is(Integer.class).assignableTo(BigInteger.class));
    assertFalse(is(Integer.class).assignableTo(Long.class));
    assertFalse(is(String.class).assignableTo(BigDecimal.class));
    assertFalse(is(String.class).assignableTo(Character.class));
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

    Person jonDoe1 = new Person(1L, "Jon", "Doe");
    Person jonDoe2 = new Person(2L, "Jon", "Doe");

    assertTrue(is(jonDoe1).comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).not().comparableTo(jonDoe2));
    assertFalse(is(jonDoe1).equalTo(jonDoe2));
  }

  @Test
  public void isNotComparableTo() {

    Person johnBlum = new Person(1L, "John", "Blum");
    Person jonBloom = new Person(1L, "Jon", "Bloom");

    assertFalse(is(johnBlum).comparableTo(jonBloom));
    assertTrue(is(johnBlum).not().comparableTo(jonBloom));
    assertTrue(is(johnBlum).notComparableTo(jonBloom));
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
  @SuppressWarnings("rawtypes")
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
  @SuppressWarnings("rawtypes")
  public void isNotEqualTo() {

    assertTrue(is(NULL).not().equalTo(NULL));
    assertTrue(is((String) null).not().equalTo("null"));
    assertTrue(is("null").not().equalTo(null));
    assertTrue(is("null").not().equalTo("nil"));
    assertTrue(is(Boolean.FALSE).not().equalTo(Boolean.TRUE));
    assertTrue(is((Comparable) 'c').not().equalTo("c"));
    assertTrue(is(-2).not().equalTo(2));
    assertTrue(is(3.14159d).not().equalTo(Math.PI));
    assertTrue(is("test").not().equalTo("TEST"));
    assertTrue(is(TestUtils.createCalendar(2011, Calendar.OCTOBER, 13))
      .not().equalTo(Calendar.getInstance()));
  }

  @Test
  @SuppressWarnings("all")
  public void isFalse() {

    assertTrue(is(false).False());
    assertTrue(is(Boolean.FALSE).False());
    assertTrue(is(!Boolean.TRUE).False());
  }

  @Test
  @SuppressWarnings("all")
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
  @SuppressWarnings("all")
  public void isTrue() {

    assertTrue(is(true).True());
    assertTrue(is(Boolean.TRUE).True());
    assertTrue(is(!Boolean.FALSE).True());
  }

  @Test
  @SuppressWarnings("all")
  public void isNotTrue() {

    assertTrue(is(false).not().True());
    assertTrue(is(Boolean.FALSE).not().True());
    assertTrue(is(!Boolean.TRUE).not().True());
    assertTrue(is((Boolean) null).not().True());
  }

  @Test
  public void isValid() {
    assertTrue(is("test").valid(argument -> true));
  }

  @Test
  public void isInvalid() {
    assertFalse(is("test").valid(argument -> false));
  }

  @Test
  public void isNotValid() {
    assertTrue(is("test").not().valid(argument -> false));
  }

  @Test(expected = IllegalArgumentException.class)
  public void isValidWithNullPredicateThrowsIllegalArgumentException() {

    try {
      is("test").valid(null);
    }
    catch (IllegalArgumentException expected) {

      Assertions.assertThat(expected).hasMessage("Predicate is required");
      Assertions.assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void isNotEmptyWithNull() {
    assertFalse(is(null).notEmpty());
  }

  @Test
  public void isNotNotEmptyWithNull() {
    assertTrue(is(null).not().notEmpty());
  }

  @Test
  public void isNotEmptyArrayWithNonEmptyArray() {

    Object[] array = { "one", "two" };

    assertTrue(is(array).notEmpty());
    assertTrue(is(new Object[] { "test" }).notEmpty());
    assertTrue(is(new Object[1]).notEmpty());
  }

  @Test
  public void isNotEmptyArrayWithEmptyArray() {
    assertFalse(is(new Object[0]).notEmpty());
  }

  @Test
  public void isNotNotEmptyArrayWithEmptyArray() {
    assertTrue(is(new Object[0]).not().notEmpty());
  }

  @Test
  public void isNotEmptyCollectionWithNonEmptyCollection() {

    List<?> list = Arrays.asList("one", "two");

    assertTrue(is(list).notEmpty());
    assertTrue(is(Collections.singleton("test")).notEmpty());
    assertTrue(is(Collections.singletonList("mock")).notEmpty());
  }

  @Test
  public void isNotEmptyCollectionWithEmptyCollection() {

    assertFalse(is(Collections.emptyList()).notEmpty());
    assertFalse(is(Collections.emptySet()).notEmpty());
  }

  @Test
  public void isNotNotEmptyCollectionWithEmptyCollection() {

    assertTrue(is(Collections.emptyList()).not().notEmpty());
    assertTrue(is(Collections.emptySet()).not().notEmpty());
  }

  @Test
  public void isNotEmptyIterableWithNonEmptyIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);
    Iterator<?> mockIterator = mock(Iterator.class);

    doReturn(mockIterator).when(mockIterable).iterator();
    doReturn(true).when(mockIterator).hasNext();

    assertTrue(is(mockIterable).notEmpty());

    verify(mockIterable, times(1)).iterator();
    verify(mockIterator, times(1)).hasNext();
    verifyNoMoreInteractions(mockIterable, mockIterator);
  }

  @Test
  public void isNotEmptyIterableWithEmptyIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);
    Iterator<?> mockIterator = mock(Iterator.class);

    doReturn(mockIterator).when(mockIterable).iterator();
    doReturn(false).when(mockIterator).hasNext();

    assertFalse(is(mockIterable).notEmpty());

    verify(mockIterable, times(1)).iterator();
    verify(mockIterator, times(1)).hasNext();
    verifyNoMoreInteractions(mockIterable, mockIterator);
  }

  @Test
  public void isNotEmptyIterableWithNullIterator() {

    Iterable<?> mockIterable = mock(Iterable.class);

    doReturn(null).when(mockIterable).iterator();

    assertFalse(is(mockIterable).notEmpty());

    verify(mockIterable, times(1)).iterator();
    verifyNoMoreInteractions(mockIterable);
  }

  @Test
  public void isNotNotEmptyIterableWithEmptyIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);
    Iterator<?> mockIterator = mock(Iterator.class);

    doReturn(mockIterator).when(mockIterable).iterator();
    doReturn(false).when(mockIterator).hasNext();

    assertTrue(is(mockIterable).not().notEmpty());

    verify(mockIterable, times(1)).iterator();
    verify(mockIterator, times(1)).hasNext();
    verifyNoMoreInteractions(mockIterable, mockIterator);
  }

  @Test
  public void isNotEmptyMapWithNonEmptyMap() {
    assertTrue(is(Collections.singletonMap(1, "one")).notEmpty());
  }

  @Test
  public void isNotEmptyMapWithEmptyMap() {
    assertFalse(is(Collections.emptyMap()).notEmpty());
  }

  @Test
  public void isNotNotEmptyMapWithEmptyMap() {
    assertTrue(is(Collections.emptyMap()).not().notEmpty());
  }

  @Test
  public void isNotEmptyStringWithNonEmptyStrings() {

    assertTrue(is("test").notEmpty());
    assertTrue(is("empty").notEmpty());
    assertTrue(is("blank").notEmpty());
    assertTrue(is("null").notEmpty());
    assertTrue(is("nil").notEmpty());
    assertTrue(is("_").notEmpty());
    assertTrue(is(" ").notEmpty());
    assertTrue(is("  ").notEmpty());
    assertTrue(is("\0").notEmpty());
    assertTrue(is("\t").notEmpty());
    assertTrue(is("\n").notEmpty());
  }

  @Test
  public void isNotEmptyWithEmptyStrings() {
    assertFalse(is("").notEmpty());
  }

  @Test
  public void isNotNotEmptyStringWithEmptyString() {
    assertTrue(is("").not().notEmpty());
  }

  @Test
  public void isReuse() {

    Is<String> isOperator = is("test");

    Assertions.assertThat(isOperator).isNotNull();
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

  @Test
  @SuppressWarnings("all")
  public void safeNavigationWithNonNullObjectChain() {

    Invoice invoice = TestInvoice.of(asList(
      TestLineItem.newLineItem(TestProduct.newProduct("coffee", BigDecimal.valueOf(4.50d)), 2),
      TestLineItem.newLineItem(TestProduct.newProduct("donut", BigDecimal.valueOf(3.00d)), 4))
    );

    BigDecimal coffeePrice = $(invoice).findBy("coffee").getProduct().getPrice();

    Assertions.assertThat(coffeePrice).isEqualTo(BigDecimal.valueOf(4.50d));
  }

  @Test
  public void safeNavigationWithNullBreakInObjectChain() {

    Invoice invoice = TestInvoice.of(asList(
      TestLineItem.newLineItem(TestProduct.newProduct("coffee", BigDecimal.valueOf(4.50d)), 2),
      TestLineItem.newLineItem(TestProduct.newProduct("donut", BigDecimal.valueOf(3.00d)), 4))
    );

    Assertions.assertThat($(invoice).findBy("nonExistingProduct").getProduct().getPrice()).isNull();
  }

  @Test
  public void safeNavigationWithNullObjectChain() {
    assertThat($((Invoice) null, Invoice.class).findBy("nonExistingProduct").getProduct().getPrice())
      .isNull();
  }

  @Getter
  @RequiredArgsConstructor(staticName = "newPerson")
  private static class Person implements Comparable<Person> {

    @lombok.NonNull
    private final Long id;

    @lombok.NonNull
    private final String firstName;

    @lombok.NonNull
    private final String lastName;

    public String getName() {
      return String.format("%1$s %2$s", getFirstName(), getLastName());
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(@NotNull Person person) {

      int compareValue = ComparatorUtils.compareIgnoreNull(this.getFirstName(), person.getFirstName());

      return compareValue != 0 ? compareValue
        : ComparatorUtils.compareIgnoreNull(this.getLastName(), person.getLastName());
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof Person)) {
        return false;
      }

      Person that = (Person) obj;

      return ObjectUtils.equalsIgnoreNull(this.getId(), that.getId());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getId());
    }

    @Override
    public String toString() {
      return String.format("{ id = %1$s, firstName = %2$s, lastName = %3$s }", getId(), getFirstName(), getLastName());
    }
  }

  @SuppressWarnings("unused")
  public interface Invoice {

    LineItem findBy(int index);

    LineItem findBy(String productName);

    BigDecimal getTotal();

    BigDecimal getTotal(String... productNames);

  }

  @Getter
  @RequiredArgsConstructor(staticName = "of")
  public static class TestInvoice implements Invoice {

    @lombok.NonNull
    private List<TestLineItem> lineItems;

    public LineItem findBy(int index) {
      return nullSafeList(getLineItems()).get(index);
    }

    public LineItem findBy(String productName) {

      return nullSafeList(getLineItems()).stream()
        .filter((lineItem) -> lineItem.getProduct().getName().equals(productName))
        .findFirst().orElse(null);
    }

    public BigDecimal getTotal() {

      return nullSafeList(getLineItems()).stream()
        .map(TestLineItem::getCost)
        .reduce(BigDecimal::add)
        .orElse(BigDecimal.ZERO);
    }

    public BigDecimal getTotal(String... productNames) {

      return (nullSafeList(getLineItems())).stream()
        .filter(lineItem -> asList(nullSafeArray(productNames)).contains(lineItem.getProduct().getName()))
        .map(TestLineItem::getCost)
        .reduce(BigDecimal::add)
        .orElse(BigDecimal.ZERO);
    }
  }

  @SuppressWarnings("unused")
  public interface LineItem {

    Product getProduct();

    Integer getQuantity();

    BigDecimal getCost();

  }

  @Getter
  @RequiredArgsConstructor(staticName = "newLineItem")
  public static class TestLineItem implements LineItem {

    @lombok.NonNull
    private final Product product;

    @lombok.NonNull
    private final Integer quantity;

    public BigDecimal getCost() {
      return getProduct().getCost(getQuantity());
    }

    @Override
    public String toString() {
      return String.format("%s|%d", getProduct(), getQuantity());
    }
  }

  public interface Product {

    String getName();

    BigDecimal getPrice();

    BigDecimal getCost(int quantity);

  }

  @Getter
  @RequiredArgsConstructor(staticName = "newProduct")
  public static class TestProduct implements Product {

    @lombok.NonNull
    private final String name;

    @lombok.NonNull
    private final BigDecimal price;

    public BigDecimal getCost(int quantity) {
      return getPrice().multiply(BigDecimal.valueOf(quantity));
    }

    @Override
    public String toString() {
      return String.format("%s(%s)", getName(), getPrice());
    }
  }

  @Getter
  @EqualsAndHashCode
  @RequiredArgsConstructor(staticName = "as")
  @SuppressWarnings("unused")
  public static class User implements Comparable<User> {

    @lombok.NonNull
    private final String name;

    @Override
    public int compareTo(@NotNull User other) {
      return getName().compareTo(other.getName());
    }

    @Override
    public String toString() {
      return getName();
    }
  }
}
