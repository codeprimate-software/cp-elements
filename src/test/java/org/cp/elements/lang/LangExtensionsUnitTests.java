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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;
import static org.cp.elements.util.CollectionUtils.nullSafeList;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
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

import org.junit.jupiter.api.Test;

import org.assertj.core.api.Assertions;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.lang.LangExtensions.Given;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.test.TestUtils;
import org.cp.elements.util.ComparatorUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

/**
 * Unit Tests for {@link LangExtensions}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.LangExtensions
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
  public void assertThatAsConvertedTypedAssertion() {

    Person jonDoe = new Person(1L, "Jon", "Doe");

    Function<Person, User> toUserFunction = person ->
      User.as(person.getFirstName().toLowerCase() + person.getLastName());

    assertThat(jonDoe).asType(toUserFunction).isEqualTo(User.as("jonDoe"));
  }

  @Test
  public void assertThatAsStringAssertion() {

    User jonDoe = User.as("jonDoe");

    assertThat(jonDoe).isNotNull();
    assertThat(jonDoe).asString().isEqualTo("jonDoe");
  }

  @Test
  void assertThatAsStringConvertedToUserTypedAssertion() {
    assertThat("jonDoe").asType(User::as).isEqualTo(User.as("jonDoe"));
  }

  @Test
  public void assertThatAsNullConverterFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("mock").asType(null).isEqualTo("test"))
      .withMessage("Function used to convert the target (subject) to the specified type is required")
      .withNoCause();
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

  @Test
  public void assertThatObjectIsAssignableToStringThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(Object.class)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isAssignableTo(String.class))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatDoubleIsAssignableToIntegerThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(Math.PI).isAssignableTo(Integer.class))
      .withMessage("[%s] is not assignable to [java.lang.Integer]", Math.PI)
      .withNoCause();
  }

  @Test
  public void assertThatCharacterIsAssignableToStringThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(Character.class)
        .throwing(newIllegalArgumentException("test"))
        .isAssignableTo(String.class))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatStringIsNotAssignableToObjectThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(String.class).not().isAssignableTo(Object.class))
      .withMessage("[class java.lang.String] is assignable to [java.lang.Object]")
      .withNoCause();
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

  @Test
  public void assertThatObjectsAreNotComparableThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("queue").isComparableTo("Q"))
      .withMessage("[queue] is not comparable to [Q]")
      .withNoCause();
  }

  @Test
  public void assertThatObjectsAreNotComparableThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("c")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isComparableTo("see"))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatObjectsAreNotComparableThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("c")
        .throwing(newIllegalArgumentException("test"))
        .isComparableTo("see"))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatComparableObjectsAreNotComparableThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").not().isComparableTo("test"))
      .withMessage("[test] is comparable to [test]")
      .withNoCause();
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

  @Test
  public void assertThatObjectIsEqualToObjectThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").isEqualTo("mock"))
      .withMessage("[test] is not equal to [mock]")
      .withNoCause();
  }

  @Test
  public void assertThatObjectIsEqualToObjectThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("r")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isEqualTo("are"))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatObjectIsEqualToObjectThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("r")
        .throwing(newIllegalArgumentException("test"))
        .isEqualTo("are"))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatObjectIsNotEqualToObjectThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").isNotEqualTo("test"))
      .withMessage("[test] is equal to [test]")
      .withNoCause();
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

  @Test
  public void assertThatTrueIsFalseThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(true).isFalse())
      .withMessage("[true] is not false")
      .withNoCause();
  }

  @Test
  public void assertThatTrueIsFalseThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(true)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isFalse())
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatTrueIsFalseThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(true)
        .throwing(newIllegalArgumentException("test"))
        .isFalse())
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatFalseIsNotFalseThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(false).not().isFalse())
      .withMessage("[false] is false")
      .withNoCause();
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
  public void assertThatIsGreaterThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isGreaterThan(2))
      .withMessage("[2] is not greater than [2]")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThan(3))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isGreaterThan(3))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotGreaterThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isGreaterThan(1))
      .withMessage("[2] is greater than [1]")
      .withNoCause();
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
  public void assertThatIsGreaterThanAndLessThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isGreaterThanAndLessThan(2, 2))
      .withMessage("[2] is not greater than [2] and less than [2]")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThanAndLessThan(2, 2))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isGreaterThanAndLessThan(2, 2))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotGreaterThanAndLessThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isGreaterThanAndLessThan(1, 3))
      .withMessage("[2] is greater than [1] and less than [3]")
      .withNoCause();
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
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isGreaterThanAndLessThanEqualTo(2, 2))
      .withMessage("[2] is not greater than [2] and less than equal to [2]")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThanAndLessThanEqualTo(2, 2))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanAndLessThanEqualToThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isGreaterThanAndLessThanEqualTo(2, 2))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotGreaterThanAndLessThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isGreaterThanAndLessThanEqualTo(1, 2))
      .withMessage("[2] is greater than [1] and less than equal to [2]")
      .withNoCause();
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
  public void assertThatIsGreaterThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isGreaterThanEqualTo(3))
      .withMessage("[2] is not greater than equal to [3]")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThanEqualTo(3))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanEqualToThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isGreaterThanEqualTo(3))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isGreaterThanEqualTo(1))
      .withMessage("[2] is greater than equal to [1]")
      .withNoCause();
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
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isGreaterThanEqualToAndLessThan(3, 1))
      .withMessage("[2] is not greater than equal to [3] and less than [1]")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1s{1}", "test", "!")
        .isGreaterThanEqualToAndLessThan(3, 1))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isGreaterThanEqualToAndLessThan(3, 1))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToAndLessThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isGreaterThanEqualToAndLessThan(1, 3))
      .withMessage("[2] is greater than equal to [1] and less than [3]")
      .withNoCause();
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
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isGreaterThanEqualToAndLessThanEqualTo(3, 1))
      .withMessage("[2] is not greater than equal to [3] and less than equal to [1]")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isGreaterThanEqualToAndLessThanEqualTo(3, 1))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsGreaterThanEqualToAndLessThanEqualToThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isGreaterThanEqualToAndLessThanEqualTo(3, 1))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotGreaterThanEqualToAndLessThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isGreaterThanEqualToAndLessThanEqualTo(1, 3))
      .withMessage("[2] is greater than equal to [1] and less than equal to [3]")
      .withNoCause();
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
    assertThat(" ").not().hasText();
    assertThat("  ").not().hasText();
    assertThat("\t").not().hasText();
    assertThat("\n").not().hasText();
  }

  @Test
  public void assertThatBlankStringHasTextThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(" ").hasText())
      .withMessage("[ ] is blank")
      .withNoCause();
  }

  @Test
  public void assertThatEmptyStringHasTextThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isNotBlank())
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatNullStringHasTextThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(null)
        .throwing(newIllegalArgumentException("test"))
        .hasText())
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatStringDoesNotHaveTextThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").not().hasText())
      .withMessage("[test] is not blank")
      .withNoCause();
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
  public void assertThatCurrentThreadHoldsLockThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(Thread.currentThread()).holdsLock(lock))
      .withMessage("[%1$s] does not hold lock [%2$s]", Thread.currentThread(), lock)
      .withNoCause();
  }

  @Test
  public void assertThatCurrentThreadHoldsLockThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(Thread.currentThread())
        .describedAs("This is a %1$s{1}", "test", "!")
        .holdsLock(lock))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatCurrentThreadHoldsLockThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(Thread.currentThread())
        .throwing(newIllegalArgumentException("test"))
        .holdsLock(lock))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatCurrentThreadDoesNotHoldLockThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> {
        synchronized (lock) {
          assertThat(Thread.currentThread()).not().holdsLock(lock);
        }
      })
      .withMessage("[%1$s] holds lock [%2$s]", Thread.currentThread(), lock)
      .withNoCause();
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

  @Test
  public void assertThatStringIsInstanceOfCharacterThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").isInstanceOf(Character.class))
      .withMessage("[test] is not an instance of [java.lang.Character]")
      .withNoCause();
  }

  @Test
  public void assertThatStringIsInstanceOfCharacterThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isInstanceOf(Character.class))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatStringIsInstanceOfCharacterThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("test")
        .throwing(newIllegalArgumentException("test"))
        .isInstanceOf(Character.class))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatStringIsNotInstanceOfStringThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").not().isInstanceOf(String.class))
      .withMessage("[test] is an instance of [java.lang.String]")
      .withNoCause();
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
  public void assertThatIsLessThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isLessThan(1))
      .withMessage("[2] is not less than [1]")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThan(1))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isLessThan(1))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotLessThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(1).not().isLessThan(2))
      .withMessage("[1] is less than [2]")
      .withNoCause();
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
  public void assertThatIsLessThanOrGreaterThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0).isLessThanOrGreaterThan(-1, 1))
      .withMessage("[0] is not less than [-1] or greater than [1]")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanOrGreaterThan(-1, 1))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(0)
        .throwing(newIllegalArgumentException("test"))
        .isLessThanOrGreaterThan(-1, 1))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotLessThanOrGreaterThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(3).not().isLessThanOrGreaterThan(-1, 1))
      .withMessage("[3] is less than [-1] or greater than [1]")
      .withNoCause();
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
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0).isLessThanOrGreaterThanEqualTo(-1, 1))
      .withMessage("[0] is not less than [-1] or greater than equal to [1]")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanOrGreaterThanEqualTo(-1, 1))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanOrGreaterThanEqualToThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(0)
        .throwing(newIllegalArgumentException("test"))
        .isLessThanOrGreaterThanEqualTo(-1, 1))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotLessThanOrGreaterThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(1).not().isLessThanOrGreaterThanEqualTo(-1, 1))
      .withMessage("[1] is less than [-1] or greater than equal to [1]")
      .withNoCause();
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
  public void assertThatIsLessThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).isLessThanEqualTo(-3))
      .withMessage("[2] is not less than equal to [-3]")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanEqualTo(-3))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanEqualToThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(2)
        .throwing(newIllegalArgumentException("test"))
        .isLessThanEqualTo(-3))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotLessThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isLessThanEqualTo(3))
      .withMessage("[2] is less than equal to [3]")
      .withNoCause();
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
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0).isLessThanEqualToOrGreaterThan(-1, 1))
      .withMessage("[0] is not less than equal to [-1] or greater than [1]")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanEqualToOrGreaterThan(-1, 1))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(0)
        .throwing(newIllegalArgumentException("test"))
        .isLessThanEqualToOrGreaterThan(-1, 1))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotLessThanEqualToOrGreaterThanThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isLessThanEqualToOrGreaterThan(-1, 1))
      .withMessage("[2] is less than equal to [-1] or greater than [1]")
      .withNoCause();
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
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0).isLessThanEqualToOrGreaterThanEqualTo(-1, 1))
      .withMessage("[0] is not less than equal to [-1] or greater than equal to [1]")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(0)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isLessThanEqualToOrGreaterThanEqualTo(-1, 1))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatIsLessThanEqualToOrGreaterThanEqualToThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(0)
        .throwing(newIllegalArgumentException("test"))
        .isLessThanEqualToOrGreaterThanEqualTo(-1, 1))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIsNotLessThanEqualToOrGreaterThanEqualToThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(2).not().isLessThanEqualToOrGreaterThanEqualTo(-1, 1))
      .withMessage("[2] is less than equal to [-1] or greater than equal to [1]")
      .withNoCause();
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
  public void assertThatNonNullObjectIsNullThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").isNull())
      .withMessage("[test] is not null")
      .withNoCause();
  }

  @Test
  public void assertThatNonNullObjectIsNullThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isNull())
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatNonNullObjectIsNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("test")
        .throwing(newIllegalArgumentException("test"))
        .isNull())
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatNullObjectIsNotNullThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(null).isNotNull())
      .withMessage("[null] is null")
      .withNoCause();
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

  @Test
  public void assertThatEmptyStringIsNotEmptyThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("").isNotEmpty())
      .withMessage("[] is empty")
      .withNoCause();
  }

  @Test
  public void assertThatEmptyStringIsNotEmptyThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isNotEmpty())
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatEmptyStringIsNotEmptyThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("")
        .throwing(newIllegalArgumentException("test"))
        .isNotEmpty())
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatNonEmptyStringIsEmptyThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(" ").not().isNotEmpty())
      .withMessage("[ ] is not empty")
      .withNoCause();
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

  @Test
  public void assertThatBlankStringThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("  ").isNotBlank())
      .withMessage("[  ] is blank")
      .withNoCause();
  }

  @Test
  public void assertThatBlankStringThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("  ")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isNotBlank())
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatBlankStringThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("  ")
        .throwing(newIllegalArgumentException("test"))
        .isNotBlank())
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatNonBlankStringIsBlankThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("nonBlank").not().isNotBlank())
      .withMessage("[nonBlank] is not blank")
      .withNoCause();
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

  @Test
  public void assertThatNonIdenticalObjectsAreTheSameThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").isSameAs("TEST"))
      .withMessage("[test] is not the same as [TEST]")
      .withNoCause();
  }

  @Test
  public void assertThatNonIdenticalObjectsAreTheSameThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test")
        .describedAs("This is a %1$s{1}", "test", "!")
        .isSameAs("TEST"))
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatNonIdenticalObjectsAreTheSameThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("test")
        .throwing(new IllegalArgumentException("test"))
        .isSameAs("TEST"))
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatIdenticalObjectsAreNotTheSameThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").isNotSameAs("test"))
      .withMessage("[test] is the same as [test]")
      .withNoCause();
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

  @Test
  public void assertThatFalseIsTrueThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(false).isTrue())
      .withMessage("[false] is not true")
      .withNoCause();
  }

  @Test
  public void assertThatFalseIsTrueThrowsAssertionExceptionWithCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(false)
        .describedAs("This is a %1$s{1}", "test", "!")
        .isTrue())
      .withMessage("This is a test!")
      .withNoCause();
  }

  @Test
  public void assertThatFalseIsTrueThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(false)
        .throwing(newIllegalArgumentException("test"))
        .isTrue())
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void assertThatTrueIsNotTrueThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(true).not().isTrue())
      .withMessage("[true] is true")
      .withNoCause();
  }

  @Test
  public void assertThatIsValidIsValid() {
    assertThat("test").isValid(argument -> true);
  }

  @Test
  public void assertThatIsValidIsNotValid() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").isValid(argument -> false))
      .withMessage("[test] is not valid")
      .withNoCause();
  }

  @Test
  public void assertThatIsValidWithNullPredicate() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("test").isValid(null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  public void assertThatIsValidUsingCustomMessage() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test")
        .describedAs("[%s] is invalid", "test")
        .isValid(argument -> false))
      .withMessage("[test] is invalid")
      .withNoCause();
  }

  @Test
  public void assertThatIsValidThrowingIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("test")
        .throwing(newIllegalArgumentException("[test] is invalid"))
        .isValid(argument -> false))
      .withMessage("[test] is invalid")
      .withNoCause();
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

  @Test
  public void assertThatIsNotValidWithValidValueThrowsAssertionException() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat("test").not().isValid(argument -> true))
      .withMessage("[test] is valid")
      .withNoCause();
  }

  @Test
  public void assertionTransformationIgnoresCondition() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(false)
        .transform(new Transformer<>() {

          @Override
          public AssertThat<Boolean> transform(AssertThat<Boolean> assertion) {

            return new AssertThatWrapper<>(assertion) {

              @Override
              public AssertThat<Boolean> when(final Condition condition) {
                assertion.when(Condition.TRUE_CONDITION);
                return this;
              }
            };
          }
        })
        .when(ENABLE_DISABLE_CONDITION).isTrue())
      .withMessage("[false] is not true")
      .withNoCause();
  }

  @Test
  public void assertionDescribeAsUsingStringAndArgumentsIsCorrect() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(true)
        .describedAs("This is a %s {1}!", "boolean", "test")
        .isFalse())
      .withMessage("This is a boolean test!")
      .withNoCause();
  }

  @Test
  public void assertionDescribeAsUsingSupplierIsCorrect() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(true)
        .describedAs(() -> "test")
        .isFalse())
      .withMessage("test")
      .withNoCause();
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

  @Test
  public void negatedAssertThatRetainsThrowable() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat(true).throwing(newIllegalArgumentException("test")).not().isTrue())
      .withMessage("test")
      .withNoCause();
  }

  @Test
  public void negatedAssertThatRetainsTransformer() {

    AtomicInteger holdsLockCallCount = new AtomicInteger(0);

    Object lock = new Object();

    Transformer<AssertThat<Thread>> assertThatTransformer = (AssertThat<Thread> assertion) ->

      new AssertThatWrapper<>(assertion) {

        @Override
        public AssertThat<Thread> holdsLock(final Object lock) {
          holdsLockCallCount.incrementAndGet();
          assertion.holdsLock(lock);
          return this;
        }
      };

    assertThat(Thread.currentThread()).transform(assertThatTransformer).not().holdsLock(lock);

    Assertions.assertThat(holdsLockCallCount.get()).isOne();
  }

  @Test
  public void negatedAssertThatRetainsExceptionMessageAndArgs() {

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat(null)
        .describedAs("%1$s cannot be {1}", "Object", "null")
        .isNotNull())
      .withMessage("Object cannot be null")
      .withNoCause();
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
  void wrappedAssertThatMapDelegatesToWrappedAssertion() {

    Function mockFunction = mock(Function.class);

    AssertThat<?> newAssertion = mock(AssertThat.class);
    AssertThat<?> mockAssertion = mock(AssertThat.class);

    doReturn(newAssertion).when(mockAssertion).map(any(Function.class));

    AssertThat<?> result = AssertThatWrapper.wrap(mockAssertion).map(mockFunction);

    Assertions.assertThat(result).isSameAs(newAssertion);

    verify(mockAssertion, times(1)).map(eq(mockFunction));
    verifyNoMoreInteractions(mockAssertion);
    verifyNoInteractions(mockFunction, newAssertion);
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatNotDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);

    when(mockAssertion.not()).thenReturn(mockAssertion);

    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).not();

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    Assertions.assertThat(wrappedAssertion).isInstanceOf(AssertThatWrapper.class);

    verify(mockAssertion, times(1)).not();
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatThrowingDelegatesToWrappedAssertion() {

    RuntimeException illegalArgument = new IllegalArgumentException("test");

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).throwing(illegalArgument);

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    Assertions.assertThat(wrappedAssertion).isInstanceOf(AssertThatWrapper.class);

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
    Assertions.assertThat(wrappedAssertion).isInstanceOf(AssertThatWrapper.class);

    verify(mockTransformer, times(1)).transform(eq(mockAssertion));
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatUsingDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).describedAs("message", "args");

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    Assertions.assertThat(wrappedAssertion).isInstanceOf(AssertThatWrapper.class);

    verify(mockAssertion, times(1)).stating(eq("message"), eq("args"));
  }

  @Test
  @SuppressWarnings({ "rawtypes", "unchecked" })
  public void wrappedAssertThatWhenDelegatesToWrappedAssertion() {

    AssertThat mockAssertion = mock(AssertThat.class);
    AssertThat wrappedAssertion = AssertThatWrapper.wrap(mockAssertion).when(ENABLE_DISABLE_CONDITION);

    Assertions.assertThat(wrappedAssertion).isNotSameAs(mockAssertion);
    Assertions.assertThat(wrappedAssertion).isInstanceOf(AssertThatWrapper.class);

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

  @Test
  public void assertThatWithCompoundActionsShortCircuitsAndThrowsAssertionException() {

    AssertThat<Integer> assertThat = spy(assertThat(2));

    Assertions.assertThat(assertThat).isNotNull();
    Assertions.assertThat(ObjectUtils.invoke(assertThat, "getTarget", Integer.class)).isEqualTo(2);

    assertThatExceptionOfType(AssertionException.class)
      .isThrownBy(() -> assertThat
        .isInstanceOf(Integer.class)
        .isGreaterThanAndLessThan(1, 3)
        .isValid(argument -> false)
        .isLessThanOrGreaterThan(3, 1))
      .withMessage("[2] is not valid")
      .withNoCause();

    verify(assertThat, times(1)).isInstanceOf(eq(Integer.class));
    verify(assertThat, times(1)).isGreaterThanAndLessThan(eq(1), eq(3));
    verify(assertThat, times(1)).isValid(any());
    verify(assertThat, never()).isLessThanOrGreaterThan(eq(3), eq(1));
    verifyNoMoreInteractions(assertThat);
  }

  @Test
  void assertThatWithMapping() {

    Product iphone = TestProduct.newProduct("Apple iPhone", BigDecimal.valueOf(1199.0d));
    LineItem lineItem = TestLineItem.newLineItem(iphone, 2);

    assertThat(lineItem).isNotNull()
      .map(LineItem::getProduct).isNotNull()
      .map(Product::getName).isEqualTo("Apple iPhone");
  }

  @Test
  void assertThatWithMappingUsingNullFunctionThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> assertThat("test").map(null))
      .withMessage("Function is required")
      .withNoCause();
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

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
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
  public void fromObjectConvertToNullType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> from("MockUser").convertTo(null))
      .withMessage("No SimpleTypeConversion exists for target type [null]")
      .withNoCause();
  }

  @Test
  public void fromObjectConvertToIncompatibleType() {

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> from("test").convertTo(Integer.class))
      .havingMessage("Cannot convert [test] into an Integer")
      .causedBy(NumberFormatException.class)
      .havingMessageContaining("test")
      .withNoCause();
  }

  @Test
  public void fromStringConvertToUser() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> from("TestUser").convertTo(User.class))
      .withMessage("No SimpleTypeConversion exists for target type [%s]", User.class.getName())
      .withNoCause();
  }

  @Test
  public void fromUserConvertToString() {
    assertThat(from(User.as("MockUser")).convertTo(String.class)).isEqualTo("MockUser");
  }

  @Test
  void fromObjectMapToUsingNullMappingFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> from("test").mapTo(null))
      .withMessage("Function used to perform the mapping is required")
      .withNoCause();
  }

  @Test
  void fromStringMappedToUser() {
    assertThat(from("jonDoe").mapTo(User::as)).isEqualTo(User.as("jonDoe"));
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

    assertThatExceptionOfType(ExpectationException.class)
      .isThrownBy(() -> assertThat(given(null).throwOnFailedExpectations().result()).isTrue())
      .withMessage("Target [null] has failed expectation(s)")
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

    assertThatExceptionOfType(ExpectationException.class)
      .isThrownBy(() -> given(invoice)
        .expectThat(it -> BigDecimal.valueOf(100.0d).compareTo(it.getTotal()) > 0)
        .throwOnFailedExpectations()
        .thenGiven(it -> it.findBy("ProductOne"))
        .expectThat(lineItemOne -> Integers.TWO.equals(lineItemOne.getQuantity()))
        .expectThat(lineItemOne -> BigDecimal.valueOf(20.0d).equals(lineItemOne.getCost()))
        .throwOnFailedExpectations()
        .thenGiven(LineItem::getProduct)
        .expectThat(product -> BigDecimal.valueOf(10.0d).equals(product.getPrice()))
        .throwOnFailedExpectations())
      .withMessage("Target [ProductOne(20.0)|2] has failed expectation(s)")
      .withNoCause();
  }

  @Test
  public void isAssignableTo() {

    Assertions.assertThat(is(Object.class).assignableTo(Object.class)).isTrue();
    Assertions.assertThat(is(Boolean.class).assignableTo(Object.class)).isTrue();
    Assertions.assertThat(is(Character.class).assignableTo(Object.class)).isTrue();
    Assertions.assertThat(is(Number.class).assignableTo(Object.class)).isTrue();
    Assertions.assertThat(is(String.class).assignableTo(Object.class)).isTrue();
    Assertions.assertThat(is(Double.class).assignableTo(Number.class)).isTrue();
    Assertions.assertThat(is(Integer.class).assignableTo(Number.class)).isTrue();
    Assertions.assertThat(is(java.sql.Date.class).assignableTo(java.util.Date.class)).isTrue();
  }

  @Test
  public void isAssignableToIsFalse() {

    Assertions.assertThat(is(Object.class).assignableTo(String.class)).isFalse();
    Assertions.assertThat(is(Boolean.TYPE).assignableTo(Boolean.class)).isFalse();
    Assertions.assertThat(is(Character.class).assignableTo(String.class)).isFalse();
    Assertions.assertThat(is(Double.class).assignableTo(BigDecimal.class)).isFalse();
    Assertions.assertThat(is(Float.class).assignableTo(Double.class)).isFalse();
    Assertions.assertThat(is(Integer.class).assignableTo(BigInteger.class)).isFalse();
    Assertions.assertThat(is(Integer.class).assignableTo(Long.class)).isFalse();
    Assertions.assertThat(is(String.class).assignableTo(BigDecimal.class)).isFalse();
    Assertions.assertThat(is(String.class).assignableTo(Character.class)).isFalse();
  }

  @Test
  public void isNotAssignableTo() {

    Assertions.assertThat(is(Boolean.TYPE).not().assignableTo(Boolean.class)).isTrue();
    Assertions.assertThat(is(Character.class).not().assignableTo(String.class)).isTrue();
    Assertions.assertThat(is(Double.TYPE).not().assignableTo(Double.class)).isTrue();
    Assertions.assertThat(is(Integer.TYPE).not().assignableTo(Integer.class)).isTrue();
    Assertions.assertThat(is(Object.class).not().assignableTo(String.class)).isTrue();
  }

  @Test
  public void isComparableTo() {

    Person jonDoe1 = new Person(1L, "Jon", "Doe");
    Person jonDoe2 = new Person(2L, "Jon", "Doe");

    Assertions.assertThat(is(jonDoe1).comparableTo(jonDoe2)).isTrue();
    Assertions.assertThat(is(jonDoe1).not().comparableTo(jonDoe2)).isFalse();
    Assertions.assertThat(is(jonDoe1).equalTo(jonDoe2)).isFalse();
  }

  @Test
  public void isNotComparableTo() {

    Person johnBlum = new Person(1L, "John", "Blum");
    Person jonBloom = new Person(1L, "Jon", "Bloom");

    Assertions.assertThat(is(johnBlum).comparableTo(jonBloom)).isFalse();
    Assertions.assertThat(is(johnBlum).not().comparableTo(jonBloom)).isTrue();
    Assertions.assertThat(is(johnBlum).notComparableTo(jonBloom)).isTrue();
    Assertions.assertThat(is(johnBlum).equalTo(jonBloom)).isTrue();
  }

  @Test
  public void isEqualTo() {

    Assertions.assertThat(is(true).equalTo(Boolean.TRUE)).isTrue();
    Assertions.assertThat(is('c').equalTo('c')).isTrue();
    Assertions.assertThat(is(2).equalTo(2)).isTrue();
    Assertions.assertThat(is(Math.PI).equalTo(Math.PI)).isTrue();
    Assertions.assertThat(is("test").equalTo("test")).isTrue();
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void isEqualToWithUnequalValues() {

    Assertions.assertThat(is(NULL).equalTo(NULL)).isFalse();
    Assertions.assertThat(is("null").equalTo(null)).isFalse();
    Assertions.assertThat(is("null").equalTo("nil")).isFalse();
    Assertions.assertThat(is(Boolean.FALSE).equalTo(Boolean.TRUE)).isFalse();
    Assertions.assertThat(is((Comparable) 'c').equalTo("c")).isFalse();
    Assertions.assertThat(is(-2).equalTo(2)).isFalse();
    Assertions.assertThat(is(3.14159d).equalTo(Math.PI)).isFalse();
    Assertions.assertThat(is("test").equalTo("TEST")).isFalse();
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void isNotEqualTo() {

    Assertions.assertThat(is(NULL).not().equalTo(NULL)).isTrue();
    Assertions.assertThat(is((String) null).not().equalTo("null")).isTrue();
    Assertions.assertThat(is("null").not().equalTo(null)).isTrue();
    Assertions.assertThat(is("null").not().equalTo("nil")).isTrue();
    Assertions.assertThat(is(Boolean.FALSE).not().equalTo(Boolean.TRUE)).isTrue();
    Assertions.assertThat(is((Comparable) 'c').not().equalTo("c")).isTrue();
    Assertions.assertThat(is(-2).not().equalTo(2)).isTrue();
    Assertions.assertThat(is(3.14159d).not().equalTo(Math.PI)).isTrue();
    Assertions.assertThat(is("test").not().equalTo("TEST")).isTrue();
    Assertions.assertThat(is(TestUtils.createCalendar(2011, Calendar.OCTOBER, 13))
      .not().equalTo(Calendar.getInstance())).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isFalse() {

    Assertions.assertThat(is(false).False()).isTrue();
    Assertions.assertThat(is(Boolean.FALSE).False()).isTrue();
    Assertions.assertThat(is(!Boolean.TRUE).False()).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotFalse() {

    Assertions.assertThat(is(true).not().False()).isTrue();
    Assertions.assertThat(is(Boolean.TRUE).not().False()).isTrue();
    Assertions.assertThat(is(!Boolean.FALSE).not().False()).isTrue();
    Assertions.assertThat(is((Boolean) null).not().False()).isTrue();
  }

  @Test
  public void isGreaterThan() {

    Assertions.assertThat(is(3).greaterThan(1)).isTrue();
    Assertions.assertThat(is(3).greaterThan(2)).isTrue();
    Assertions.assertThat(is(3).greaterThan(3)).isFalse();
    Assertions.assertThat(is(3).greaterThan(4)).isFalse();
    Assertions.assertThat(is(3).greaterThan(5)).isFalse();
  }

  @Test
  public void isNotGreaterThan() {

    Assertions.assertThat(is(3).not().greaterThan(1)).isFalse();
    Assertions.assertThat(is(3).not().greaterThan(2)).isFalse();
    Assertions.assertThat(is(3).not().greaterThan(3)).isTrue();
    Assertions.assertThat(is(3).not().greaterThan(4)).isTrue();
    Assertions.assertThat(is(3).not().greaterThan(5)).isTrue();
  }

  @Test
  public void isGreaterThanAndLessThan() {

    Assertions.assertThat(is(1).greaterThanAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(2).greaterThanAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(3).greaterThanAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(4).greaterThanAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(5).greaterThanAndLessThan(2, 4)).isFalse();
  }

  @Test
  public void isNotGreaterThanAndLessThan() {

    Assertions.assertThat(is(1).not().greaterThanAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(2).not().greaterThanAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(3).not().greaterThanAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(4).not().greaterThanAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(5).not().greaterThanAndLessThan(2, 4)).isTrue();
  }

  @Test
  public void isGreaterThanAndLessThanEqualTo() {

    Assertions.assertThat(is(1).greaterThanAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(2).greaterThanAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(3).greaterThanAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(4).greaterThanAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(5).greaterThanAndLessThanEqualTo(2, 4)).isFalse();
  }

  @Test
  public void isNotGreaterThanAndLessThanEqualTo() {

    Assertions.assertThat(is(1).not().greaterThanAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(2).not().greaterThanAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(3).not().greaterThanAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(4).not().greaterThanAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(5).not().greaterThanAndLessThanEqualTo(2, 4)).isTrue();
  }

  @Test
  public void isGreaterThanEqualTo() {

    Assertions.assertThat(is(3).greaterThanEqualTo(1)).isTrue();
    Assertions.assertThat(is(3).greaterThanEqualTo(2)).isTrue();
    Assertions.assertThat(is(3).greaterThanEqualTo(3)).isTrue();
    Assertions.assertThat(is(3).greaterThanEqualTo(4)).isFalse();
    Assertions.assertThat(is(3).greaterThanEqualTo(5)).isFalse();
  }

  @Test
  public void isNotGreaterThanEqualTo() {

    Assertions.assertThat(is(3).not().greaterThanEqualTo(1)).isFalse();
    Assertions.assertThat(is(3).not().greaterThanEqualTo(2)).isFalse();
    Assertions.assertThat(is(3).not().greaterThanEqualTo(3)).isFalse();
    Assertions.assertThat(is(3).not().greaterThanEqualTo(4)).isTrue();
    Assertions.assertThat(is(3).not().greaterThanEqualTo(5)).isTrue();
  }

  @Test
  public void isGreaterThanEqualToAndLessThan() {

    Assertions.assertThat(is(1).greaterThanEqualToAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(2).greaterThanEqualToAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(3).greaterThanEqualToAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(4).greaterThanEqualToAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(5).greaterThanEqualToAndLessThan(2, 4)).isFalse();
  }

  @Test
  public void isNotGreaterThanEqualToAndLessThan() {

    Assertions.assertThat(is(1).not().greaterThanEqualToAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(2).not().greaterThanEqualToAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(3).not().greaterThanEqualToAndLessThan(2, 4)).isFalse();
    Assertions.assertThat(is(4).not().greaterThanEqualToAndLessThan(2, 4)).isTrue();
    Assertions.assertThat(is(5).not().greaterThanEqualToAndLessThan(2, 4)).isTrue();
  }

  @Test
  public void isGreaterThanEqualToAndLessThanEqualTo() {

    Assertions.assertThat(is(1).greaterThanEqualToAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(2).greaterThanEqualToAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(3).greaterThanEqualToAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(4).greaterThanEqualToAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(5).greaterThanEqualToAndLessThanEqualTo(2, 4)).isFalse();
  }

  @Test
  public void isNotGreaterThanEqualToAndLessThanEqualTo() {

    Assertions.assertThat(is(1).not().greaterThanEqualToAndLessThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(2).not().greaterThanEqualToAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(3).not().greaterThanEqualToAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(4).not().greaterThanEqualToAndLessThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(5).not().greaterThanEqualToAndLessThanEqualTo(2, 4)).isTrue();
  }

  @Test
  public void isInstanceOf() {

    Assertions.assertThat(is(false).instanceOf(Boolean.class)).isTrue();
    Assertions.assertThat(is(Boolean.TRUE).instanceOf(Boolean.class)).isTrue();
    Assertions.assertThat(is(Calendar.getInstance()).instanceOf(Calendar.class)).isTrue();
    Assertions.assertThat(is('c').instanceOf(Character.class)).isTrue();
    Assertions.assertThat(is('0').instanceOf(Character.class)).isTrue();
    Assertions.assertThat(is(2).instanceOf(Number.class)).isTrue();
    Assertions.assertThat(is(0).instanceOf(Integer.class)).isTrue();
    Assertions.assertThat(is(Math.PI).instanceOf(Number.class)).isTrue();
    Assertions.assertThat(is(0.0d).instanceOf(Double.class)).isTrue();
    Assertions.assertThat(is("null").instanceOf(String.class)).isTrue();
    Assertions.assertThat(is("test").instanceOf(Object.class)).isTrue();
  }

  @Test
  public void isNotInstanceOf() {

    Assertions.assertThat(is("false").not().instanceOf(Boolean.class)).isTrue();
    Assertions.assertThat(is(Calendar.getInstance()).not().instanceOf(Date.class)).isTrue();
    Assertions.assertThat(is("c").not().instanceOf(Character.class)).isTrue();
    Assertions.assertThat(is(Math.PI).not().instanceOf(Integer.class)).isTrue();
    Assertions.assertThat(is(2).not().instanceOf(Double.class)).isTrue();
    Assertions.assertThat(is('c').not().instanceOf(String.class)).isTrue();
    Assertions.assertThat(is(NULL).not().instanceOf(Object.class)).isTrue();
    Assertions.assertThat(is("test").not().instanceOf(null)).isTrue();
  }

  @Test
  public void isLessThan() {

    Assertions.assertThat(is(3).lessThan(1)).isFalse();
    Assertions.assertThat(is(3).lessThan(2)).isFalse();
    Assertions.assertThat(is(3).lessThan(3)).isFalse();
    Assertions.assertThat(is(3).lessThan(4)).isTrue();
    Assertions.assertThat(is(3).lessThan(5)).isTrue();
  }

  @Test
  public void isNotLessThan() {

    Assertions.assertThat(is(3).not().lessThan(1)).isTrue();
    Assertions.assertThat(is(3).not().lessThan(2)).isTrue();
    Assertions.assertThat(is(3).not().lessThan(3)).isTrue();
    Assertions.assertThat(is(3).not().lessThan(4)).isFalse();
    Assertions.assertThat(is(3).not().lessThan(5)).isFalse();
  }

  @Test
  public void isLessThanOrGreaterThan() {

    Assertions.assertThat(is(1).lessThanOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(2).lessThanOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(3).lessThanOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(4).lessThanOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(5).lessThanOrGreaterThan(2, 4)).isTrue();
  }

  @Test
  public void isNotLessThanOrGreaterThan() {

    Assertions.assertThat(is(1).not().lessThanOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(2).not().lessThanOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(3).not().lessThanOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(4).not().lessThanOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(5).not().lessThanOrGreaterThan(2, 4)).isFalse();
  }

  @Test
  public void isLessThanOrGreaterThanEqualTo() {

    Assertions.assertThat(is(1).lessThanOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(2).lessThanOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(3).lessThanOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(4).lessThanOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(5).lessThanOrGreaterThanEqualTo(2, 4)).isTrue();
  }

  @Test
  public void isNotLessThanOrGreaterThanEqualTo() {

    Assertions.assertThat(is(1).not().lessThanOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(2).not().lessThanOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(3).not().lessThanOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(4).not().lessThanOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(5).not().lessThanOrGreaterThanEqualTo(2, 4)).isFalse();
  }

  @Test
  public void isLessThanEqualTo() {

    Assertions.assertThat(is(3).lessThanEqualTo(1)).isFalse();
    Assertions.assertThat(is(3).lessThanEqualTo(2)).isFalse();
    Assertions.assertThat(is(3).lessThanEqualTo(3)).isTrue();
    Assertions.assertThat(is(3).lessThanEqualTo(4)).isTrue();
    Assertions.assertThat(is(3).lessThanEqualTo(5)).isTrue();
  }

  @Test
  public void isNotLessThanEqualTo() {

    Assertions.assertThat(is(3).not().lessThanEqualTo(1)).isTrue();
    Assertions.assertThat(is(3).not().lessThanEqualTo(2)).isTrue();
    Assertions.assertThat(is(3).not().lessThanEqualTo(3)).isFalse();
    Assertions.assertThat(is(3).not().lessThanEqualTo(4)).isFalse();
    Assertions.assertThat(is(3).not().lessThanEqualTo(5)).isFalse();
  }

  @Test
  public void isLessThanEqualToOrGreaterThan() {

    Assertions.assertThat(is(1).lessThanEqualToOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(2).lessThanEqualToOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(3).lessThanEqualToOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(4).lessThanEqualToOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(5).lessThanEqualToOrGreaterThan(2, 4)).isTrue();
  }

  @Test
  public void isNotLessThanEqualToOrGreaterThan() {

    Assertions.assertThat(is(1).not().lessThanEqualToOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(2).not().lessThanEqualToOrGreaterThan(2, 4)).isFalse();
    Assertions.assertThat(is(3).not().lessThanEqualToOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(4).not().lessThanEqualToOrGreaterThan(2, 4)).isTrue();
    Assertions.assertThat(is(5).not().lessThanEqualToOrGreaterThan(2, 4)).isFalse();
  }

  @Test
  public void isLessThanEqualToOrGreaterThanEqualTo() {

    Assertions.assertThat(is(1).lessThanEqualToOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(2).lessThanEqualToOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(3).lessThanEqualToOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(4).lessThanEqualToOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(5).lessThanEqualToOrGreaterThanEqualTo(2, 4)).isTrue();
  }

  @Test
  public void isNotLessThanEqualToOrGreaterThanEqualTo() {

    Assertions.assertThat(is(1).not().lessThanEqualToOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(2).not().lessThanEqualToOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(3).not().lessThanEqualToOrGreaterThanEqualTo(2, 4)).isTrue();
    Assertions.assertThat(is(4).not().lessThanEqualToOrGreaterThanEqualTo(2, 4)).isFalse();
    Assertions.assertThat(is(5).not().lessThanEqualToOrGreaterThanEqualTo(2, 4)).isFalse();
  }

  @Test
  public void isNotNull() {

    Assertions.assertThat(is(Boolean.FALSE).notNull()).isTrue();
    Assertions.assertThat(is(Calendar.getInstance()).notNull()).isTrue();
    Assertions.assertThat(is('\0').notNull()).isTrue();
    Assertions.assertThat(is(Integer.MIN_VALUE).notNull()).isTrue();
    Assertions.assertThat(is(Double.MAX_VALUE).notNull()).isTrue();
    Assertions.assertThat(is("null").notNull()).isTrue();
  }

  @Test
  public void isNull() {
    Assertions.assertThat(is(NULL).Null()).isTrue();
  }

  @Test
  public void isSameAs() {

    Assertions.assertThat((is(NULL).sameAs(NULL))).isTrue();
    Assertions.assertThat(is(Boolean.TRUE).sameAs(Boolean.TRUE)).isTrue();
    Assertions.assertThat(is("test").sameAs("test")).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotSameAs() {

    Assertions.assertThat(is(NULL).notSameAs("null")).isTrue();
    Assertions.assertThat(is(Boolean.TRUE).notSameAs(Boolean.FALSE)).isTrue();
    Assertions.assertThat(is("test").notSameAs(new String("test"))).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isTrue() {

    Assertions.assertThat(is(true).True()).isTrue();
    Assertions.assertThat(is(Boolean.TRUE).True()).isTrue();
    Assertions.assertThat(is(!Boolean.FALSE).True()).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotTrue() {

    Assertions.assertThat(is(false).not().True()).isTrue();
    Assertions.assertThat(is(Boolean.FALSE).not().True()).isTrue();
    Assertions.assertThat(is(!Boolean.TRUE).not().True()).isTrue();
    Assertions.assertThat(is((Boolean) null).not().True()).isTrue();
  }

  @Test
  public void isValid() {
    Assertions.assertThat(is("test").valid(argument -> true)).isTrue();
  }

  @Test
  public void isInvalid() {
    Assertions.assertThat(is("test").valid(argument -> false)).isFalse();
  }

  @Test
  public void isNotValid() {
    Assertions.assertThat(is("test").not().valid(argument -> false)).isTrue();
  }

  @Test
  public void isValidWithNullPredicateThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> is("test").valid(null))
      .withMessage("Predicate is required")
      .withNoCause();
  }

  @Test
  public void isNotEmptyWithNull() {
    assertThat(is(null).notEmpty()).isFalse();
  }

  @Test
  public void isNotNotEmptyWithNull() {
    Assertions.assertThat(is(null).not().notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyArrayWithNonEmptyArray() {

    Object[] array = { "one", "two" };

    Assertions.assertThat(is(array).notEmpty()).isTrue();
    Assertions.assertThat(is(new Object[] { "test" }).notEmpty()).isTrue();
    Assertions.assertThat(is(new Object[1]).notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyArrayWithEmptyArray() {
    Assertions.assertThat(is(new Object[0]).notEmpty()).isFalse();
  }

  @Test
  public void isNotNotEmptyArrayWithEmptyArray() {
    Assertions.assertThat(is(new Object[0]).not().notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyCollectionWithNonEmptyCollection() {

    List<?> list = Arrays.asList("one", "two");

    Assertions.assertThat(is(list).notEmpty()).isTrue();
    Assertions.assertThat(is(Collections.singleton("test")).notEmpty()).isTrue();
    Assertions.assertThat(is(Collections.singletonList("mock")).notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyCollectionWithEmptyCollection() {

    Assertions.assertThat(is(Collections.emptyList()).notEmpty()).isFalse();
    Assertions.assertThat(is(Collections.emptySet()).notEmpty()).isFalse();
  }

  @Test
  public void isNotNotEmptyCollectionWithEmptyCollection() {

    Assertions.assertThat(is(Collections.emptyList()).not().notEmpty()).isTrue();
    Assertions.assertThat(is(Collections.emptySet()).not().notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyIterableWithNonEmptyIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);
    Iterator<?> mockIterator = mock(Iterator.class);

    doReturn(mockIterator).when(mockIterable).iterator();
    doReturn(true).when(mockIterator).hasNext();

    Assertions.assertThat(is(mockIterable).notEmpty()).isTrue();

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

    Assertions.assertThat(is(mockIterable).notEmpty()).isFalse();

    verify(mockIterable, times(1)).iterator();
    verify(mockIterator, times(1)).hasNext();
    verifyNoMoreInteractions(mockIterable, mockIterator);
  }

  @Test
  public void isNotEmptyIterableWithNullIterator() {

    Iterable<?> mockIterable = mock(Iterable.class);

    doReturn(null).when(mockIterable).iterator();

    Assertions.assertThat(is(mockIterable).notEmpty()).isFalse();

    verify(mockIterable, times(1)).iterator();
    verifyNoMoreInteractions(mockIterable);
  }

  @Test
  public void isNotNotEmptyIterableWithEmptyIterable() {

    Iterable<?> mockIterable = mock(Iterable.class);
    Iterator<?> mockIterator = mock(Iterator.class);

    doReturn(mockIterator).when(mockIterable).iterator();
    doReturn(false).when(mockIterator).hasNext();

    Assertions.assertThat(is(mockIterable).not().notEmpty()).isTrue();

    verify(mockIterable, times(1)).iterator();
    verify(mockIterator, times(1)).hasNext();
    verifyNoMoreInteractions(mockIterable, mockIterator);
  }

  @Test
  public void isNotEmptyMapWithNonEmptyMap() {
    Assertions.assertThat(is(Collections.singletonMap(1, "one")).notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyMapWithEmptyMap() {
    Assertions.assertThat(is(Collections.emptyMap()).notEmpty()).isFalse();
  }

  @Test
  public void isNotNotEmptyMapWithEmptyMap() {
    Assertions.assertThat(is(Collections.emptyMap()).not().notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyStringWithNonEmptyStrings() {

    Assertions.assertThat(is("test").notEmpty()).isTrue();
    Assertions.assertThat(is("empty").notEmpty()).isTrue();
    Assertions.assertThat(is("blank").notEmpty()).isTrue();
    Assertions.assertThat(is("null").notEmpty()).isTrue();
    Assertions.assertThat(is("nil").notEmpty()).isTrue();
    Assertions.assertThat(is("_").notEmpty()).isTrue();
    Assertions.assertThat(is(" ").notEmpty()).isTrue();
    Assertions.assertThat(is("  ").notEmpty()).isTrue();
    Assertions.assertThat(is("\0").notEmpty()).isTrue();
    Assertions.assertThat(is("\t").notEmpty()).isTrue();
    Assertions.assertThat(is("\n").notEmpty()).isTrue();
  }

  @Test
  public void isNotEmptyWithEmptyStrings() {
    Assertions.assertThat(is("").notEmpty()).isFalse();
  }

  @Test
  public void isNotNotEmptyStringWithEmptyString() {
    Assertions.assertThat(is("").not().notEmpty()).isTrue();
  }

  @Test
  public void isReuse() {

    Is<String> isOperator = is("test");

    Assertions.assertThat(isOperator).isNotNull();
    Assertions.assertThat(isOperator.equalTo("test")).isTrue();
    Assertions.assertThat(isOperator.equalTo("testing")).isFalse();
    Assertions.assertThat(isOperator.not().equalTo("testing")).isTrue();
    Assertions.assertThat(isOperator.equalTo("test")).isTrue();
    Assertions.assertThat(isOperator.equalTo("TEST")).isFalse();
    Assertions.assertThat(isOperator.not().equalTo("TEST")).isTrue();
    Assertions.assertThat(isOperator.equalTo("test")).isTrue();
  }

  @Test
  public void isDoubleNegative() {

    Assertions.assertThat(is(true).not().not().True()).isTrue();
    Assertions.assertThat(is(false).not().not().False()).isTrue();
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
  @SuppressWarnings("unused")
  @RequiredArgsConstructor(staticName = "newPerson")
  private static class Person implements Comparable<Person> {

    private final Long id;

    private final String firstName;

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

      if (!(obj instanceof Person that)) {
        return false;
      }

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
  interface Invoice {

    LineItem findBy(int index);

    LineItem findBy(String productName);

    BigDecimal getTotal();

    BigDecimal getTotal(String... productNames);

  }

  @Getter
  @RequiredArgsConstructor(staticName = "of")
  static class TestInvoice implements Invoice {

    private final List<TestLineItem> lineItems;

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
  interface LineItem {

    Product getProduct();

    Integer getQuantity();

    BigDecimal getCost();

  }

  @Getter
  @RequiredArgsConstructor(staticName = "newLineItem")
  static class TestLineItem implements LineItem {

    private final Product product;

    private final Integer quantity;

    public BigDecimal getCost() {
      return getProduct().getCost(getQuantity());
    }

    @Override
    public String toString() {
      return String.format("%s|%d", getProduct(), getQuantity());
    }
  }

  interface Product {

    String getName();

    BigDecimal getPrice();

    BigDecimal getCost(int quantity);

  }

  @Getter
  @RequiredArgsConstructor(staticName = "newProduct")
  static class TestProduct implements Product {

    private final String name;

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
  static class User implements Comparable<User> {

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
