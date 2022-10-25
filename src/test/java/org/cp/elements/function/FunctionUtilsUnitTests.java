/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.function;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.junit.Test;

import org.cp.elements.security.model.User;

/**
 * Unit Tests for {@link FunctionUtils}
 *
 * @author John Blum
 * @see java.util.function.Consumer
 * @see java.util.function.Function
 * @see java.util.function.Predicate
 * @see java.util.function.Supplier
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.function.FunctionUtils
 * @since 1.0.0
 */
public class FunctionUtilsUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  public void composeFunctionsIsCorrect() {

    Function<Integer, Integer> sum = value -> value + value;
    Function<Integer, Integer> multiply = value -> value * value;

    Function<Integer, Integer> function = FunctionUtils.compose(sum, multiply);

    assertThat(function).isNotNull();
    assertThat(function.apply(2)).isEqualTo(16);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeFunctionsContainingNullsIsNullSafe() {

    Function<String, String> duplicate = stringValue -> stringValue + " " + stringValue;

    Function<String, String> function =
      FunctionUtils.compose(null, duplicate, null, null, duplicate, duplicate, null);

    assertThat(function).isNotNull();
    assertThat(function.apply("TEST")).isEqualTo("TEST TEST TEST TEST TEST TEST TEST TEST");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeFunctionsOfDifferentValuesIsCorrect() {

    Function<String, Integer> toInteger = Integer::parseInt;

    Function<Integer, Par> toEnum = Par::valueOf;

    Function<String, Par> function = FunctionUtils.compose(toInteger, toEnum);

    assertThat(function).isNotNull();
    assertThat(function.apply("4")).isEqualTo(Par.FOUR);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void composeNullFunctionsIsNullSate() {

    Function<Object, Object> function = FunctionUtils.compose((Function<?, ?>[]) null);

    assertThat(function).isNotNull();
    assertThat(function.apply("TEST")).isEqualTo("TEST");
  }

  @Test
  public void noopConsumerIsCorrect() {

    User<?> mockUser = mock(User.class);

    Consumer<User<?>> userConsumer = FunctionUtils.noopConsumer();

    assertThat(userConsumer).isNotNull();

    userConsumer.accept(mockUser);

    verifyNoInteractions(mockUser);
  }

  @Test
  public void noopSupplierIsCorrect() {

    Supplier<User<?>> supplier = FunctionUtils.noopSupplier();

    assertThat(supplier).isNotNull();
    assertThat(supplier.get()).isNull();
  }

  @Test
  public void nullSafeConsumerWithNonNullConsumer() {

    Consumer<?> mockConsumer = mock(Consumer.class);

    assertThat(FunctionUtils.nullSafeConsumer(mockConsumer)).isSameAs(mockConsumer);

    verifyNoInteractions(mockConsumer);
  }

  @Test
  public void nullSafeConsumerWithNullConsumer() {

    Consumer<?> consumer = FunctionUtils.nullSafeConsumer(null);

    assertThat(consumer).isNotNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void nullSafeFunctionWithNonNullFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);

    assertThat(FunctionUtils.nullSafeFunction(mockFunction)).isSameAs(mockFunction);

    verifyNoInteractions(mockFunction);
  }

  @Test
  public void nullSafeFunctionWithNullFunction() {

    Function<Object, Object> function = FunctionUtils.nullSafeFunction(null);

    assertThat(function).isNotNull();
    assertThat(function.apply("test")).isEqualTo("test");
  }

  @Test
  public void nullSafePredicateMatchAllWithNonNullPredicate() {

    Predicate<?> mockPredicate = mock(Predicate.class);

    assertThat(FunctionUtils.nullSafePredicateMatchAll(mockPredicate)).isSameAs(mockPredicate);

    verifyNoInteractions(mockPredicate);
  }

  @Test
  public void nullSafePredicateMatchAllWithNullPredicate() {

    Predicate<Object> predicate = FunctionUtils.nullSafePredicateMatchAll(null);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test("test")).isTrue();
    assertThat(predicate.test("nil")).isTrue();
    assertThat(predicate.test("mock")).isTrue();
    assertThat(predicate.test(null)).isTrue();
  }

  @Test
  public void nullSafePredicateMatchNoneWithNonNullPredicate() {

    Predicate<?> mockPredicate = mock(Predicate.class);

    assertThat(FunctionUtils.nullSafePredicateMatchNone(mockPredicate)).isSameAs(mockPredicate);

    verifyNoInteractions(mockPredicate);
  }

  @Test
  public void nullSafePredicateMatchNoneWithNullPredicate() {

    Predicate<Object> predicate = FunctionUtils.nullSafePredicateMatchNone(null);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test("test")).isFalse();
    assertThat(predicate.test("nil")).isFalse();
    assertThat(predicate.test("mock")).isFalse();
    assertThat(predicate.test(null)).isFalse();
  }

  @Test
  public void nullSafeSupplierWithNonNullSupplier() {

    Supplier<?> mockSupplier = mock(Supplier.class);

    assertThat(FunctionUtils.nullSafeSupplier(mockSupplier)).isSameAs(mockSupplier);

    verifyNoInteractions(mockSupplier);
  }

  @Test
  public void nullSafeSupplierWithNullSupplier() {

    Supplier<?> supplier = FunctionUtils.nullSafeSupplier(null);

    assertThat(supplier).isNotNull();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toConsumerFromFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);

    Consumer<Object> consumer = FunctionUtils.toConsumer(mockFunction);

    assertThat(consumer).isNotNull();

    consumer.accept("TEST");

    verify(mockFunction, times(1)).apply(eq("TEST"));
    verifyNoMoreInteractions(mockFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toConsumerFromNullFunction() {

    try {
      FunctionUtils.toConsumer(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toFunctionFromConsumer() {

    Consumer<Object> mockConsumer = mock(Consumer.class);

    Function<Object, Object> function = FunctionUtils.toFunction(mockConsumer);

    assertThat(function).isNotNull();
    assertThat(function.apply("TEST")).isEqualTo("TEST");

    verify(mockConsumer, times(1)).accept(eq("TEST"));
    verifyNoMoreInteractions(mockConsumer);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toFunctionFromNullConsumer() {

    try {
      FunctionUtils.toFunction((Consumer<?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Consumer is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toFunctionFromPredicate() {

    Predicate<Object> mockPredicate = mock(Predicate.class);

    doReturn(true).when(mockPredicate).test(any());

    Function<Object, Boolean> function = FunctionUtils.toFunction(mockPredicate);

    assertThat(function).isNotNull();
    assertThat(function.apply("test")).isTrue();

    verify(mockPredicate, times(1)).test(eq("test"));
    verifyNoMoreInteractions(mockPredicate);
  }

  @SuppressWarnings("all")
  @Test(expected = IllegalArgumentException.class)
  public void toFunctionFromNullPredicate() {

    try {
      FunctionUtils.toFunction((Predicate<?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Predicate is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toFunctionFromSupplier() {

    Supplier<Object> mockSupplier = mock(Supplier.class);

    doReturn("TEST").when(mockSupplier).get();

    Function<Object, Object> function = FunctionUtils.toFunction(mockSupplier);

    assertThat(function).isNotNull();
    assertThat(function.apply(null)).isEqualTo("TEST");

    verify(mockSupplier, times(1)).get();
    verifyNoMoreInteractions(mockSupplier);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toFunctionFromNullSupplier() {

    try {
      FunctionUtils.toFunction((Supplier<?>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Supplier is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toPredicateFromFunction() {

    Function<Object, Boolean> mockFunction = mock(Function.class);

    doReturn(true).when(mockFunction).apply(any());

    Predicate<Object> predicate = FunctionUtils.toPredicate(mockFunction);

    assertThat(predicate).isNotNull();
    assertThat(predicate.test("test")).isTrue();
    assertThat(predicate.negate().test("mock")).isFalse();

    verify(mockFunction, times(1)).apply(eq("test"));
    verify(mockFunction, times(1)).apply(eq("mock"));
    verifyNoMoreInteractions(mockFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toPredicateFromNullFunction() {

    try {
      FunctionUtils.toPredicate(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void toSupplierFromFunction() {

    Function<Object, Object> mockFunction = mock(Function.class);

    doReturn("TEST").when(mockFunction).apply(any());

    Supplier<Object> supplier = FunctionUtils.toSupplier(mockFunction);

    assertThat(supplier).isNotNull();
    assertThat(supplier.get()).isEqualTo("TEST");

    verify(mockFunction, times(1)).apply(isNull());
    verifyNoMoreInteractions(mockFunction);
  }

  @Test(expected = IllegalArgumentException.class)
  public void toSupplierFromNullFunction() {

    try {
      FunctionUtils.toSupplier(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Function is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  enum Par {

    THREE(3), FOUR(4), FIVE(5);

    static Par valueOf(int parValue) {

      return Arrays.stream(Par.values())
        .filter(par -> par.getValue() == parValue)
        .findFirst()
        .orElse(null);
    }

    final int value;

    Par(int value) {
      this.value = value;
    }

    public int getValue() {
      return value;
    }
  }
}
