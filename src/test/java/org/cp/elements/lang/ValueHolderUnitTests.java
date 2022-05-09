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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Calendar;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link ValueHolder}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.ValueHolder
 * @see org.cp.elements.test.TestUtils
 * @since 1.0.0
 */
public class ValueHolderUnitTests {

  @Test
  public void constructWithNullValue() {

    ValueHolder<Object> valueHolder = new ValueHolder<>(null);

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isNull();
  }

  @Test
  public void constructWithNonNullValue() {

    ValueHolder<String> valueHolder = new ValueHolder<>("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");
  }

  @Test
  public void setAndGetValue() {

    ValueHolder<String> valueHolder = new ValueHolder<>();

    assertThat(valueHolder.getValue()).isNull();

    valueHolder.setValue("test");

    assertThat(valueHolder.getValue()).isEqualTo("test");

    valueHolder.setValue(null);

    assertThat(valueHolder.getValue()).isNull();

    valueHolder.setValue("mock");

    assertThat(valueHolder.getValue()).isEqualTo("mock");
  }

  @Test
  public void equalValues() {

    ValueHolder<String> testValueOne = new ValueHolder<>("test");
    ValueHolder<String> testValueTwo = new ValueHolder<>("test");

    assertThat(testValueOne).isNotSameAs(testValueTwo);
    assertThat(testValueOne).isEqualTo(testValueTwo);
    assertThat(testValueTwo).isEqualTo(testValueOne);
  }

  @Test
  public void unequalValues() {

    ValueHolder<String> testValue = new ValueHolder<>("test");
    ValueHolder<String> mockValue = new ValueHolder<>("mock");

    assertThat(testValue).isNotSameAs(mockValue);
    assertThat(testValue).isNotEqualTo(mockValue);
    assertThat(mockValue).isNotEqualTo(testValue);
  }

  @Test
  public void hashCodeValue() {

    ValueHolder<String> valueHolder = new ValueHolder<>("test");

    assertThat(valueHolder.hashCode()).isNotEqualTo(0);
    assertThat(valueHolder.hashCode()).isNotEqualTo("test".hashCode());
    assertThat(valueHolder.hashCode()).isEqualTo(ObjectUtils.hashCodeOf("test"));
  }

  @Test
  public void toStringIsValue() {

    ValueHolder<Object> valueHolder = new ValueHolder<>("test");

    assertThat(valueHolder.toString()).isEqualTo("test");

    valueHolder.setValue(2L);

    assertThat(valueHolder.toString()).isEqualTo("2");

    valueHolder.setValue(Math.PI);

    assertThat(valueHolder.toString()).isEqualTo(String.valueOf(Math.PI));

    valueHolder.setValue(true);

    assertThat(valueHolder.toString()).isEqualTo("true");

    valueHolder.setValue('x');

    assertThat(valueHolder.toString()).isEqualTo("x");
  }

  @Test
  public void withComparableValue() {

    ValueHolder.ComparableValueHolder<String> comparableValue = ValueHolder.withComparableValue("one");

    assertThat(comparableValue).isInstanceOf(Comparable.class);
    assertThat(comparableValue.getValue()).isInstanceOf(Comparable.class);
    assertThat(comparableValue.getValue()).isEqualTo("one");
    assertThat(comparableValue.compareTo("two") < 0).isTrue();
    assertThat(comparableValue.compareTo("none") > 0).isTrue();
    assertThat(comparableValue.compareTo("one") == 0).isTrue();
  }

  @Test
  public void withComparableValueInitializedWithNullIsNullSafe() {

    ValueHolder.ComparableValueHolder<String> nullComparableValue = ValueHolder.withComparableValue(null);

    assertThat(nullComparableValue).isNotNull();
    assertThat(nullComparableValue.getValue()).isNull();
    assertThat(nullComparableValue.compareTo("test") > 0).isTrue();
  }

  @Test
  public void withImmutableValue() {

    Calendar today = Calendar.getInstance();

    ValueHolder<Calendar> immutableValue = ValueHolder.withImmutableValue(today);

    assertThat(immutableValue).isNotNull();

    Calendar todayClone = immutableValue.getValue();

    assertThat(todayClone).isInstanceOf(Calendar.class);
    assertThat(todayClone).isNotSameAs(today);
    assertThat(todayClone).isEqualTo(today);

    todayClone.add(Calendar.DAY_OF_MONTH, -1); // yesterday

    Calendar anotherTodayClone = immutableValue.getValue();

    assertThat(anotherTodayClone).isInstanceOf(Calendar.class);
    assertThat(anotherTodayClone).isNotSameAs(todayClone);
    assertThat(anotherTodayClone).isNotEqualTo(todayClone);
    assertThat(anotherTodayClone).isNotSameAs(today);
    assertThat(anotherTodayClone).isEqualTo(today);

    Calendar tomorrow = Calendar.getInstance();

    tomorrow.add(Calendar.DAY_OF_MONTH, 1);
    immutableValue.setValue(tomorrow);

    Calendar tomorrowClone = immutableValue.getValue();

    assertThat(tomorrowClone).isInstanceOf(Calendar.class);
    assertThat(tomorrowClone).isNotSameAs(tomorrow);
    assertThat(tomorrowClone).isEqualTo(tomorrow);

    Calendar tomorrowCopy = (Calendar) tomorrow.clone();

    immutableValue.setValue(tomorrowCopy);
    tomorrowCopy.add(Calendar.YEAR, 1); // tomorrow next year

    Calendar anotherTomorrowClone = immutableValue.getValue();

    assertThat(anotherTomorrowClone).isInstanceOf(Calendar.class);
    assertThat(anotherTomorrowClone).isNotSameAs(tomorrowCopy);
    assertThat(anotherTomorrowClone).isNotEqualTo(tomorrowCopy);
    assertThat(anotherTomorrowClone).isNotSameAs(tomorrow);
    assertThat(anotherTomorrowClone).isEqualTo(tomorrow);
  }

  @Test(expected = IllegalTypeException.class)
  public void withImmutableValueInitializedWithNull() {

    try {
      ValueHolder.withImmutableValue(null);
    }
    catch (IllegalTypeException expected) {

      assertThat(expected).hasMessage("Value [null] is not Cloneable");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void withNonNullValue() {

    ValueHolder<String> nonNullValue = ValueHolder.withNonNullValue("test");

    assertThat(nonNullValue).isNotNull();
    assertThat(nonNullValue.getValue()).isEqualTo("test");

    nonNullValue.setValue("null");

    assertThat(nonNullValue.getValue()).isEqualTo("null");

    nonNullValue.setValue("mock");

    assertThat(nonNullValue.getValue()).isEqualTo("mock");

    nonNullValue.setValue("nil");

    assertThat(nonNullValue.getValue()).isEqualTo("nil");
  }

  @Test(expected = IllegalArgumentException.class)
  public void withNonNullValueInitializedWithNull() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> ValueHolder.withNonNullValue(null),
      () -> "Value is required");
  }

  @Test(expected = IllegalArgumentException.class)
  public void withNonNullValueSetToNull() {

    ValueHolder<String> nonNullValue = ValueHolder.withNonNullValue("test");

    assertThat(nonNullValue).isNotNull();
    assertThat(nonNullValue.getValue()).isEqualTo("test");

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> nonNullValue.setValue(null),
      () -> "Value is required", () -> assertThat(nonNullValue.getValue()).isEqualTo("test"));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void withSerializableValue() throws Exception {

    ValueHolder<Integer> twentyOne = ValueHolder.withSerializableValue(21);

    assertThat(twentyOne).isNotNull();
    assertThat(twentyOne.getValue()).isEqualTo(21);

    ByteArrayOutputStream out = new ByteArrayOutputStream();

    ObjectOutputStream objectOut = new ObjectOutputStream(out);

    objectOut.writeObject(twentyOne);
    objectOut.flush();
    objectOut.close();

    byte[] twentyOneBytes = out.toByteArray();

    assertThat(twentyOneBytes).isNotNull();
    assertThat(twentyOneBytes.length).isNotZero();

    ObjectInputStream objectIn = new ObjectInputStream(new ByteArrayInputStream(twentyOneBytes));

    ValueHolder<Integer> twentyOneCopy = (ValueHolder<Integer>) objectIn.readObject();

    objectIn.close();

    assertThat(twentyOneCopy).isNotNull();
    assertThat(twentyOneCopy).isNotSameAs(twentyOne);
    assertThat(twentyOneCopy).isEqualTo(twentyOne);
    assertThat(twentyOneCopy.getValue()).isEqualTo(21);
  }
}
