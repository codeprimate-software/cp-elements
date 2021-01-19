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
public class ValueHolderTest {

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

    ValueHolder<String> testOne = new ValueHolder<>("test");
    ValueHolder<String> testTwo = new ValueHolder<>("test");

    assertThat(testOne).isNotSameAs(testTwo);
    assertThat(testOne).isEqualTo(testTwo);
  }

  @Test
  public void unequalValues() {

    ValueHolder<String> testOne = new ValueHolder<>("test");
    ValueHolder<String> mockOne = new ValueHolder<>("mock");

    assertThat(testOne).isNotSameAs(mockOne);
    assertThat(testOne).isNotEqualTo(mockOne);
  }

  @Test
  public void hashCodeValue() {

    ValueHolder<String> valueHolder = new ValueHolder<>("test");

    assertThat(valueHolder.hashCode()).isNotEqualTo(0);
    assertThat(valueHolder.hashCode()).isNotEqualTo("test".hashCode());
  }

  @Test
  public void testToString() {

    ValueHolder<Object> valueHolder = new ValueHolder<>("test");

    assertThat(valueHolder.toString()).isEqualTo("test");

    valueHolder.setValue(2L);

    assertThat(valueHolder.toString()).isEqualTo("2");

    valueHolder.setValue(Math.PI);

    assertThat(valueHolder.toString()).isEqualTo(String.valueOf(Math.PI));

    valueHolder.setValue(true);

    assertThat(valueHolder.toString()).isEqualTo("true");
  }

  @Test
  public void withComparableValue() {

    ValueHolder.ComparableValueHolder<String> valueOne = ValueHolder.withComparableValue("one");

    assertThat(valueOne).isInstanceOf(Comparable.class);
    assertThat(valueOne.getValue()).isInstanceOf(Comparable.class);
    assertThat(valueOne.getValue()).isEqualTo("one");
    assertThat(valueOne.compareTo("two") < 0).isTrue();
    assertThat(valueOne.compareTo("none") > 0).isTrue();
    assertThat(valueOne.compareTo("one") == 0).isTrue();
  }

  @Test
  public void withImmutableValue() {

    Calendar today = Calendar.getInstance();

    ValueHolder<Calendar> dayHolder = ValueHolder.withImmutableValue(today);

    assertThat(dayHolder).isNotNull();

    Calendar todayClone = dayHolder.getValue();

    assertThat(todayClone).isInstanceOf(Calendar.class);
    assertThat(todayClone).isNotSameAs(today);
    assertThat(todayClone).isEqualTo(today);

    todayClone.add(Calendar.DAY_OF_MONTH, -1); // yesterday

    Calendar anotherTodayClone = dayHolder.getValue();

    assertThat(anotherTodayClone).isInstanceOf(Calendar.class);
    assertThat(anotherTodayClone).isNotSameAs(todayClone);
    assertThat(anotherTodayClone).isNotEqualTo(todayClone);
    assertThat(anotherTodayClone).isNotSameAs(today);
    assertThat(anotherTodayClone).isEqualTo(today);

    Calendar tomorrow = Calendar.getInstance();
    tomorrow.add(Calendar.DAY_OF_MONTH, 1);
    dayHolder.setValue(tomorrow);

    Calendar tomorrowClone = dayHolder.getValue();

    assertThat(tomorrowClone).isInstanceOf(Calendar.class);
    assertThat(tomorrowClone).isNotSameAs(tomorrow);
    assertThat(tomorrowClone).isEqualTo(tomorrow);

    Calendar tomorrowCopy = (Calendar) tomorrow.clone();

    tomorrow.add(Calendar.YEAR, 1);

    Calendar anotherTomorrowClone = dayHolder.getValue();

    assertThat(anotherTomorrowClone).isInstanceOf(Calendar.class);
    assertThat(anotherTomorrowClone).isNotSameAs(tomorrow);
    assertThat(anotherTomorrowClone).isNotEqualTo(tomorrow);
    assertThat(anotherTomorrowClone).isNotSameAs(tomorrowCopy);
    assertThat(anotherTomorrowClone).isEqualTo(tomorrowCopy);
  }

  @Test
  public void withNonNullValue() {

    ValueHolder<String> valueHolder = ValueHolder.withNonNullValue("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");

    valueHolder.setValue("null");

    assertThat(valueHolder.getValue()).isEqualTo("null");

    valueHolder.setValue("mock");

    assertThat(valueHolder.getValue()).isEqualTo("mock");

    valueHolder.setValue("nil");

    assertThat(valueHolder.getValue()).isEqualTo("nil");
  }

  @Test(expected = IllegalArgumentException.class)
  public void withNonNullValueHolderConstructedWithNullValue() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> ValueHolder.withNonNullValue(null),
      () -> "The value must not be null!");
  }

  @Test(expected = IllegalArgumentException.class)
  public void withNonNullValueHolderSettingNullValue() {

    ValueHolder<String> valueHolder = ValueHolder.withNonNullValue("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");

    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> valueHolder.setValue(null),
      () -> "The value must not be null!");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void withSerializableValue() throws Exception {

    ValueHolder<Integer> twentyOne = ValueHolder.withSerializableValue(21);

    ByteArrayOutputStream out = new ByteArrayOutputStream();

    ObjectOutputStream objectOut = new ObjectOutputStream(out);

    objectOut.writeObject(twentyOne);
    objectOut.flush();
    objectOut.close();

    byte[] twentyOneBytes = out.toByteArray();

    assertThat(twentyOneBytes).isNotNull();
    assertThat(twentyOneBytes.length).isNotEqualTo(0);

    ObjectInputStream objectIn = new ObjectInputStream(new ByteArrayInputStream(twentyOneBytes));

    ValueHolder<Integer> twentyOneCopy = (ValueHolder<Integer>) objectIn.readObject();

    objectIn.close();

    assertThat(twentyOneCopy).isNotNull();
    assertThat(twentyOneCopy).isNotSameAs(twentyOne);
    assertThat(twentyOneCopy).isEqualTo(twentyOne);
  }
}
