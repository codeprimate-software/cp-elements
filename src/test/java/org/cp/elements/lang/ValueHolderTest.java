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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.Calendar;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The ValueHolderTest class is a test suite of test cases testing the contract and functionality
 * of the ValueHolder class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.ValueHolder
 * @see org.junit.Test
 * @since 1.0.0
 */
public class ValueHolderTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void constructWithNullValue() {
    ValueHolder<Object> valueHolder = new ValueHolder<Object>(null);

    assertThat(valueHolder, is(not(nullValue())));
    assertThat(valueHolder.getValue(), is(nullValue()));
  }

  @Test
  public void constructWithNonNullValue() {
    ValueHolder<String> valueHolder = new ValueHolder<String>("test");

    assertThat(valueHolder, is(not(nullValue())));
    assertThat(valueHolder.getValue(), is(equalTo("test")));
  }

  @Test
  public void setAndGetValue() {
    ValueHolder<String> valueHolder = new ValueHolder<String>();

    assertThat(valueHolder.getValue(), is(nullValue(String.class)));

    valueHolder.setValue("test");

    assertThat(valueHolder.getValue(), is(equalTo("test")));

    valueHolder.setValue(null);

    assertThat(valueHolder.getValue(), is(nullValue()));

    valueHolder.setValue("mock");

    assertThat(valueHolder.getValue(), is(equalTo("mock")));
  }

  @Test
  public void equalValues() {
    ValueHolder<String> testOne = new ValueHolder<String>("test");
    ValueHolder<String> testTwo = new ValueHolder<String>("test");

    assertThat(testOne, is(not(sameInstance(testTwo))));
    assertThat(testOne, is(equalTo(testTwo)));
  }

  @Test
  public void unequalValues() {
    ValueHolder<String> testOne = new ValueHolder<String>("test");
    ValueHolder<String> mockOne = new ValueHolder<String>("mock");

    assertThat(testOne, is(not(sameInstance(mockOne))));
    assertThat(testOne, is(not(equalTo(mockOne))));
  }

  @Test
  public void hashCodeValue() {
    ValueHolder<String> valueHolder = new ValueHolder<String>("test");

    assertThat(valueHolder.hashCode(), is(not(equalTo(0))));
    assertThat(valueHolder.hashCode(), is(not(equalTo("test".hashCode()))));
  }

  @Test
  public void testToString() {
    ValueHolder<Object> valueHolder = new ValueHolder<Object>("test");

    assertThat(valueHolder.toString(), is(equalTo("test")));

    valueHolder.setValue(2l);

    assertThat(valueHolder.toString(), is(equalTo("2")));

    valueHolder.setValue(Math.PI);

    assertThat(valueHolder.toString(), is(equalTo(String.valueOf(Math.PI))));

    valueHolder.setValue(true);

    assertThat(valueHolder.toString(), is(equalTo("true")));
  }

  @Test
  public void withComparableValue() {
    ValueHolder.ComparableValueHolder<String> valueOne = ValueHolder.withComparableValue("one");

    assertThat(valueOne, is(instanceOf(Comparable.class)));
    assertThat(valueOne.getValue(), is(instanceOf(Comparable.class)));
    assertThat(valueOne.getValue(), is(equalTo("one")));
    assertTrue(valueOne.compareTo("two") < 0);
    assertTrue(valueOne.compareTo("none") > 0);
    assertTrue(valueOne.compareTo("one") == 0);
  }

  @Test
  public void withImmutableValue() {
    Calendar today = Calendar.getInstance();
    ValueHolder<Calendar> dayHolder = ValueHolder.withImmutableValue(today);

    assertThat(dayHolder, is(not(nullValue())));

    Calendar todayClone = dayHolder.getValue();

    assertThat(todayClone, is(instanceOf(Calendar.class)));
    assertThat(todayClone, is(not(sameInstance(today))));
    assertThat(todayClone, is(equalTo(today)));

    todayClone.add(Calendar.DAY_OF_MONTH, -1); // yesterday

    Calendar anotherTodayClone = dayHolder.getValue();

    assertThat(anotherTodayClone, is(instanceOf(Calendar.class)));
    assertThat(anotherTodayClone, is(not(sameInstance(todayClone))));
    assertThat(anotherTodayClone, is(not(equalTo(todayClone))));
    assertThat(anotherTodayClone, is(not(sameInstance(today))));
    assertThat(anotherTodayClone, is(equalTo(today)));

    Calendar tomorrow = Calendar.getInstance();
    tomorrow.add(Calendar.DAY_OF_MONTH, 1);
    dayHolder.setValue(tomorrow);

    Calendar tomorrowClone = dayHolder.getValue();

    assertThat(tomorrowClone, is(instanceOf(Calendar.class)));
    assertThat(tomorrowClone, is(not(sameInstance(tomorrow))));
    assertThat(tomorrowClone, is(equalTo(tomorrow)));

    Calendar tomorrowCopy = (Calendar) tomorrow.clone();

    tomorrow.add(Calendar.YEAR, 1);

    Calendar anotherTomorrowClone = dayHolder.getValue();

    assertThat(anotherTomorrowClone, is(instanceOf(Calendar.class)));
    assertThat(anotherTomorrowClone, is(not(sameInstance(tomorrow))));
    assertThat(anotherTomorrowClone, is(not(equalTo(tomorrow))));
    assertThat(anotherTomorrowClone, is(not(sameInstance(tomorrowCopy))));
    assertThat(anotherTomorrowClone, is(equalTo(tomorrowCopy)));
  }

  @Test
  public void withNonNullValue() {
    ValueHolder<String> valueHolder = ValueHolder.withNonNullValue("test");

    assertThat(valueHolder, is(not(nullValue())));
    assertThat(valueHolder.getValue(), is(equalTo("test")));

    valueHolder.setValue("null");

    assertThat(valueHolder.getValue(), is(equalTo("null")));

    valueHolder.setValue("mock");

    assertThat(valueHolder.getValue(), is(equalTo("mock")));

    valueHolder.setValue("nil");

    assertThat(valueHolder.getValue(), is(equalTo("nil")));
  }

  @Test
  public void withNonNullValueHolderConstructedWithNullValue() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The value must not be null!");

    ValueHolder.withNonNullValue(null);
  }

  @Test
  public void withNonNullValueHolderSettingNullValue() {
    ValueHolder<String> valueHolder = ValueHolder.withNonNullValue("test");

    assertThat(valueHolder, is(not(nullValue())));
    assertThat(valueHolder.getValue(), is(equalTo("test")));

    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The value must not be null!");

    valueHolder.setValue(null);
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

    assertThat(twentyOneBytes, is(not(nullValue())));
    assertThat(twentyOneBytes.length, is(not(equalTo(0))));

    ObjectInputStream objectIn = new ObjectInputStream(new ByteArrayInputStream(twentyOneBytes));

    ValueHolder<Integer> twentyOneCopy = (ValueHolder<Integer>) objectIn.readObject();

    objectIn.close();

    assertThat(twentyOneCopy, is(not(nullValue())));
    assertThat(twentyOneCopy, is(not(sameInstance(twentyOne))));
    assertThat(twentyOneCopy, is(equalTo(twentyOne)));
  }
}
