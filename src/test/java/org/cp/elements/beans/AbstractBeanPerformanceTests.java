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
package org.cp.elements.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;

import org.junit.Assert;

/**
 * Test suite of test cases testing and measuring the performance of the {@link AbstractBean} class.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.AbstractBean
 * @since 1.0.0
 */
public class AbstractBeanPerformanceTests {

  private static final boolean EVENT_DISPATCH_ENABLED = false;
  private static final int NUMBER_OF_OPERATIONS = 1000000;

  public static void main(String... args) {

    testPerformanceOfCallback();
    testPerformanceOfReflection();
    testPerformanceOfSetter();
    testPerformanceOfSetterWithEventNotification();
  }

  private static void testPerformanceOfCallback() {
    ValueHolder<Integer> valueHolder = new ValueHolder<>();

    long timeBegin = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setCallbackValue(count);
      Assert.assertEquals(count, valueHolder.getCallbackValue().intValue());
    }

    long timeEnd = System.currentTimeMillis();

    System.out.printf("Setting through callback took [%d] milliseconds.%n", timeEnd - timeBegin);
  }

  private static void testPerformanceOfReflection() {
    ValueHolder<Integer> valueHolder = new ValueHolder<>();

    long timeBegin = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setReflectionValue(count);
      Assert.assertEquals(count, valueHolder.getReflectionValue().intValue());
    }

    long timeEnd = System.currentTimeMillis();

    System.out.printf("Setting through reflection took [%d] milliseconds.%n", timeEnd - timeBegin);
  }

  private static void testPerformanceOfSetter() {
    ValueHolder<Integer> valueHolder = new ValueHolder<>();

    long timeBegin = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setValue(count);
      Assert.assertEquals(count, valueHolder.getValue().intValue());
    }

    long timeEnd = System.currentTimeMillis();

    System.out.printf("Calling setter took [%d] milliseconds.%n", timeEnd - timeBegin);
  }

  private static void testPerformanceOfSetterWithEventNotification() {
    ValueHolder<Integer> valueHolder = new ValueHolder<>();

    long timeBegin = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setNotificationValue(count);
      Assert.assertEquals(count, valueHolder.getNotificationValue().intValue());
    }

    long timeEnd = System.currentTimeMillis();

    System.out.printf("Calling setter with event notification took [%d] milliseconds.", timeEnd - timeBegin);
  }

  @SuppressWarnings("unused")
  private static final class ValueHolder<T> extends AbstractBean<Long, String, String> {

    private T callbackValue;
    private T notificationValue;
    private T reflectionValue;
    private T value;

    public ValueHolder() {
      setEventDispatchEnabled(EVENT_DISPATCH_ENABLED);
    }

    public T getCallbackValue() {
      return callbackValue;
    }

    public void setCallbackValue(T callbackValue) {
      processChange("callbackValue", this.callbackValue, callbackValue, newValue -> ValueHolder.this.callbackValue = newValue);
    }

    public T getNotificationValue() {
      return notificationValue;
    }

    public void setNotificationValue(T notificationValue) {
      try {
        PropertyChangeEvent event = newPropertyChangeEvent("notificationValue",
          this.notificationValue, notificationValue);

        fireVetoableChange(event);
        this.notificationValue = notificationValue;
        firePropertyChange(event);
        fireChange();
      }
      catch (PropertyVetoException e) {
        throw new IllegalPropertyValueException(e);
      }
    }

    public T getReflectionValue() {
      return reflectionValue;
    }

    public void setReflectionValue(T reflectionValue) {
      processChange("reflectionValue", this.reflectionValue, reflectionValue);
    }

    public T getValue() {
      return value;
    }

    public void setValue(T value) {
      this.value = value;
    }
  }
}
