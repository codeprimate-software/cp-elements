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

package org.cp.elements.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;

import org.junit.Assert;

/**
 * The AbstractBeanPerformanceTest class is a test suite for measuring and collection runtime performance metrics
 * for the AbstractBean class.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.AbstractBean
 * @since 1.0.0
 */
public class AbstractBeanPerformanceTest {

  private static final boolean EVENT_DISPATCH_ENABLED = false;
  private static final int NUMBER_OF_OPERATIONS = 1000000;

  public static void main(final String... args) {
    processByCallback();
    processByReflection();
    processBySetter();
    processBySetterWithEventNotification();
  }

  private static void processByCallback() {
    final ValueHolder<Integer> valueHolder = new ValueHolder<>();
    final long t0 = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setCallbackValue(count);
      Assert.assertEquals(count, valueHolder.getCallbackValue().intValue());
    }

    final long t1 = System.currentTimeMillis();

    System.out.println("Setting through callback took (" + (t1 - t0) + ") milliseconds.");
  }

  private static void processByReflection() {
    final ValueHolder<Integer> valueHolder = new ValueHolder<>();
    final long t0 = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setReflectionValue(count);
      Assert.assertEquals(count, valueHolder.getReflectionValue().intValue());
    }

    final long t1 = System.currentTimeMillis();

    System.out.println("Setting through reflection took (" + (t1 - t0) + ") milliseconds.");
  }

  private static void processBySetter() {
    final ValueHolder<Integer> valueHolder = new ValueHolder<>();
    final long t0 = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setValue(count);
      Assert.assertEquals(count, valueHolder.getValue().intValue());
    }

    final long t1 = System.currentTimeMillis();

    System.out.println("Calling setter took (" + (t1 - t0) + ") milliseconds.");
  }

  private static void processBySetterWithEventNotification() {
    final ValueHolder<Integer> valueHolder = new ValueHolder<>();
    final long t0 = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setNotificationValue(count);
      Assert.assertEquals(count, valueHolder.getNotificationValue().intValue());
    }

    final long t1 = System.currentTimeMillis();

    System.out.println("Calling setter with event notification took (" + (t1 - t0) + ") milliseconds.");
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

    public void setCallbackValue(final T callbackValue) {
      processChange("callbackValue", this.callbackValue, callbackValue, () -> ValueHolder.this.callbackValue = callbackValue);
    }

    public T getNotificationValue() {
      return notificationValue;
    }

    public void setNotificationValue(final T notificationValue) {
      try {
        final PropertyChangeEvent event = createPropertyChangeEvent("notificationValue", this.notificationValue, notificationValue);
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

    public void setReflectionValue(final T reflectionValue) {
      processChange("reflectionValue", this.reflectionValue, reflectionValue);
    }

    public T getValue() {
      return value;
    }

    public void setValue(final T value) {
      this.value = value;
    }
  }

}
