/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.beans;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import org.junit.Assert;

/**
 * The AbstractBeanPerformanceTest class is a test suite for measuring and collection runtime performance metrics
 * for the AbstractBean class.
 * </p>
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
    final ValueHolder<Integer> valueHolder = new ValueHolder<Integer>();
    final long t0 = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setCallbackValue(count);
      Assert.assertEquals(count, valueHolder.getCallbackValue().intValue());
    }

    final long t1 = System.currentTimeMillis();

    System.out.println("Setting through callback took (" + (t1 - t0) + ") milliseconds.");
  }

  private static void processByReflection() {
    final ValueHolder<Integer> valueHolder = new ValueHolder<Integer>();
    final long t0 = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setReflectionValue(count);
      Assert.assertEquals(count, valueHolder.getReflectionValue().intValue());
    }

    final long t1 = System.currentTimeMillis();

    System.out.println("Setting through reflection took (" + (t1 - t0) + ") milliseconds.");
  }

  private static void processBySetter() {
    final ValueHolder<Integer> valueHolder = new ValueHolder<Integer>();
    final long t0 = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolder.setValue(count);
      Assert.assertEquals(count, valueHolder.getValue().intValue());
    }

    final long t1 = System.currentTimeMillis();

    System.out.println("Calling setter took (" + (t1 - t0) + ") milliseconds.");
  }

  private static void processBySetterWithEventNotification() {
    final ValueHolder<Integer> valueHolder = new ValueHolder<Integer>();
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
      processChange("callbackValue", this.callbackValue, callbackValue, new StateChangeCallback() {
        public void changeState() {
          ValueHolder.this.callbackValue = callbackValue;
        }
      });
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
