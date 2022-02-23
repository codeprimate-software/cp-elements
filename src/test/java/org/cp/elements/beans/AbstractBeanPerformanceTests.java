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

import static org.assertj.core.api.Assertions.assertThat;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Objects;
import java.util.Stack;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.cp.elements.beans.event.ChangeEvent;
import org.cp.elements.beans.event.ChangeListener;
import org.cp.elements.security.model.User;

/**
 * Performance Tests for {@link AbstractBean}.
 *
 * @author John J. Blum
 * @see org.cp.elements.beans.AbstractBean
 * @since 1.0.0
 */
public class AbstractBeanPerformanceTests {

  private static final int NUMBER_OF_OPERATIONS = 1_000_000;

  public static void main(String... args) {

    testPerformanceOfSetValueUsingLambda(new ValueHolder<>());
    testPerformanceOfSetValueUsingMethodHandle(new ValueHolder<>());
    testPerformanceOfSetValueUsingReflection(new ValueHolder<>());
    testPerformanceOfSetValueUsingSetter(new ValueHolder<>());
    testPerformanceOfSetValueWithEventNotification(new ValueHolder<>());

  }

  private static long measurePerformanceInMilliseconds(
      Consumer<Integer> valueHolderSetter, Supplier<Integer> valueHolderGetter) {

    long timeBegin = System.currentTimeMillis();

    for (int count = 0; count < NUMBER_OF_OPERATIONS; count++) {
      valueHolderSetter.accept(count);
      assertThat(valueHolderGetter.get()).isEqualTo(count);
    }

    long timeEnd = System.currentTimeMillis();

    return timeEnd - timeBegin;
  }

  private static void testPerformanceOfSetValueUsingLambda(ValueHolder<Integer> valueHolder) {

    long milliseconds =
      measurePerformanceInMilliseconds(valueHolder::setValueUsingLambda, valueHolder::getValue);

    System.out.printf("Set with callback using Lambda took [%d] milliseconds.%n", milliseconds);
  }

  private static void testPerformanceOfSetValueUsingMethodHandle(ValueHolder<Integer> valueHolder) {

    long milliseconds =
      measurePerformanceInMilliseconds(valueHolder::setValueUsingMethodHandle, valueHolder::getValue);

    System.out.printf("Set with callback using method handle took [%d] milliseconds.%n", milliseconds);
  }

  private static void testPerformanceOfSetValueUsingReflection(ValueHolder<Integer> valueHolder) {

    long milliseconds =
      measurePerformanceInMilliseconds(valueHolder::setValueUsingReflection, valueHolder::getValue);

    System.out.printf("Set with reflection took [%d] milliseconds.%n", milliseconds);
  }

  private static void testPerformanceOfSetValueUsingSetter(ValueHolder<Integer> valueHolder) {

    long milliseconds =
      measurePerformanceInMilliseconds(valueHolder::setValue, valueHolder::getValue);

    System.out.printf("Calling setter took [%d] milliseconds.%n", milliseconds);
  }

  private static void testPerformanceOfSetValueWithEventNotification(ValueHolder<Integer> valueHolder) {

    long milliseconds =
      measurePerformanceInMilliseconds(valueHolder::setValueWithEventNotification, valueHolder::getValue);

    System.out.printf("Calling setter with event notification took [%d] milliseconds.", milliseconds);
  }

  @SuppressWarnings("unused")
  private static final class ValueHolder<T> extends AbstractBean<Long, User<Integer>, Object> {

    private T value;
    private T valueCopy;

    public ValueHolder() {

      setEventDispatchEnabled(false);
      register(LoggingChangeListener.INSTANCE);
      register("value", CopyPropertyChangeListener.INSTANCE);
      register("value", RequiredVetoableChangeListener.INSTANCE);
    }

    public T getValue() {
      return this.value;
    }

    public void setValue(T value) {
      this.value = value;
    }

    @SuppressWarnings("all")
    public void setValueUsingLambda(T value) {
      processChange("value", getValue(), value, newValue -> setValue(newValue));
    }

    public void setValueUsingMethodHandle(T value) {
      processChange("value", getValue(), value, this::setValue);
    }

    public void setValueUsingReflection(T value) {
      processChange("value", getValue(), value);
    }

    public void setValueWithEventNotification(T value) {

      try {
        setEventDispatchEnabled(true);
        processChange("value", getValue(), value, this::setValue);
      }
      finally {
        setEventDispatchEnabled(false);
      }
    }

    public T getValueCopy() {
      return this.valueCopy;
    }

    public void setValueCopy(T valueCopy) {
      this.valueCopy = valueCopy;
    }
  }

  private static final class LoggingChangeListener implements ChangeListener {

    private static final LoggingChangeListener INSTANCE = new LoggingChangeListener();

    private final DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd@hh:mm:ss");

    private final Stack<String> logMessages = new Stack<>();

    @SuppressWarnings("unused")
    public String getLastLogMessage() {
      return this.logMessages.peek();
    }

    @Override
    public void stateChanged(ChangeEvent event) {
      this.logMessages.push(String.format("Source [%s] was changed on [%s]", event.getSource().getClass().getName(),
        event.getChangeDateTime().atZone(ZoneOffset.systemDefault()).format(this.dateTimeFormatter)));
    }
  }

  private static final class CopyPropertyChangeListener implements PropertyChangeListener {

    private static final CopyPropertyChangeListener INSTANCE = new CopyPropertyChangeListener();

    @Override
    @SuppressWarnings("unchecked")
    public void propertyChange(PropertyChangeEvent event) {

      Object source = event.getSource();

      if (source instanceof ValueHolder) {
        ((ValueHolder<Object>) source).setValueCopy(event.getNewValue());
      }
    }
  }

  private static final class RequiredVetoableChangeListener implements VetoableChangeListener {

    private static final RequiredVetoableChangeListener INSTANCE = new RequiredVetoableChangeListener();

    @Override
    public void vetoableChange(PropertyChangeEvent event) throws PropertyVetoException {

      if (Objects.isNull(event.getNewValue())) {
        String message = String.format("Property [%s] must not be null", event.getPropertyName());
        throw new PropertyVetoException(message, event);
      }
    }
  }
}
