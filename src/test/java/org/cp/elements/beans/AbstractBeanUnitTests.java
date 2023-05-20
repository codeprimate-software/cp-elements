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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.fail;
import static org.cp.elements.lang.LangExtensions.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyVetoException;
import java.beans.VetoableChangeListener;
import java.io.FileNotFoundException;
import java.time.Instant;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import org.cp.elements.beans.AbstractBean.StateChangeCallback;
import org.cp.elements.beans.event.ChangeEvent;
import org.cp.elements.beans.event.ChangeListener;
import org.cp.elements.beans.model.BeanAdapter;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.lang.ThrowableOperation;
import org.cp.elements.lang.Visitor;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.reflect.FieldNotFoundException;
import org.cp.elements.security.model.User;
import org.mockito.InOrder;
import org.mockito.quality.Strictness;
import org.mockito.stubbing.Answer;

/**
 * Unit Tests for {@link AbstractBean}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.AbstractBean
 * @since 1.0.0
 */
public class AbstractBeanUnitTests {

  @SuppressWarnings("unchecked")
  private static @NotNull User<Long> mockUser(@Nullable String name) {

    User<Long> mockUser = mock(User.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(System.currentTimeMillis()).when(mockUser).getId();
    doReturn(name).when(mockUser).getName();

    return mockUser;
  }

  @Test
  public void constructAbstractBean() {

    TestBean<Integer> bean = new TestBean<>();

    assertThat(bean).isNotNull();
    assertThat(bean.getId()).isNull();
    assertThat(bean.isNew()).isTrue();
    assertThat(bean.isModified()).isFalse();
    assertThat(bean.isEventDispatchEnabled()).isTrue();
  }

  @Test
  public void constructAbstractBeanWithId() {

    TestBean<Integer> bean = new TestBean<>(2);

    assertThat(bean).isNotNull();
    assertThat(bean.getId()).isEqualTo(2);
    assertThat(bean.isNew()).isFalse();
    assertThat(bean.isModified()).isTrue();
    assertThat(bean.isEventDispatchEnabled()).isTrue();
  }

  @Test
  public void getAdapter() {

    TestBean<Integer> bean = new TestBean<>(1);

    BeanAdapter beanAdapter = bean.getAdapter();

    assertThat(beanAdapter).isNotNull();
    assertThat(beanAdapter.getTarget()).isSameAs(bean);
    assertThat(bean.getAdapter()).isSameAs(beanAdapter);
  }

  @Test
  public void setAndGetId() {

    TestBean<Integer> bean = new TestBean<>(null);

    assertThat(bean).isNotNull();
    assertThat(bean.getId()).isNull();
    assertThat(bean.isNew()).isTrue();
    assertThat(bean.isModified()).isFalse();

    bean.setId(8);

    assertThat(bean.getId()).isEqualTo(8);
    assertThat(bean.isNotNew()).isTrue();
    assertThat(bean.isModified()).isTrue();

    bean.setId(null);

    assertThat(bean.getId()).isNull();
    assertThat(bean.isNew()).isTrue();
    assertThat(bean.isModified()).isFalse();

    assertThat(bean.<Bean<Integer, User<Long>, Object>>identifiedBy(2)).isSameAs(bean);
    assertThat(bean.getId()).isEqualTo(2);
    assertThat(bean.isNotNew()).isTrue();
    assertThat(bean.isModified()).isTrue();
  }

  @Test
  public void setAndGetEventDispatchEnabled() {

    TestBean<Integer> bean = new TestBean<>();

    assertThat(bean).isNotNull();
    assertThat(bean.isEventDispatchEnabled()).isTrue();

    bean.setEventDispatchEnabled(false);

    assertThat(bean.isEventDispatchEnabled()).isFalse();

    bean.setEventDispatchEnabled(true);

    assertThat(bean.isEventDispatchEnabled()).isTrue();
  }

  @Test
  public void setAndGetAuditableFields() {

    User<Long> jonDoe = mockUser("jonDoe");
    User<Long> pieDoe = mockUser("pieDoe");
    User<Long> sourDoe = mockUser("sourDoe");

    Object process = AbstractBeanUnitTests.class;

    Instant now = Instant.now();
    Instant beforeNow = now.minusSeconds(5);
    Instant afterNow = now.plusSeconds(5);

    TestBean<Integer> bean = new TestBean<>();

    bean.setCreatedBy(jonDoe);
    bean.setCreatedOn(beforeNow);
    bean.setCreatedWith(process);
    bean.setModifiedBy(pieDoe);
    bean.setModifiedOn(now);
    bean.setModifiedWith(process);

    assertThat(bean.getCreatedBy()).isEqualTo(jonDoe);
    assertThat(bean.getCreatedOn()).isEqualTo(beforeNow);
    assertThat(bean.getCreatedWith()).isEqualTo(process);
    assertThat(bean.getModifiedBy()).isEqualTo(pieDoe);
    assertThat(bean.getModifiedOn()).isEqualTo(now);
    assertThat(bean.getModifiedWith()).isEqualTo(process);
    assertThat(bean.getLastModifiedBy()).isEqualTo(pieDoe);
    assertThat(bean.getLastModifiedOn()).isEqualTo(now);
    assertThat(bean.getLastModifiedWith()).isEqualTo(process);

    bean.setModifiedBy(sourDoe);
    bean.setModifiedOn(afterNow);
    bean.setModifiedWith(process);

    assertThat(bean.getCreatedBy()).isEqualTo(jonDoe);
    assertThat(bean.getCreatedOn()).isEqualTo(beforeNow);
    assertThat(bean.getCreatedWith()).isEqualTo(process);
    assertThat(bean.getModifiedBy()).isEqualTo(sourDoe);
    assertThat(bean.getModifiedOn()).isEqualTo(afterNow);
    assertThat(bean.getModifiedWith()).isEqualTo(process);
    assertThat(bean.getLastModifiedBy()).isEqualTo(pieDoe);
    assertThat(bean.getLastModifiedOn()).isEqualTo(now);
    assertThat(bean.getLastModifiedWith()).isEqualTo(process);
  }

  @Test
  public void isModified() {

    ValueHolder valueHolder = new ValueHolder("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.isModified()).isFalse();

    valueHolder.setValue("test");

    assertThat(valueHolder.isModified()).isFalse();
    assertThat(valueHolder.isModified("id")).isFalse();
    assertThat(valueHolder.isModified("value")).isFalse();

    valueHolder.setValue("TEST");

    assertThat(valueHolder.isModified()).isTrue();
    assertThat(valueHolder.isModified("id")).isFalse();
    assertThat(valueHolder.isModified("value")).isTrue();

    valueHolder.setValue("mock");

    assertThat(valueHolder.isModified()).isTrue();
    assertThat(valueHolder.isModified("id")).isFalse();
    assertThat(valueHolder.isModified("value")).isTrue();

    valueHolder.setValue("test");

    assertThat(valueHolder.isModified()).isFalse();
    assertThat(valueHolder.isModified("id")).isFalse();
    assertThat(valueHolder.isModified("value")).isFalse();

    valueHolder.setValue(null);

    assertThat(valueHolder.isModified()).isTrue();
    assertThat(valueHolder.isModified("id")).isFalse();
    assertThat(valueHolder.isModified("value")).isTrue();
  }

  @Test
  public void acceptsVisitor() {

    Visitor mockVisitor = mock(Visitor.class);

    TestBean<Integer> bean = new TestBean<>();

    bean.accept(mockVisitor);

    verify(mockVisitor, times(1)).visit(eq(bean));
    verifyNoMoreInteractions(mockVisitor);
  }

  @Test
  public void changeStateIsSuccessful() {

    ValueHolder valueHolder = new ValueHolder("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");

    valueHolder.changeState("value", "mock");

    assertThat(valueHolder.getValue()).isEqualTo("mock");
  }

  @Test
  public void changeStateForPropertyMappedFieldIsSuccessful() {

    ValueHolder valueHolder = new ValueHolder("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");
    assertThat(valueHolder.mapPropertyNameToFieldName("nonExistingProperty", "value")).isNull();
    assertThat(valueHolder.getFieldName("nonExistingProperty")).isEqualTo("value");

    valueHolder.changeState("nonExistingProperty", "mock");

    assertThat(valueHolder.getValue()).isEqualTo("mock");
  }

  @Test
  public void changeStateForPropertyWithUnmappedFieldThrowsException() {

    ValueHolder valueHolder = new ValueHolder("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");
    assertThat(valueHolder.getFieldName("nonExistingProperty")).isEqualTo("nonExistingProperty");

    ThrowableAssertions.assertThatThrowableOfType(FileNotFoundException.class)
      .isThrownBy(ThrowableOperation.fromRunnable(() ->
        valueHolder.changeState("nonExistingProperty", "mock")))
      .havingMessage("No field [nonExistingProperty] for property [nonExistingProperty] was found on this Bean [%s]",
          valueHolder.getClass().getName())
      .causedBy(IllegalArgumentException.class)
      .causedBy(FieldNotFoundException.class)
      .causedBy(NoSuchFieldException.class)
      .withNoCause();

    assertThat(valueHolder.getValue()).isEqualTo("test");
  }

  @Test
  public void firePropertyChangeEventWithNullEvent() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestBean<>().firePropertyChangeEvent(null))
      .withMessage("PropertyChangeEvent is required")
      .withNoCause();
  }

  @Test
  public void fireVetoableChangeEventWithNullEvent() {

    assertThatIllegalArgumentException()
      .isThrownBy(() ->new TestBean<>().fireVetoableChangeEvent(null))
      .withMessage("PropertyChangeEvent is required")
      .withNoCause();
  }

  @Test
  public void mapAndUnmapPropertyNameToFieldName() {

    TestBean<Integer> bean = new TestBean<>();

    assertThat(bean.mapPropertyNameToFieldName("testProperty", "testField")).isNull();
    assertThat(bean.getFieldName("testProperty")).isEqualTo("testField");
    assertThat(bean.getFieldName("mockProperty")).isEqualTo("mockProperty");
    assertThat(bean.unmapPropertyNameToFieldName("testProperty")).isEqualTo("testField");
    assertThat(bean.getFieldName("testProperty")).isEqualTo("testProperty");
    assertThat(bean.getFieldName("mockProperty")).isEqualTo("mockProperty");
  }

  @Test
  public void mapIllegalPropertyNameToFieldName() {

    Arrays.asList("  ", "", null).forEach(propertyName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> new ValueHolder().mapPropertyNameToFieldName(propertyName, "value"))
        .withMessage("Property name [%s] is required", propertyName)
        .withNoCause());
  }

  @Test
  public void mapPropertyNameToIllegalFieldName() {

    Arrays.asList("  ", "", null).forEach(fieldName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> new ValueHolder().mapPropertyNameToFieldName("alias", fieldName))
        .withMessage("Field name [%s] is required", fieldName)
        .withNoCause());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void mapAndUnmapPropertyNameToStateChangeCallback() {

    StateChangeCallback<Object> mockCallback = mock(StateChangeCallback.class);

    ValueHolder valueHolder = new ValueHolder();

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getAlias()).isNull();
    assertThat(valueHolder.getValue()).isNull();
    assertThat(valueHolder.mapPropertyNameToStateChangeCallback("alias", mockCallback)).isNull();

    valueHolder.setAlias("test");

    assertThat(valueHolder.getAlias()).isNull();
    assertThat(valueHolder.getValue()).isNull();
    assertThat(valueHolder.unmapPropertyNameToStateChangeCallback("alias")).isEqualTo(mockCallback);

    verify(mockCallback, times(1)).changeState(eq("test"));
    verifyNoMoreInteractions(mockCallback);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void mapIllegalPropertyNameToStateChangeCallback() {

    StateChangeCallback<Object> mockStateChangeCallback = mock(StateChangeCallback.class);

    Arrays.asList("  ", "", null).forEach(propertyName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> new ValueHolder().mapPropertyNameToStateChangeCallback(propertyName, mockStateChangeCallback))
        .withMessage("Property name [%s] is required", propertyName)
        .withNoCause());

    verifyNoInteractions(mockStateChangeCallback);
  }

  @Test
  public void mapPropertyNameToNullStateChangeCallback() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new ValueHolder().mapPropertyNameToStateChangeCallback("alias", null))
      .withMessage("The StateChangeCallback to map to property [alias] is required")
      .withNoCause();
  }

  @Test
  public void newPropertyChangeEventWasInitialized() {

    TestBean<?> bean = new TestBean<>();

    assertThat(bean).isNotNull();

    PropertyChangeEvent event =
      bean.newPropertyChangeEvent("testProperty", "test", "mock");

    assertThat(event).isNotNull();
    assertThat(event.getSource()).isSameAs(bean);
    assertThat(event.getPropertyName()).isEqualTo("testProperty");
    assertThat(event.getOldValue()).isEqualTo("test");
    assertThat(event.getNewValue()).isEqualTo("mock");
  }

  @Test
  public void setPropertyForUnmappedFieldAndUnmappedStateChangeCallback() {

    ValueHolder valueHolder = new ValueHolder("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");

    ThrowableAssertions.assertThatThrowableOfType(FileNotFoundException.class)
      .isThrownBy(ThrowableOperation.fromRunnable(() -> valueHolder.setAlias("mock")))
      .havingMessage("No field [alias] for property [alias] was found on this Bean [%s]",
        valueHolder.getClass().getName())
      .causedBy(IllegalArgumentException.class)
      .withNoCause();

    assertThat(valueHolder.getValue()).isEqualTo("test");
  }

  @Test
  public void setPropertyWhenPropertyNameIsMappedToFieldName() {

    ValueHolder valueHolder = spy(new ValueHolder("test"));

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");
    assertThat(valueHolder.mapPropertyNameToFieldName("alias", "value")).isNull();
    assertThat(valueHolder.getFieldName("alias")).isEqualTo("value");

    valueHolder.setAlias("mock");

    assertThat(valueHolder.getValue()).isEqualTo("mock");

    verify(valueHolder, times(1)).setAlias(eq("mock"));
    verify(valueHolder, times(1)).changeState(eq("alias"), eq("mock"));
    verify(valueHolder, times(3)).getValue();
    verify(valueHolder, never()).setValue(any());
  }

  @Test
  public void setPropertyWhenPropertyNameIsMappedToStateChangeCallback() {

    ValueHolder valueHolder = spy(new ValueHolder("test"));

    StateChangeCallback<Object> stateChangeCallback = newValue -> valueHolder.value = newValue;

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");
    assertThat(valueHolder.mapPropertyNameToStateChangeCallback("alias", stateChangeCallback)).isNull();
    assertThat(valueHolder.getFieldName("alias")).isEqualTo("alias");

    valueHolder.setAlias("mock");

    assertThat(valueHolder.getValue()).isEqualTo("mock");

    verify(valueHolder, times(1)).setAlias(eq("mock"));
    verify(valueHolder, times(3)).getValue();
    verify(valueHolder, never()).changeState(anyString(), any());
    verify(valueHolder, never()).setValue(eq("mock"));
  }

  @Test
  public void propertyChangeFiresAllRegisteredEventListeners() throws PropertyVetoException {

    Instant beforeEventDateTime = Instant.now();

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    PropertyChangeListener mockPropertyChangeListener = mock(PropertyChangeListener.class);

    VetoableChangeListener mockVetoableChangeListener = mock(VetoableChangeListener.class);

    ValueHolder valueHolder = new ValueHolder("test");
    ValueHolder valueHolderSpy = spy(valueHolder);

    assertThat(valueHolderSpy).isNotNull();
    assertThat(valueHolderSpy.getId()).isNull();
    assertThat(valueHolderSpy.getValue()).isEqualTo("test");

    valueHolderSpy.register(mockChangeListener);
    valueHolderSpy.register(mockPropertyChangeListener);
    valueHolderSpy.register(mockVetoableChangeListener);

    doAnswer(invocation -> {

      ChangeEvent event = invocation.getArgument(0);

      assertThat(event).isNotNull();
      assertThat(event.getSource()).isEqualTo(valueHolder);
      assertThat(event.getChangeDateTime()).isAfterOrEqualTo(beforeEventDateTime);

      return null;

    }).when(mockChangeListener).stateChanged(any(ChangeEvent.class));

    Answer<Void> propertyChangeEventHandler = invocation -> {

      PropertyChangeEvent event = invocation.getArgument(0);

      assertThat(event).isNotNull();
      assertThat(event.getSource()).isEqualTo(valueHolderSpy);
      assertThat(event.getPropertyName()).isEqualTo("value");
      assertThat(event.getOldValue()).isEqualTo("test");
      assertThat(event.getNewValue()).isEqualTo("mock");

      return null;

    };

    doAnswer(propertyChangeEventHandler).when(mockPropertyChangeListener).propertyChange(any(PropertyChangeEvent.class));
    doAnswer(propertyChangeEventHandler).when(mockVetoableChangeListener).vetoableChange(any(PropertyChangeEvent.class));

    valueHolderSpy.setValue("mock");

    assertThat(valueHolderSpy.getValue()).isEqualTo("mock");

    InOrder order = inOrder(valueHolderSpy, mockChangeListener, mockPropertyChangeListener, mockVetoableChangeListener);

    order.verify(valueHolderSpy, times(1)).setValue(eq("mock"));
    order.verify(valueHolderSpy, times(1)).processChange(eq("value"), eq("test"), eq("mock"));
    order.verify(mockVetoableChangeListener, times(1)).vetoableChange(isA(PropertyChangeEvent.class));
    order.verify(valueHolderSpy, times(1)).changeState(eq("value"), eq("mock"));
    order.verify(mockPropertyChangeListener, times(1)).propertyChange(isA(PropertyChangeEvent.class));
    order.verify(mockChangeListener, times(1)).stateChanged(isA(ChangeEvent.class));

    verifyNoMoreInteractions(mockChangeListener, mockPropertyChangeListener, mockVetoableChangeListener);
  }

  @Test
  public void propertyChangeFiresNoRegisteredEventListenersWhenEventDispatchIsDisabled() {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    PropertyChangeListener mockPropertyChangeListener = mock(PropertyChangeListener.class);

    VetoableChangeListener mockVetoableChangeListener = mock(VetoableChangeListener.class);

    ValueHolder valueHolder = spy(new ValueHolder("test"));

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getId()).isNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");

    valueHolder.setEventDispatchEnabled(false);
    valueHolder.register(mockChangeListener);
    valueHolder.register(mockPropertyChangeListener);
    valueHolder.register(mockVetoableChangeListener);

    assertThat(valueHolder.isEventDispatchEnabled()).isFalse();

    valueHolder.setValue("mock");

    assertThat(valueHolder.getValue()).isEqualTo("mock");

    verify(valueHolder, times(1)).setValue(eq("mock"));
    verify(valueHolder, times(1)).processChange(eq("value"), eq("test"), eq("mock"));
    verifyNoInteractions(mockChangeListener, mockPropertyChangeListener, mockVetoableChangeListener);
  }

  @Test
  public void propertyChangeFiresPropertySpecificEventListenersBasedOnRegistration() throws PropertyVetoException {

    Instant beforeEventDateTime = Instant.now();

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    PropertyChangeListener mockPropertyChangeListener = mock(PropertyChangeListener.class);

    VetoableChangeListener mockVetoableChangeListener = mock(VetoableChangeListener.class);

    ValueHolder valueHolder = new ValueHolder("test").identifiedBy(1);

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getId()).isEqualTo(1);
    assertThat(valueHolder.getValue()).isEqualTo("test");
    assertThat(valueHolder.isEventDispatchEnabled()).isTrue();

    valueHolder.register(mockChangeListener);
    valueHolder.register("id", mockVetoableChangeListener);
    valueHolder.register("value", mockPropertyChangeListener);

    doAnswer(invocation -> {

      ChangeEvent event = invocation.getArgument(0);

      assertThat(event).isNotNull();
      assertThat(event.getSource()).isEqualTo(valueHolder);
      assertThat(event.getChangeDateTime()).isAfterOrEqualTo(beforeEventDateTime);

      return null;

    }).when(mockChangeListener).stateChanged(any(ChangeEvent.class));

    doAnswer(invocation -> {

      PropertyChangeEvent event = invocation.getArgument(0);

      assertThat(event).isNotNull();
      assertThat(event.getSource()).isEqualTo(valueHolder);
      assertThat(event.getPropertyName()).isEqualTo("value");

      return null;

    }).when(mockPropertyChangeListener).propertyChange(any(PropertyChangeEvent.class));

    doAnswer(invocation -> {

      PropertyChangeEvent event = invocation.getArgument(0);

      assertThat(event).isNotNull();
      assertThat(event.getSource()).isEqualTo(valueHolder);
      assertThat(event.getPropertyName()).isEqualTo("id");

      Integer newId = (Integer) event.getNewValue();

      if (is(newId).not().greaterThan(0)) {
        throw new PropertyVetoException(String.format("ID [%s] must be greater than 0", newId), event);
      }

      return null;

    }).when(mockVetoableChangeListener).vetoableChange(any(PropertyChangeEvent.class));

    valueHolder.setId(2);

    assertThat(valueHolder.getId()).isEqualTo(2);
    assertThat(valueHolder.getValue()).isEqualTo("test");

    verify(mockVetoableChangeListener, times(1)).vetoableChange(isA(PropertyChangeEvent.class));
    verify(mockChangeListener, times(1)).stateChanged(isA(ChangeEvent.class));
    verifyNoMoreInteractions(mockChangeListener, mockVetoableChangeListener);
    verifyNoInteractions(mockPropertyChangeListener);
    reset(mockChangeListener, mockPropertyChangeListener, mockVetoableChangeListener);

    valueHolder.setValue("mock");

    assertThat(valueHolder.getId()).isEqualTo(2);
    assertThat(valueHolder.getValue()).isEqualTo("mock");

    verify(mockPropertyChangeListener, times(1)).propertyChange(isA(PropertyChangeEvent.class));
    verify(mockChangeListener, times(1)).stateChanged(isA(ChangeEvent.class));
    verifyNoMoreInteractions(mockChangeListener, mockPropertyChangeListener);
    verifyNoInteractions(mockVetoableChangeListener);
  }

  @Test
  public void propertyChangeIsVetoed() throws PropertyVetoException {

    ChangeListener mockChangeListener = mock(ChangeListener.class);

    PropertyChangeListener mockPropertyChangeListener = mock(PropertyChangeListener.class);

    VetoableChangeListener mockVetoableChangeListener = mock(VetoableChangeListener.class);

    ValueHolder valueHolder = new ValueHolder("test");

    assertThat(valueHolder).isNotNull();
    assertThat(valueHolder.getValue()).isEqualTo("test");

    valueHolder.register(mockChangeListener);
    valueHolder.register(mockPropertyChangeListener);
    valueHolder.register(mockVetoableChangeListener);

    doAnswer(invocation -> { throw new PropertyVetoException("New value is invalid", invocation.getArgument(0)); })
      .when(mockVetoableChangeListener).vetoableChange(any(PropertyChangeEvent.class));

    try {
      valueHolder.setValue("mock");
      fail("Expected %s", IllegalPropertyValueException.class.getName());
    }
    catch (IllegalPropertyValueException expected) {

      assertThat(expected).hasMessage("The new value [mock] for property [value] of Bean [%s] is not valid",
        valueHolder.getClass().getName());

      assertThat(expected).hasCauseInstanceOf(PropertyVetoException.class);

      PropertyVetoException exception = (PropertyVetoException) expected.getCause();

      assertThat(exception.getMessage()).isEqualTo("New value is invalid");

      PropertyChangeEvent event = exception.getPropertyChangeEvent();

      assertThat(event).isNotNull();
      assertThat(event.getSource()).isEqualTo(valueHolder);
      assertThat(event.getPropertyName()).isEqualTo("value");
      assertThat(event.getOldValue()).isEqualTo("test");
      assertThat(event.getNewValue()).isEqualTo("mock");
    }
    finally {

      assertThat(valueHolder.getValue()).isEqualTo("test");

      verify(mockVetoableChangeListener, times(1)).vetoableChange(isA(PropertyChangeEvent.class));
      verifyNoMoreInteractions(mockVetoableChangeListener);
      verifyNoInteractions(mockChangeListener, mockPropertyChangeListener);
    }
  }

  @Test
  @SuppressWarnings({ "all", "unchecked" })
  public void compareToNonAbstractBean() {

    Bean mockBean = mock(Bean.class);

    assertThatExceptionOfType(ClassCastException.class)
      .isThrownBy(() -> new TestBean<Integer>().compareTo(mockBean))
      .withMessage("The Bean to compare with this Bean must be an instance of [%s]",
        TestBean.class.getName())
      .withNoCause();

    verifyNoInteractions(mockBean);
  }

  @Test
  public void compareToEqualBean() {

    TestBean<Integer> beanOne = new TestBean<>(1);
    TestBean<Integer> beanTwo = new TestBean<>(1);

    assertThat(beanOne.compareTo(beanTwo)).isZero();
  }

  @Test
  public void compareToGreaterBean() {

    TestBean<Integer> beanOne = new TestBean<>(1);
    TestBean<Integer> beanTwo = new TestBean<>(2);

    assertThat(beanOne.compareTo(beanTwo)).isLessThan(0);
  }

  @Test
  public void compareToLesserBean() {

    TestBean<Integer> beanOne = new TestBean<>(1);
    TestBean<Integer> beanTwo = new TestBean<>(2);

    assertThat(beanTwo.compareTo(beanOne)).isGreaterThan(0);
  }

  @Test
  @SuppressWarnings("all")
  public void compareToSameBean() {

    TestBean<Integer> bean = new TestBean<>(0);

    assertThat(bean.compareTo(bean)).isZero();
  }

  @Test
  public void equalsWithEqualBean() {

    ValueHolder testValueOne = new ValueHolder("test");
    ValueHolder testValueTwo = new ValueHolder("test");

    assertThat(testValueOne.equals(testValueTwo)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsWithSameBean() {

    ValueHolder value = new ValueHolder("test");

    assertThat(value.equals(value)).isTrue();
  }

  @Test
  public void equalsWithUnequalBean() {

    ValueHolder testValue = new ValueHolder("test");
    ValueHolder mockValue = new ValueHolder("mock");

    assertThat(testValue.equals(mockValue)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsWithNonBeanIsFalse() {
    assertThat(new ValueHolder("test").equals("test")).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsWithNullIsFalse() {
    assertThat(new ValueHolder("test").equals(null)).isFalse();
  }

  @Test
  public void hashCodeIsNotZero() {

    assertThat(new TestBean<Integer>().hashCode()).isNotZero();
    assertThat(new ValueHolder("test").hashCode()).isNotZero();
  }

  @Test
  public void hashCodeForEqualBeansIsEqual() {

    TestBean<Integer> beanOne = new TestBean<>(1);
    TestBean<Integer> beanTwo = new TestBean<>(1);

    assertThat(beanOne.hashCode()).isEqualTo(beanTwo.hashCode());
  }

  @Test
  public void hashCodeForSameBeanIsEqual() {

    TestBean<Integer> bean = new TestBean<>(0);

    assertThat(bean.hashCode()).isEqualTo(bean.hashCode());
  }

  @Test
  public void hashCodeForUnequalsBeansIsNotEqual() {

    TestBean<Integer> beanOne = new TestBean<>(1);
    TestBean<Integer> beanTwo = new TestBean<>(2);

    assertThat(beanOne.hashCode()).isNotEqualTo(beanTwo.hashCode());
  }

  private static final class TestBean<ID extends Comparable<ID>> extends AbstractBean<ID, User<Long>, Object> {

    public TestBean() { }

    public TestBean(ID id) {
      super(id);
    }
  }

  @SuppressWarnings("unused")
  private static class ValueHolder extends AbstractBean<Integer, User<Long>, Object> {

    private Object value;

    public ValueHolder() { }

    public ValueHolder(@Nullable Object value) {
      this.value = value;
    }

    public @Nullable Object getAlias() {
      return getValue();
    }

    public void setAlias(Object value) {
      processChange("alias", getValue(), value);
    }

    public @Nullable Object getValue() {
      return this.value;
    }

    public void setValue(@Nullable Object value) {
      processChange("value", getValue(), value);
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof ValueHolder that)) {
        return false;
      }

      return ObjectUtils.equals(this.getValue(), that.getValue());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getValue());
    }

    @Override
    public String toString() {
      return String.format("ValueHolder [%s]", getValue());
    }
  }
}
