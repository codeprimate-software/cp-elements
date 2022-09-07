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
package org.cp.elements.beans.event.support;

import static org.cp.elements.lang.ThrowableAssertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyVetoException;
import java.time.Instant;

import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.beans.PropertyNotSetException;
import org.cp.elements.beans.annotation.Required;
import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.lang.ThrowableOperation;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.security.model.User;
import org.cp.elements.test.annotation.SubjectUnderTest;

import org.junit.Test;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link RequiredPropertyVetoableChangeListener}.
 *
 * @author John Blum
 * @see java.beans.PropertyChangeEvent
 * @see java.beans.VetoableChangeListener
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.event.support.RequiredPropertyVetoableChangeListener
 * @since 1.0.0
 */
public class RequiredPropertyVetoableChangeListenerUnitTests {

  @SubjectUnderTest
  private final RequiredPropertyVetoableChangeListener listener = new RequiredPropertyVetoableChangeListener();

  @Test
  public void doHandleRequiresRequiredPropertyToBeSet() throws PropertyVetoException {

    User<Integer> user = RequiredNameFieldUser.as("jonDoe");

    PropertyChangeEvent event =
      BeanUtils.newPropertyChangeEvent(user, "name", "jonDoe", "jackHandy");

    this.listener.vetoableChange(event);
  }

  @Test
  public void doHandleRequiresRequiredInheritedPropertyToBeSet() {

    PropertyChangeEvent event =
      BeanUtils.newPropertyChangeEvent(RequiredNameFieldUser.as("froDoe"), "lastAccess", Instant.now(), null);

    assertThatThrowableOfType(PropertyVetoException.class)
      .isThrownBy(ThrowableOperation.from(args -> this.listener.vetoableChange(event)))
      .havingMessage("Failed to process event [%s]", event)
      .causedBy(PropertyNotSetException.class)
      .havingMessage("Property [lastAccess] not set")
      .withNoCause();
  }

  @Test
  public void doHandleDoesNotRequireNonRequiredProperty() throws PropertyVetoException {

    PropertyChangeEvent event =
      BeanUtils.newPropertyChangeEvent(RequiredNameFieldUser.as("dillDoe"), "id", 1, null);

    this.listener.vetoableChange(event);
  }

  @Test
  public void doHandleWithNonExistingProperty() {

    PropertyChangeEvent event =
      spy(BeanUtils.newPropertyChangeEvent(mock(User.class), "mockProperty", 1, 2));

    assertThatThrowableOfType(PropertyVetoException.class)
      .isThrownBy(ThrowableOperation.from(args -> this.listener.vetoableChange(event)))
      .havingMessage("Failed to process event [%s]", event)
      .causedBy(PropertyNotFoundException.class)
      .havingMessage("Property with name [mockProperty] not found")
      .withNoCause();

    verify(event, atLeastOnce()).getPropertyName();
    verify(event, atLeastOnce()).getSource();
  }

  @Test
  public void doHandleWithNullSource() {

    PropertyChangeEvent event =
      spy(new PropertyChangeEvent(new Object(), "mockProperty", 1, 2));

    doReturn(null).when(event).getSource();

    assertThatIllegalArgumentException()
      .isThrownBy(ThrowableOperation.from(args -> this.listener.vetoableChange(event)))
      .havingMessage("A target object to adapt as a JavaBean is required")
      .withNoCause();

    verify(event, times(1)).getPropertyName();
    verify(event, times(1)).getSource();
    verifyNoMoreInteractions(event);
  }

  private void testRequiresRequiredUnsetPropertyThrowsPropertyVetoException(@NotNull User<Integer> source) {

    String propertyName = "name";

    PropertyChangeEvent event =
      BeanUtils.newPropertyChangeEvent(source, propertyName, source.getName(), null);

    assertThatThrowableOfType(PropertyVetoException.class)
      .isThrownBy(ThrowableOperation.from(args -> this.listener.vetoableChange(event)))
      .havingMessage("Failed to process event [%s]", event)
      .causedBy(PropertyNotSetException.class)
      .havingMessage("Property [name] not set")
      .withNoCause();
  }

  @Test
  public void requiresPropertyForRequiredField() {
    testRequiresRequiredUnsetPropertyThrowsPropertyVetoException(RequiredNameFieldUser.as("janeDoe"));
  }

  @Test
  public void requiresPropertyForRequiredGetterMethod() {
    testRequiresRequiredUnsetPropertyThrowsPropertyVetoException(RequiredNameGetterMethodUser.as("bobDoe"));
  }

  @Test
  public void requiresPropertyForRequiredSetterMethod() {
    testRequiresRequiredUnsetPropertyThrowsPropertyVetoException(RequiredNameSetterMethodUser.as("cookieDoe"));
  }

  interface Id<T> {
    @Required T getId();
  }

  @SuppressWarnings("unused")
  static abstract class InstantAccess {

    @Required
    public Instant getLastAccess() {
      return Instant.now();
    }
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name", callSuper = true)
  @RequiredArgsConstructor(staticName = "as")
  static class RequiredNameFieldUser extends InstantAccess implements Id<Integer>, User<Integer> {

    @Setter
    private Integer id;

    @Required
    @lombok.NonNull
    private final String name;

  }

  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class RequiredNameGetterMethodUser implements User<Integer> {

    @Getter @Setter
    private Integer id;

    @lombok.NonNull
    @Getter(value = AccessLevel.PUBLIC, onMethod = @__({ @Required }))
    private final String name;

  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class RequiredNameSetterMethodUser implements User<Integer> {

    @Setter
    private Integer id;

    @lombok.NonNull
    @Setter(value = AccessLevel.PUBLIC, onMethod = @__({ @Required }))
    private String name;

  }
}
