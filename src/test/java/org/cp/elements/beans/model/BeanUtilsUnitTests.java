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
package org.cp.elements.beans.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.BeanDescriptor;
import java.beans.BeanInfo;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.cp.elements.beans.PropertyNotFoundException;
import org.cp.elements.beans.model.support.ArrayProperty;
import org.cp.elements.process.AbstractBaseProcess;
import org.cp.elements.security.model.User;

import org.junit.jupiter.api.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link BeanUtils}.
 *
 * @author John Blum
 * @see java.beans.BeanDescriptor
 * @see java.beans.BeanInfo
 * @see java.beans.PropertyDescriptor
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.BeanUtils
 * @since 1.0.0
 */
public class BeanUtilsUnitTests {

  @Test
  public void acquiresBeanInfoForUserObject() {

    User<?> user = TestUser.as("jonDoe");

    BeanInfo userInfo = BeanUtils.acquireBeanInformation(user);

    assertThat(userInfo).isNotNull();

    BeanDescriptor userDescriptor = userInfo.getBeanDescriptor();

    assertThat(userDescriptor).isNotNull();
    assertThat(User.class).isAssignableFrom(userDescriptor.getBeanClass());

    PropertyDescriptor[] userProperties = userInfo.getPropertyDescriptors();

    assertThat(userProperties).isNotNull();
    assertThat(userProperties).hasSize(2);
    assertThat(Arrays.stream(userProperties)
        .map(PropertyDescriptor::getName)
        .collect(Collectors.toList())
        .stream()
        .sorted())
      .containsExactly("id", "name");

    PropertyDescriptor id = Arrays.stream(userProperties)
      .filter(it -> "id".equals(it.getName()))
      .findFirst()
      .orElse(null);

    assertThat(id).isNotNull();
    assertThat(id.getName()).isEqualTo("id");
    assertThat(id.getPropertyType()).isEqualTo(Integer.class);
    assertThat(id.getReadMethod()).isNotNull();
    assertThat(id.getReadMethod().getName()).isEqualTo("getId");
    assertThat(id.getWriteMethod()).isNotNull();
    assertThat(id.getWriteMethod().getName()).isEqualTo("setId");

    PropertyDescriptor name = Arrays.stream(userProperties)
      .filter(it -> "name".equals(it.getName()))
      .findFirst()
      .orElse(null);

    assertThat(name).isNotNull();
    assertThat(name.getName()).isEqualTo("name");
    assertThat(name.getPropertyType()).isEqualTo(String.class);
    assertThat(name.getReadMethod()).isNotNull();
    assertThat(name.getReadMethod().getName()).isEqualTo("getName");
    assertThat(name.getWriteMethod()).isNull();
  }

  @Test
  public void acquireBeanInfoFromProcess() {

    Process process = new TestBaseProcess();

    BeanInfo processInfo = BeanUtils.acquireBeanInformation(process);

    assertThat(processInfo).isNotNull();

    BeanDescriptor processDescriptor = processInfo.getBeanDescriptor();

    assertThat(processDescriptor).isNotNull();
    assertThat(Process.class).isAssignableFrom(processDescriptor.getBeanClass());

    PropertyDescriptor[] processProperties = processInfo.getPropertyDescriptors();

    assertThat(processProperties).isNotNull();
    assertThat(processProperties).hasSize(6);

    assertThat(Arrays.stream(processProperties)
        .map(PropertyDescriptor::getName)
        .collect(Collectors.toSet())
        .stream()
        .sorted())
      .containsExactly("alive", "errorStream", "id", "inputStream", "name", "outputStream");

    PropertyDescriptor alive = Arrays.stream(processProperties)
      .filter(it -> "alive".equals(it.getName()))
      .findFirst()
      .orElse(null);

    assertThat(alive).isNotNull();
    assertThat(alive.getName()).isEqualTo("alive");
    assertThat(alive.getPropertyType()).isEqualTo(Boolean.TYPE);
    assertThat(alive.getReadMethod()).isNotNull();
    assertThat(alive.getReadMethod().getName()).isEqualTo("isAlive");
    assertThat(alive.getWriteMethod()).isNull();
  }

  @Test
  public void acquireBeanInfoFromNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BeanUtils.acquireBeanInformation(null))
      .withMessage("Bean is required")
      .withNoCause();
  }

  @Test
  public void getPropertyFromObject() {

    Property name = BeanUtils.getProperty(TestUser.as("jonDoe"), "name");

    assertThat(name).isNotNull();
    assertThat(name.getName()).isEqualTo("name");
    assertThat(name.getType()).isEqualTo(String.class);
    assertThat(name.getValue()).isEqualTo("jonDoe");
  }

  @Test
  public void getArrayPropertyFromObject() {

    Property array = BeanUtils.getProperty(new TypeWithArrayProperty(), "array");

    assertThat(array).isInstanceOf(ArrayProperty.class);
    assertThat(array.getName()).isEqualTo("array");
    assertThat(array.getType()).isEqualTo(Object[].class);
    assertThat(((ArrayProperty) array).getValue(1)).isEqualTo(2);
  }

  @Test
  public void getPropertyFromNullObject() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BeanUtils.getProperty(null, "mockProperty"))
      .withMessage("A target object to adapt as a JavaBean is required")
      .withNoCause();
  }

  @Test
  public void getNonExistingPropertyFromObject() {

    assertThatExceptionOfType(PropertyNotFoundException.class)
      .isThrownBy(() -> BeanUtils.getProperty(new TestBaseProcess(), "nonExistingProperty"))
      .withMessage("Property with name [nonExistingProperty] not found")
      .withNoCause();
  }

  @Test
  public void newPropertyChangeEventIsCorrect() {

    Object source = new Object();

    PropertyChangeEvent event =
      BeanUtils.newPropertyChangeEvent(source, "mockProperty", 1, 2);

    assertThat(event).isNotNull();
    assertThat(event.getSource()).isEqualTo(source);
    assertThat(event.getPropertyName()).isEqualTo("mockProperty");
    assertThat(event.getOldValue()).isEqualTo(1);
    assertThat(event.getNewValue()).isEqualTo(2);
  }

  @Test
  public void newPropertyChangeEventWithNoSource() {

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> BeanUtils.newPropertyChangeEvent(null, "mockProperty", 1, 2))
      .withMessage("Source is required")
      .withNoCause();
  }

  @Test
  public void newPropertyChangeEventWithBlankPropertyName() {

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> BeanUtils.newPropertyChangeEvent(new Object(), "  ", 1, 2))
      .withMessage("Property name [  ] is required")
      .withNoCause();
  }

  @Test
  public void newPropertyChangeEventWithEmptyPropertyName() {

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> BeanUtils.newPropertyChangeEvent(new Object(), "", 1, 2))
      .withMessage("Property name [] is required")
      .withNoCause();
  }

  @Test
  public void newPropertyChangeEventWithNullPropertyName() {

    assertThatExceptionOfType(IllegalArgumentException.class)
      .isThrownBy(() -> BeanUtils.newPropertyChangeEvent(new Object(), null, 1, 2))
      .withMessage("Property name [null] is required")
      .withNoCause();
  }

  @Test
  public void resolveTypeFromBean() {

    BeanAdapter mockBean = mock(BeanAdapter.class);

    TestUser user = TestUser.as("TestUser");

    doReturn(user).when(mockBean).getTarget();

    assertThat(BeanUtils.resolveType(mockBean)).isEqualTo(TestUser.class);

    verify(mockBean, times(1)).getTarget();
    verifyNoMoreInteractions(mockBean);
  }

  @Test
  public void resolveTypeFromClass() {

    assertThat(BeanUtils.resolveType(Boolean.class)).isEqualTo(Boolean.class);
    assertThat(BeanUtils.resolveType(Integer.TYPE)).isEqualTo(Integer.TYPE);
    assertThat(BeanUtils.resolveType(Integer.class)).isEqualTo(Integer.class);
    assertThat(BeanUtils.resolveType(String.class)).isEqualTo(String.class);
  }

  @Test
  public void resolveTypeFromObject() {

    User<Integer> user = TestUser.as("TestUser");

    assertThat(BeanUtils.resolveType(user)).isEqualTo(TestUser.class);
  }

  @Test
  public void resolveTypeFromProperty() {

    Property mockProperty = mock(Property.class);

    doReturn(User.class).when(mockProperty).getType();

    assertThat(BeanUtils.resolveType(mockProperty)).isEqualTo(User.class);

    verify(mockProperty, times(1)).getType();
    verifyNoMoreInteractions(mockProperty);
  }

  @Test
  public void resolveTypeFromPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(User.class).when(mockPropertyDescriptor).getPropertyType();

    assertThat(BeanUtils.resolveType(mockPropertyDescriptor)).isEqualTo(User.class);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void resolveTypeFromNullIsNullSafe() {
    assertThat(BeanUtils.resolveType(null)).isEqualTo(Object.class);
  }

  private static class TestBaseProcess extends AbstractBaseProcess { }

  @EqualsAndHashCode
  @ToString(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  private static class TestUser implements User<Integer> {

    @Getter @Setter
    private Integer id;

    @NonNull @Getter
    private final String name;

  }

  @Getter
  private static class TypeWithArrayProperty {

    private final Object[] array = { 1, 2, 3 };

  }
}
