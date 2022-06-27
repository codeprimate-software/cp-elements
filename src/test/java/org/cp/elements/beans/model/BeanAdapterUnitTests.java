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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.cp.elements.security.model.User;
import org.junit.Test;

/**
 * Unit Tests for {@link BeanAdapter}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.BeanAdapter
 * @since 1.0.0
 */
public class BeanAdapterUnitTests {

  @Test
  public void fromObject() {

    User<?> mockUser = mock(User.class);

    BeanAdapter userBean = BeanAdapter.from(mockUser);

    assertThat(userBean).isNotNull();
    assertThat(userBean.getTarget()).isSameAs(mockUser);

    verifyNoInteractions(mockUser);
  }

  @Test
  public void fromNullObject() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BeanAdapter.from(null))
      .withMessage("The target object to adapt as a JavaBean is required")
      .withNoCause();
  }

  @Test
  public void constructBeanAdapterWithObject() {

    User<?> mockUser = mock(User.class);

    BeanAdapter userBean = new BeanAdapter(mockUser);

    assertThat(userBean.getTarget()).isSameAs(mockUser);

    verifyNoInteractions(mockUser);
  }

  @Test
  public void constructBeanAdapterWithNullObject() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new BeanAdapter(null))
      .withMessage("The target object to adapt as a JavaBean is required")
      .withNoCause();
  }

  @Test
  public void getPropertyValueWithName() {

    User<?> mockUser = mock(User.class);

    Property mockProperty = mock(Property.class);

    BeanModel mockBeanModel = mock(BeanModel.class);

    BeanAdapter userBean = spy(BeanAdapter.from(mockUser));

    doReturn(mockBeanModel).when(userBean).getModel();
    doReturn(mockProperty).when(mockBeanModel).getProperty(eq("name"));
    doReturn("jonDoe").when(mockProperty).getValue();

    assertThat(userBean.<String>getPropertyValue("name")).isEqualTo("jonDoe");

    verify(userBean, times(1)).getModel();
    verify(mockBeanModel, times(1)).getProperty(eq("name"));
    verify(mockProperty, times(1)).getValue();
    verifyNoMoreInteractions(mockBeanModel, mockProperty);
    verifyNoInteractions(mockUser);
  }

  private void testGetPropertyValueWithInvalidName(String name) {

    User<?> mockUser = mock(User.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BeanAdapter.from(mockUser).getPropertyValue(name))
      .withMessage("The name [%s] of the property is required", name)
      .withNoCause();

    verifyNoInteractions(mockUser);
  }

  @Test
  public void getPropertyValueWithBlankName() {
    testGetPropertyValueWithInvalidName("  ");
  }

  @Test
  public void getPropertyValueWithEmptyName() {
    testGetPropertyValueWithInvalidName("");
  }

  @Test
  public void getPropertyValueWithNullName() {
    testGetPropertyValueWithInvalidName(null);
  }

  @Test
  public void setPropertyWithName() {

    User<?> mockUser = mock(User.class);

    BeanModel mockBeanModel = mock(BeanModel.class);

    Property mockProperty = mock(Property.class);

    BeanAdapter userBean = spy(BeanAdapter.from(mockUser));

    doReturn(mockBeanModel).when(userBean).getModel();
    doReturn(mockProperty).when(mockBeanModel).getProperty(eq("id"));

    userBean.setPropertyValue("id", 1);

    verify(userBean, times(1)).getModel();
    verify(mockBeanModel, times(1)).getProperty(eq("id"));
    verify(mockProperty, times(1)).setValue(eq(1));
    verifyNoMoreInteractions(mockBeanModel, mockProperty);
    verifyNoInteractions(mockUser);
  }

  private void testSetPropertyValueWithInvalidName(String name) {

    User<?> mockUser = mock(User.class);

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BeanAdapter.from(mockUser).setPropertyValue(name, 1))
      .withMessage("The name [%s] of the property to set is required", name)
      .withNoCause();

    verifyNoInteractions(mockUser);
  }

  @Test
  public void setPropertyWithBlankName() {
    testSetPropertyValueWithInvalidName("  ");
  }

  @Test
  public void setPropertyWithEmptyName() {
    testSetPropertyValueWithInvalidName("");
  }

  @Test
  public void setPropertyWithNullName() {
    testSetPropertyValueWithInvalidName(null);
  }
}
