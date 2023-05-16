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
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.UUID;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.security.model.User;

/**
 * Unit Tests for {@link BeanModel}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.BeanModel
 * @since 1.0.0
 */
public class BeanModelUnitTests {

  @SuppressWarnings("unchecked")
  private @NotNull User<UUID> mockUser(@NotNull String name) {

    User <UUID> mockUser = mock(User.class, name);

    doReturn(UUID.randomUUID()).when(mockUser).getId();
    doReturn(name).when(mockUser).getName();

    return mockUser;
  }

  @Test
  public void fromNonNullBean() {

    User<?> mockUser = mock(User.class);

    BeanAdapter mockUserBean = mock(BeanAdapter.class);

    doReturn(mockUser).when(mockUserBean).getTarget();

    BeanModel userModel = BeanModel.from(mockUserBean);

    assertThat(userModel).isNotNull();
    assertThat(userModel.getBean()).isSameAs(mockUserBean);
    assertThat(userModel.getBeanInfo()).isNotNull();
    assertThat(userModel.getProperties()).isNotNull();
    assertThat(userModel.getTargetObject()).isSameAs(mockUser);
    assertThat(User.class).isAssignableFrom(userModel.getTargetType());

    verify(mockUserBean, atLeastOnce()).getTarget();
  }

  @Test
  public void fromNonNullBeanUsesCache() {

    User<?> pieDoe = mockUser("pieDoe");
    User<?> sourDoe = mockUser("sourDoe");

    BeanAdapter pieDoeBean = BeanAdapter.from(pieDoe);
    BeanAdapter sourDoeBean= BeanAdapter.from(sourDoe);

    assertThat(pieDoeBean).isNotNull();
    assertThat(sourDoeBean).isNotNull();
    assertThat(pieDoeBean).isNotEqualTo(sourDoeBean);

    BeanModel pieDoeModel = pieDoeBean.getModel();
    BeanModel sourDoeModel = sourDoeBean.getModel();

    assertThat(pieDoeModel).isNotNull();
    assertThat(sourDoeModel).isNotNull();
    assertThat(pieDoeModel).isNotEqualTo(sourDoeModel);
  }

  @Test
  public void fromNullBean() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> BeanModel.from(null))
      .withMessage("Bean is required")
      .withNoCause();
  }

  @Test
  public void constructNewBeanModelIsCorrect() {

    Process mockProcess = mock(Process.class);

    BeanAdapter mockProcessBean = mock(BeanAdapter.class);

    doReturn(mockProcess).when(mockProcessBean).getTarget();

    BeanModel processModel = new BeanModel(mockProcessBean);

    assertThat(processModel).isNotNull();
    assertThat(processModel.getBean()).isSameAs(mockProcessBean);
    assertThat(processModel.getBeanInfo()).isNotNull();
    assertThat(processModel.getProperties()).isNotNull();
    assertThat(processModel.getTargetObject()).isSameAs(mockProcess);
    assertThat(Process.class).isAssignableFrom(processModel.getTargetType());

    verify(mockProcessBean, atLeastOnce()).getTarget();
  }

  @Test
  public void constructNewBeanModelWithNullThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new BeanModel(null))
      .withMessage("Bean is required")
      .withNoCause();
  }

  @Test
  public void getPropertyDelegatesToProperties() {

    User<?> mockUser = mock(User.class);

    Properties mockProperties = mock(Properties.class);

    Property mockProperty = mock(Property.class);

    BeanAdapter mockUserBean = mock(BeanAdapter.class);

    doReturn(mockUser).when(mockUserBean).getTarget();
    doReturn(mockProperty).when(mockProperties).findByName(eq("mockPropertyName"));

    BeanModel beanModel = spy(BeanModel.from(mockUserBean));

    doReturn(mockProperties).when(beanModel).getProperties();

    assertThat(beanModel.getProperty("mockPropertyName")).isEqualTo(mockProperty);

    verify(beanModel, times(1)).getProperties();
    verify(mockProperties, times(1)).findByName(eq("mockPropertyName"));
    verifyNoMoreInteractions(mockProperties);
    verifyNoInteractions(mockProperty);
  }
}
