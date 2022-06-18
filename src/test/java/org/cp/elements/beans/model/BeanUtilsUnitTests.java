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

import java.beans.BeanDescriptor;
import java.beans.BeanInfo;
import java.beans.PropertyDescriptor;
import java.util.Arrays;
import java.util.stream.Collectors;

import org.cp.elements.process.BaseProcess;
import org.cp.elements.security.model.User;
import org.junit.Test;

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
 * @see org.junit.Test
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

    Process process = new TestProcess();

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

  private static class TestProcess extends BaseProcess { }

  @EqualsAndHashCode
  @ToString(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  private static class TestUser implements User<Integer> {

    @Getter @Setter
    private Integer id;

    @NonNull @Getter
    private final String name;

  }
}
