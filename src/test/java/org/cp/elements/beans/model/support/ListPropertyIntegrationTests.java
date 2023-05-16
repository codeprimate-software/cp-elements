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
package org.cp.elements.beans.model.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.util.Arrays;
import java.util.List;

import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.beans.model.Property;
import org.junit.jupiter.api.Test;

import lombok.Getter;

/**
 * Integration Tests for {@link ListProperty}.
 *
 * @author John Blum
 * @see java.util.List
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.model.support.ListProperty
 * @since 1.0.0
 */
public class ListPropertyIntegrationTests {

  @Test
  public void getValueAtIndex() {

    Property property = BeanUtils.getProperty(new TypeWithListProperty(), "list");

    assertThat(property).isInstanceOf(ListProperty.class);
    assertThat(property.getName()).isEqualTo("list");
    assertThat(property.getType()).isEqualTo(List.class);

    ListProperty listProperty = (ListProperty) property;

    assertThat(listProperty.getValue(1)).isEqualTo(2);
  }

  @Test
  public void getValueAtNegativeIndex() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ListProperty>getProperty(new TypeWithListProperty(), "list")
        .getValue(-1))
      .withMessage("List index [-1] must be greater than equal to [0] and less than [3]")
      .withNoCause();
  }

  @Test
  public void getValueAtOverflowIndex() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ListProperty>getProperty(new TypeWithListProperty(), "list")
        .getValue(3))
      .withMessage("List index [3] must be greater than equal to [0] and less than [3]")
      .withNoCause();
  }

  @Test
  public void setValueAtIndex() {

    TypeWithListProperty target = new TypeWithListProperty();

    Property property = BeanUtils.getProperty(target, "list");

    assertThat(property).isInstanceOf(ListProperty.class);
    assertThat(property.getName()).isEqualTo("list");
    assertThat(property.getType()).isEqualTo(List.class);

    ListProperty listProperty = (ListProperty) property;

    assertThat(target.getList().get(2)).isEqualTo(3);
    assertThat(listProperty.setValue(2, 4)).isEqualTo(3);
    assertThat(target.getList().get(2)).isEqualTo(4);
  }

  @Test
  public void setValueAtNegativeIndex() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ListProperty>getProperty(new TypeWithListProperty(), "list")
        .setValue(-1, 8))
      .withMessage("List index [-1] must be greater than equal to [0] and less than [3]")
      .withNoCause();
  }

  @Test
  public void setValueAtOverflowIndex() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ListProperty>getProperty(new TypeWithListProperty(), "list")
        .setValue(3, 8))
      .withMessage("List index [3] must be greater than equal to [0] and less than [3]")
      .withNoCause();
  }

  @Getter
  static class TypeWithListProperty {

    private final List<Object> list = Arrays.asList(1, 2, 3);

  }
}
