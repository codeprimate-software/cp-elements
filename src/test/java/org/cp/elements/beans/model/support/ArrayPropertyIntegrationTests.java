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

import org.cp.elements.beans.model.BeanUtils;
import org.cp.elements.beans.model.Property;
import org.junit.jupiter.api.Test;

import lombok.Getter;
import lombok.Setter;

/**
 * Integration Tests for {@link ArrayProperty}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.beans.model.support.ArrayProperty
 * @since 1.0.0
 */
public class ArrayPropertyIntegrationTests {

 @Test
  public void getValueAtIndex() {

    Property property = BeanUtils.getProperty(new TypeWithArrayProperty(), "array");

    assertThat(property).isInstanceOf(ArrayProperty.class);
    assertThat(property.getName()).isEqualTo("array");
    assertThat(property.getType()).isEqualTo(Object[].class);

    ArrayProperty arrayProperty = (ArrayProperty) property;

    assertThat(arrayProperty.getValue(1)).isEqualTo(2);
  }

  @Test
  public void getValueAtNegativeIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ArrayProperty>getProperty(new TypeWithArrayProperty(), "array")
        .getValue(-1))
      .withMessage("Array index [-1] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  @Test
  public void getValueAtOverflowIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ArrayProperty>getProperty(new TypeWithArrayProperty(), "array")
        .getValue(3))
      .withMessage("Array index [3] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  @Test
  public void setValueAtIndex() {

    TypeWithArrayProperty target = new TypeWithArrayProperty();

    Property property = BeanUtils.getProperty(target, "array");

    assertThat(property).isInstanceOf(ArrayProperty.class);
    assertThat(property.getName()).isEqualTo("array");
    assertThat(property.getType()).isEqualTo(Object[].class);

    ArrayProperty arrayProperty = (ArrayProperty) property;

    assertThat(target.array[2]).isEqualTo(3);
    assertThat(arrayProperty.setValue(2, 4)).isEqualTo(3);
    assertThat(target.array[2]).isEqualTo(4);
  }

  @Test
  public void setValueAtNegativeIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ArrayProperty>getProperty(new TypeWithArrayProperty(), "array")
        .setValue(-1, 13))
      .withMessage("Array index [-1] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  @Test
  public void setValueAtOverflowIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> BeanUtils.<ArrayProperty>getProperty(new TypeWithArrayProperty(), "array")
        .setValue(3, 13))
      .withMessage("Array index [3] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  static class TypeWithArrayProperty {

    @Getter @Setter
    private Object[] array = { 1, 2, 3 };

  }
}
