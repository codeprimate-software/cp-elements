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

import org.cp.elements.beans.model.BeanAdapter;
import org.cp.elements.beans.model.Property;
import org.junit.Test;

import lombok.Getter;
import lombok.Setter;

/**
 * Integration Tests for {@link ArrayProperty}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.beans.model.support.ArrayProperty
 * @since 1.0.0
 */
public class ArrayPropertyIntegrationTests {

  private Property propertyFrom(Object target, String propertyName) {
    return BeanAdapter.from(target).getModel().getProperty(propertyName);
  }

  private <T extends Property> T typedPropertyFrom(Object target, String propertyName, Class<T> propertyClass) {
    return propertyFrom(target, propertyName).asTypedProperty(propertyClass);
  }

  @Test
  public void getValueAtIndex() {

    Property array = BeanAdapter.from(new TypeWithArrayProperty()).getModel().getProperty("array");

    assertThat(array).isNotNull();
    assertThat(array.getName()).isEqualTo("array");
    assertThat(array.getType()).isEqualTo(Object[].class);
    assertThat(array).isInstanceOf(ArrayProperty.class);

    ArrayProperty arrayProperty = (ArrayProperty) array;

    assertThat(arrayProperty.getValue(1)).isEqualTo(2);
  }

  @Test
  public void getValueAtNegativeIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> typedPropertyFrom(new TypeWithArrayProperty(), "array", ArrayProperty.class)
        .getValue(-1))
      .withMessage("Array index [-1] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  @Test
  public void getValueAtOverflowIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> typedPropertyFrom(new TypeWithArrayProperty(), "array", ArrayProperty.class)
        .getValue(3))
      .withMessage("Array index [3] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  @Test
  public void setValueAtIndex() {

    TypeWithArrayProperty target = new TypeWithArrayProperty();

    Property array = BeanAdapter.from(target).getModel().getProperty("array");

    assertThat(array).isNotNull();
    assertThat(array.getName()).isEqualTo("array");
    assertThat(array.getType()).isEqualTo(Object[].class);
    assertThat(array).isInstanceOf(ArrayProperty.class);

    ArrayProperty arrayProperty = (ArrayProperty) array;

    assertThat(target.array[2]).isEqualTo(3);
    assertThat(arrayProperty.setValue(2, 4)).isEqualTo(3);
    assertThat(target.array[2]).isEqualTo(4);
  }

  @Test
  public void setValueAtNegativeIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> typedPropertyFrom(new TypeWithArrayProperty(), "array", ArrayProperty.class)
        .setValue(-1, 13))
      .withMessage("Array index [-1] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  @Test
  public void setValueAtOverflowIndex() {

    assertThatExceptionOfType(ArrayIndexOutOfBoundsException.class)
      .isThrownBy(() -> typedPropertyFrom(new TypeWithArrayProperty(), "array", ArrayProperty.class)
        .setValue(3, 13))
      .withMessage("Array index [3] must be greater than equal to 0 and less than [3]")
      .withNoCause();
  }

  static class TypeWithArrayProperty {

    @Getter @Setter
    private Object[] array = { 1, 2, 3 };

  }
}
