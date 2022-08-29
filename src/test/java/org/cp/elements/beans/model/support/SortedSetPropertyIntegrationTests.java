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
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIndexOutOfBoundsException;

import java.util.Arrays;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.cp.elements.beans.model.BeanAdapter;
import org.cp.elements.beans.model.Property;
import org.junit.Test;

import lombok.Getter;

/**
 * Integration Tests for {@link SortedSetProperty}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.beans.model.support.SortedSetProperty
 * @since 1.0.0
 */
public class SortedSetPropertyIntegrationTests {

  private <T> T getElementAt(Set<T> set, int index) {

    int currentIndex = 0;

    for (T element : set) {
      if (currentIndex++ == index) {
        return element;
      }
    }

    throw newIndexOutOfBoundsException("Index [%d] is not valid in Set of size [%d]", index, set.size());
  }

  private Property getProperty(Object target, String propertyName) {
    return BeanAdapter.from(target).getModel().getProperty(propertyName);
  }

  @SuppressWarnings("unchecked")
  private <P extends SortedSetProperty> P getAsSortedSetProperty(Object target, String propertyName) {
    return (P) getProperty(target, propertyName);
  }

  @Test
  public void getValueAtIndex() {

    Property property = getProperty(new TypeWithSortedSetProperty(), "animals");

    assertThat(property).isInstanceOf(SortedSetProperty.class);
    assertThat(property.getName()).isEqualTo("animals");
    assertThat(SortedSet.class).isAssignableFrom(property.getType());

    SortedSetProperty setProperty = (SortedSetProperty) property;

    assertThat(setProperty.getValue(1)).isEqualTo("cat");
    assertThat(setProperty.getValue(2)).isEqualTo("dog");
  }

  @Test
  public void getValueAtNegativeIndex() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> getAsSortedSetProperty(new TypeWithSortedSetProperty(), "animals").getValue(-1))
      .withMessage("[-1] is not a valid index in Set with size [6]")
      .withNoCause();
  }

  @Test
  public void getValueAtOutOfBoundsIndex() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> getAsSortedSetProperty(new TypeWithSortedSetProperty(), "animals").getValue(9))
      .withMessage("[9] is not a valid index in Set with size [6]")
      .withNoCause();
  }

  @Test
  public void setValueAtIndex() {

    TypeWithSortedSetProperty target = new TypeWithSortedSetProperty();

    Property property = getProperty(target, "animals");

    assertThat(property).isInstanceOf(SortedSetProperty.class);
    assertThat(property.getName()).isEqualTo("animals");
    assertThat(SortedSet.class).isAssignableFrom(property.getType());

    SortedSetProperty setProperty = (SortedSetProperty) property;

    assertThat(target.getAnimals().iterator().next()).isEqualTo("bird");
    assertThat(setProperty.setValue(0, "hawk")).isEqualTo(true);
    assertThat(target.getAnimals().iterator().next()).isEqualTo("cat");
    assertThat(getElementAt(target.getAnimals(), 3)).isEqualTo("hawk");
    assertThat(setProperty.setValue(3, "bat")).isEqualTo(true);
    assertThat(target.getAnimals().iterator().next()).isEqualTo("bat");
  }

  @Test
  public void setValueAtNegativeIndex() {

    TypeWithSortedSetProperty target = new TypeWithSortedSetProperty();

    assertThat(target.getAnimals().iterator().next()).isEqualTo("bird");

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> getAsSortedSetProperty(target, "animals").setValue(-1, "raptor"))
      .withMessage("[-1] is not a valid index in Set with size [6]")
      .withNoCause();

    assertThat(target.getAnimals().iterator().next()).isEqualTo("bird");
  }

  @Test
  public void setValueAtOutOfBoundsIndex() {

    assertThatExceptionOfType(IndexOutOfBoundsException.class)
      .isThrownBy(() -> getAsSortedSetProperty(new TypeWithSortedSetProperty(), "animals").setValue(6, "reptile"))
      .withMessage("[6] is not a valid index in Set with size [6]")
      .withNoCause();
  }

  @Getter
  static class TypeWithSortedSetProperty  {

    private final SortedSet<String> animals =
      new TreeSet<>(Arrays.asList("monkey", "cat", "snake", "bird", "fish", "dog"));

  }
}
