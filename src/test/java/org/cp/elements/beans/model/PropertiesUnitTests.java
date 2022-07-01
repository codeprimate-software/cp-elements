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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link Properties}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.Properties
 * @since 1.0.0
 */
public class PropertiesUnitTests {

  @Test
  public void emptyProperties() {

    Properties properties = Properties.empty();

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void fromNullBeanModel() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Properties.from(null))
      .withMessage("BeanModel is required")
      .withNoCause();
  }

  @Test
  public void ofArrayOfProperties() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Properties properties = Properties.of(mockPropertyOne, mockPropertyTwo);

    assertThat(properties).isNotNull();
    assertThat(properties).containsExactlyInAnyOrder(mockPropertyOne, mockPropertyTwo);

    verifyNoInteractions(mockPropertyOne, mockPropertyTwo);
  }

  @Test
  public void ofEmptyArray() {

    Properties properties = Properties.of();

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofNullArrayIsNullSafe() {

    Properties properties = Properties.of((Property[]) null);

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofIterableContainingProperties() {

    Property mockPropertyOne = mock(Property.class);
    Property mockPropertyTwo = mock(Property.class);

    Iterable<Property> iterable = ArrayUtils.asIterable(mockPropertyOne, mockPropertyTwo);

    Properties properties = Properties.of(iterable);

    assertThat(properties).isNotNull();
    assertThat(properties).containsExactlyInAnyOrder(mockPropertyOne, mockPropertyTwo);

    verifyNoInteractions(mockPropertyOne, mockPropertyTwo);
  }

  @Test
  public void ofEmptyIterable() {

    Properties properties = Properties.of(CollectionUtils.emptyIterable());

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void ofNullIterableIsNullSafe() {

    Properties properties = Properties.of((Iterable<Property>) null);

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }
}
