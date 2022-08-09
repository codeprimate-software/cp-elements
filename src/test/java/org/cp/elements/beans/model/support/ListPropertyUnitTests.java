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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.beans.PropertyDescriptor;
import java.util.List;

import org.junit.Test;

/**
 * Unit Tests for {@link ListProperty}.
 *
 * @author John Blum
 * @see java.util.List
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.beans.model.support.ListProperty
 * @since 1.0.0
 */
public class ListPropertyUnitTests {

  @Test
  public void assertListTypeWithListBasedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(List.class).when(mockPropertyDescriptor).getPropertyType();

    assertThat(ListProperty.assertListType(mockPropertyDescriptor)).isSameAs(mockPropertyDescriptor);

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertListTypeWithNonListBasedPropertyDescriptor() {

    PropertyDescriptor mockPropertyDescriptor = mock(PropertyDescriptor.class);

    doReturn(Object.class).when(mockPropertyDescriptor).getPropertyType();

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListProperty.assertListType(mockPropertyDescriptor))
      .withMessage("Property [%s] must be a List", mockPropertyDescriptor)
      .withNoCause();

    verify(mockPropertyDescriptor, times(1)).getPropertyType();
    verifyNoMoreInteractions(mockPropertyDescriptor);
  }

  @Test
  public void assertListTypeWithNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> ListProperty.assertListType(null))
      .withMessage("Property [null] must be a List")
      .withNoCause();
  }
}
