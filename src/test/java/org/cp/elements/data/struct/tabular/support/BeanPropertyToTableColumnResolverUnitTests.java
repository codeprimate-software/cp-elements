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
package org.cp.elements.data.struct.tabular.support;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Optional;

import org.junit.Test;

import org.cp.elements.beans.model.Property;
import org.cp.elements.data.struct.tabular.Column;

/**
 * Unit Tests for {@link BeanPropertyToTableColumnResolver}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.tabular.support.BeanPropertyToTableColumnResolver
 * @see org.cp.elements.data.struct.tabular.Column
 * @see org.cp.elements.beans.model.Property
 * @since 1.0.2
 */
public class BeanPropertyToTableColumnResolverUnitTests {

  @Test
  @SuppressWarnings({ "unchecked", "rawtypes" })
  public void requireTableColumnFromBeanPropertyIsSuccessful() {

    Property mockProperty = mock(Property.class);

    Column<?> mockColumn = mock(Column.class);

    BeanPropertyToTableColumnResolver resolver = mock(BeanPropertyToTableColumnResolver.class);

    doCallRealMethod().when(resolver).require(any(Property.class));
    doReturn(Optional.of(mockColumn)).when(resolver).resolve(eq(mockProperty));

    assertThat(resolver.require(mockProperty)).isEqualTo((Column) mockColumn);

    verify(resolver, times(1)).require(eq(mockProperty));
    verify(resolver, times(1)).resolve(eq(mockProperty));
    verifyNoInteractions(mockColumn, mockProperty);
    verifyNoMoreInteractions(resolver);
  }

  @Test
  public void requireWhenTableColumnForBeanPropertyIsMissing() {

    Property mockProperty = mock(Property.class);

    BeanPropertyToTableColumnResolver resolver = mock(BeanPropertyToTableColumnResolver.class);

    doCallRealMethod().when(resolver).require(any(Property.class));
    doReturn(Optional.empty()).when(resolver).resolve(eq(mockProperty));

    assertThatExceptionOfType(ColumnNotFoundException.class)
      .isThrownBy(() -> resolver.require(mockProperty))
      .withMessage("Table column for bean property [%s] was not found", mockProperty)
      .withNoCause();

    verify(resolver, times(1)).require(eq(mockProperty));
    verify(resolver, times(1)).resolve(eq(mockProperty));
    verifyNoMoreInteractions(resolver);
    verifyNoInteractions(mockProperty);
  }
}
