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
package org.cp.elements.data.conversion.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.beans.PropertyEditor;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.enums.Gender;
import org.cp.elements.lang.ThrowableAssertions;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link PropertyEditingConverterAdapter}.
 *
 * @author John Blum
 * @see java.beans.PropertyEditor
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.data.conversion.support.PropertyEditingConverterAdapter
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class PropertyEditingConverterAdapterTests {

  @Mock
  private PropertyEditor mockPropertyEditor;

  @Test
  public void constructNewPropertyEditingConverterAdapter() {

    PropertyEditingConverterAdapter converter = new PropertyEditingConverterAdapter(this.mockPropertyEditor);

    assertThat(converter).isNotNull();
    assertThat(converter.getPropertyEditor()).isEqualTo(this.mockPropertyEditor);
  }

  @Test
  public void constructNewPropertyEditingConverterAdapterWithNullPropertyEditorThrowsException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new PropertyEditingConverterAdapter(null))
      .withMessage("PropertyEditor is required")
      .withNoCause();
  }

  @Test
  public void convertIsSuccessful() {

    when(this.mockPropertyEditor.getValue()).thenReturn(Gender.FEMALE);

    assertThat(PropertyEditingConverterAdapter.of(this.mockPropertyEditor).convert("female"))
      .isEqualTo(Gender.FEMALE);

    verify(this.mockPropertyEditor, times(1)).setAsText(eq("female"));
    verify(this.mockPropertyEditor, times(1)).getValue();
  }

  @Test
  public void convertHandlesIllegalArgumentExceptionThrowsConversionException() {

    doThrow(newIllegalArgumentException("test")).when(this.mockPropertyEditor).setAsText(anyString());

    ThrowableAssertions.assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> PropertyEditingConverterAdapter.of(this.mockPropertyEditor).convert("test"))
      .havingMessage("Cannot convert [test] to an Object type")
      .causedBy(IllegalArgumentException.class)
      .havingMessage("test")
      .withNoCause();

    verify(this.mockPropertyEditor, times(1)).setAsText(eq("test"));
    verify(this.mockPropertyEditor, never()).getValue();
  }
}
