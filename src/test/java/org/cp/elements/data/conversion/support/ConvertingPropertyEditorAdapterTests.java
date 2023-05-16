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
import static org.cp.elements.lang.ElementsExceptionsFactory.newConversionException;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.enums.Gender;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link ConvertingPropertyEditorAdapter}.
 *
 * @author John Blum
 * @see java.beans.PropertyEditor
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.data.conversion.support.ConvertingPropertyEditorAdapter
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ConvertingPropertyEditorAdapterTests {

  @Mock
  private Converter<String, Object> mockConverter;

  @Test
  public void constructsNewConvertingPropertyEditorAdapter() {

    ConvertingPropertyEditorAdapter propertyEditor = new ConvertingPropertyEditorAdapter(this.mockConverter);

    assertThat(propertyEditor).isNotNull();
    assertThat(propertyEditor.getConverter()).isEqualTo(this.mockConverter);
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructsNewConvertingPropertyEditorWithNullConverterThrowsException() {

    try {
      new ConvertingPropertyEditorAdapter(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Converter is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void setAsTextIsSuccessful() {

    when(this.mockConverter.convert(eq("female"))).thenReturn(Gender.FEMALE);

    ConvertingPropertyEditorAdapter propertyEditor = ConvertingPropertyEditorAdapter.of(this.mockConverter);

    assertThat(propertyEditor).isNotNull();
    assertThat(propertyEditor.getConverter()).isEqualTo(this.mockConverter);

    propertyEditor.setAsText("female");

    assertThat(propertyEditor.getValue()).isEqualTo(Gender.FEMALE);

    verify(this.mockConverter, times(1)).convert(eq("female"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void setAsTextHandlesConversionExceptionThrowsIllegalArgumentException() {

    when(this.mockConverter.convert(anyString())).thenThrow(newConversionException("test"));

    ConvertingPropertyEditorAdapter propertyEditor = ConvertingPropertyEditorAdapter.of(this.mockConverter);

    try {
      assertThat(propertyEditor).isNotNull();
      assertThat(propertyEditor.getConverter()).isEqualTo(this.mockConverter);
      assertThat(propertyEditor.getValue()).isNull();

      propertyEditor.setAsText("test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Could not set text [test] as value");
      assertThat(expected).hasCauseInstanceOf(ConversionException.class);
      assertThat(expected.getCause()).hasMessage("test");
      assertThat(expected.getCause()).hasNoCause();

      throw expected;
    }
    finally {
      verify(this.mockConverter, times(1)).convert(eq("test"));
      assertThat(propertyEditor.getValue()).isNull();
    }
  }
}
