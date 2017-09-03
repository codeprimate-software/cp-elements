/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.data.convert;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;

import java.sql.Timestamp;
import java.util.Calendar;

import org.cp.elements.lang.Constants;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Test suite of test cases testing the contract and functionality of the {@link AbstractConverter} class.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.convert.AbstractConverter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractConverterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void setAndGetConversionService() {
    AbstractConverter converter = new TestConverter();
    ConversionService mockConversionService = mock(ConversionService.class);

    converter.setConversionService(mockConversionService);

    assertSame(mockConversionService, converter.getConversionService());
  }

  @Test
  public void getConversionService() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The ConversionService reference was not properly initialized!");

    new TestConverter().getConversionService();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void isAssignableTo() {
    AbstractConverter converter = new TestConverter();

    assertTrue(converter.isAssignableTo(Character.class, Short.class, String.class, Object.class));
    assertTrue(converter.isAssignableTo(Boolean.class, Boolean.class, Byte.class, Character.class, String.class));
    assertFalse(converter.isAssignableTo(Timestamp.class, Calendar.class, Long.class, String.class));
    assertFalse(converter.isAssignableTo(Object.class, Boolean.class, Integer.class, String.class));
  }

  protected static class TestConverter extends AbstractConverter<Object, Object> {

    @Override
    public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @Override
    public Object convert(final Object value) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    @Override
    public <QT> QT convert(final Object value, final Class<QT> qualifyingType) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
