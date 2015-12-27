/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util.convert;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.sql.Timestamp;
import java.util.Calendar;

import org.cp.elements.lang.Constants;
import org.cp.elements.test.AbstractMockingTestSuite;
import org.junit.Test;

/**
 * The AbstractConverterTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractConverter class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.cp.elements.util.convert.AbstractConverter
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings({ "deprecation", "unused" })
public class AbstractConverterTest extends AbstractMockingTestSuite {

  @Test
  public void testSetAndGetConversionService() {
    AbstractConverter converter = new TestConverter();
    ConversionService mockConversionService = mockContext.mock(ConversionService.class);

    converter.setConversionService(mockConversionService);

    assertSame(mockConversionService, converter.getConversionService());
  }

  @Test(expected = IllegalStateException.class)
  public void testGetConversionService() {
    try {
      new TestConverter().getConversionService();
    }
    catch (IllegalStateException expected) {
      assertEquals("The ConversionService reference was not properly initialized!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  @SuppressWarnings("unchecked")
  public void testIsAssignableTo() {
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
