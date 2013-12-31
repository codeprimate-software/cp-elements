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

package org.cp.elements.util.convert.provider;

import static org.junit.Assert.assertTrue;

import java.util.HashSet;
import java.util.Set;

import org.cp.elements.util.convert.Converter;
import org.cp.elements.util.convert.support.BigDecimalConverter;
import org.cp.elements.util.convert.support.BigIntegerConverter;
import org.cp.elements.util.convert.support.BooleanConverter;
import org.cp.elements.util.convert.support.ByteConverter;
import org.cp.elements.util.convert.support.CalendarConverter;
import org.cp.elements.util.convert.support.CharacterConverter;
import org.cp.elements.util.convert.support.DoubleConverter;
import org.cp.elements.util.convert.support.EnumConverter;
import org.cp.elements.util.convert.support.FloatConverter;
import org.cp.elements.util.convert.support.IdentifiableConverter;
import org.cp.elements.util.convert.support.IntegerConverter;
import org.cp.elements.util.convert.support.LongConverter;
import org.cp.elements.util.convert.support.NumberConverter;
import org.cp.elements.util.convert.support.ShortConverter;
import org.cp.elements.util.convert.support.StringConverter;
import org.cp.elements.util.convert.support.URIConverter;
import org.cp.elements.util.convert.support.URLConverter;
import org.junit.Test;

/**
 * The DefaultConversionServiceTest class is a test suite of test cases testing the contract and functionality of the
 * DefaultConversionService class.
 * <p/>
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.util.convert.provider.DefaultConversionService
 * @since 1.0.0
 */
public class DefaultConversionServiceTest {

  @Test
  public void testRegisteredSupportConverters() {
    DefaultConversionService conversionService = new DefaultConversionService();

    Set<Class> expectedRegisteredSupportConverters = new HashSet<Class>(17);

    expectedRegisteredSupportConverters.add(BigDecimalConverter.class);
    expectedRegisteredSupportConverters.add(BigIntegerConverter.class);
    expectedRegisteredSupportConverters.add(BooleanConverter.class);
    expectedRegisteredSupportConverters.add(ByteConverter.class);
    expectedRegisteredSupportConverters.add(CalendarConverter.class);
    expectedRegisteredSupportConverters.add(CharacterConverter.class);
    expectedRegisteredSupportConverters.add(DoubleConverter.class);
    expectedRegisteredSupportConverters.add(EnumConverter.class);
    expectedRegisteredSupportConverters.add(FloatConverter.class);
    expectedRegisteredSupportConverters.add(IdentifiableConverter.class);
    expectedRegisteredSupportConverters.add(IntegerConverter.class);
    expectedRegisteredSupportConverters.add(LongConverter.class);
    expectedRegisteredSupportConverters.add(NumberConverter.class);
    expectedRegisteredSupportConverters.add(ShortConverter.class);
    expectedRegisteredSupportConverters.add(StringConverter.class);
    expectedRegisteredSupportConverters.add(URIConverter.class);
    expectedRegisteredSupportConverters.add(URLConverter.class);

    for (Converter converter : conversionService) {
      assertTrue(String.format("Expected the Converter (%1$s) to registered in the DefaultConversionService!",
        converter.getClass().getName()), expectedRegisteredSupportConverters.remove(converter.getClass()));
    }

    assertTrue(String.format("Expected the registered, support Converters Set to be empty; but was (%1$s)!",
      expectedRegisteredSupportConverters), expectedRegisteredSupportConverters.isEmpty());
  }

}
