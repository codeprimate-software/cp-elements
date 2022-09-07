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
package org.cp.elements.data.conversion.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Supplier;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.data.conversion.converters.BigDecimalConverter;
import org.cp.elements.data.conversion.converters.BigIntegerConverter;
import org.cp.elements.data.conversion.converters.BooleanConverter;
import org.cp.elements.data.conversion.converters.ByteConverter;
import org.cp.elements.data.conversion.converters.CharacterConverter;
import org.cp.elements.data.conversion.converters.DateConverter;
import org.cp.elements.data.conversion.converters.DoubleConverter;
import org.cp.elements.data.conversion.converters.EnumConverter;
import org.cp.elements.data.conversion.converters.FloatConverter;
import org.cp.elements.data.conversion.converters.IdentifiableConverter;
import org.cp.elements.data.conversion.converters.IntegerConverter;
import org.cp.elements.data.conversion.converters.LongConverter;
import org.cp.elements.data.conversion.converters.NumberConverter;
import org.cp.elements.data.conversion.converters.ShortConverter;
import org.cp.elements.data.conversion.converters.StringConverter;
import org.cp.elements.data.conversion.converters.URIConverter;
import org.cp.elements.data.conversion.converters.URLConverter;
import org.cp.elements.enums.Gender;
import org.cp.elements.enums.Race;
import org.cp.elements.lang.Identifiable;
import org.junit.Test;

/**
 * Unit tests for {@link SimpleConversionService}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.conversion.provider.SimpleConversionService
 * @since 1.0.0
 */
public class SimpleConversionServiceTests {

  private final SimpleConversionService conversionService = new SimpleConversionService();

  @Test
  public void constructAndRegisterConvertersIsSuccessful() {

    Set<Class<?>> expectedRegisteredConverters = new HashSet<>(17);

    expectedRegisteredConverters.add(BigDecimalConverter.class);
    expectedRegisteredConverters.add(BigIntegerConverter.class);
    expectedRegisteredConverters.add(BooleanConverter.class);
    expectedRegisteredConverters.add(ByteConverter.class);
    expectedRegisteredConverters.add(DateConverter.class);
    expectedRegisteredConverters.add(CharacterConverter.class);
    expectedRegisteredConverters.add(DoubleConverter.class);
    expectedRegisteredConverters.add(EnumConverter.class);
    expectedRegisteredConverters.add(FloatConverter.class);
    expectedRegisteredConverters.add(IdentifiableConverter.class);
    expectedRegisteredConverters.add(IntegerConverter.class);
    expectedRegisteredConverters.add(LongConverter.class);
    expectedRegisteredConverters.add(NumberConverter.class);
    expectedRegisteredConverters.add(ShortConverter.class);
    expectedRegisteredConverters.add(StringConverter.class);
    expectedRegisteredConverters.add(URIConverter.class);
    expectedRegisteredConverters.add(URLConverter.class);

    for (Converter<?, ?> converter : this.conversionService) {
      assertThat(expectedRegisteredConverters.remove(converter.getClass()))
        .describedAs("Converter [%s] was not registered with the SimpleConversionService", converter)
        .isTrue();
    }

    assertThat(expectedRegisteredConverters)
      .describedAs("Converters [%s] were not registered", expectedRegisteredConverters)
      .isEmpty();
  }

  @Test
  public void enableAndDisableDefaultValuesIsSuccessful() {

    assertThat(this.conversionService.isDefaultValuesEnabled()).isFalse();

    this.conversionService.setDefaultValuesEnabled(true);

    assertThat(this.conversionService.isDefaultValuesEnabled()).isTrue();

    this.conversionService.setDefaultValuesEnabled(false);

    assertThat(this.conversionService.isDefaultValuesEnabled()).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void initializeDefaultValuesIsSuccessful() {

    assertThat(this.conversionService.isDefaultValuesEnabled()).isFalse();
    assertThat(this.conversionService.getDefaultValue(BigDecimal.class)).isEqualTo(new BigDecimal("0.0"));
    assertThat(this.conversionService.getDefaultValue(BigInteger.class)).isEqualTo(new BigInteger("0"));
    assertThat(this.conversionService.getDefaultValue(Boolean.class)).isFalse();
    assertThat(this.conversionService.getDefaultValue(Byte.class)).isEqualTo((byte) 0);
    assertThat(this.conversionService.getDefaultValue(Calendar.class)).isNotNull();
    assertThat(this.conversionService.getDefaultValue(Character.class)).isEqualTo('\0');
    assertThat(this.conversionService.getDefaultValue(Double.class)).isEqualTo(0.0d);
    assertThat(this.conversionService.getDefaultValue(Enum.class)).isNull();
    assertThat(this.conversionService.getDefaultValue(Float.class)).isEqualTo(0.0f);
    assertThat(this.conversionService.getDefaultValue(Identifiable.class)).isNull();
    assertThat(this.conversionService.getDefaultValue(Integer.class)).isEqualTo(0);
    assertThat(this.conversionService.getDefaultValue(Long.class)).isEqualTo(0L);
    assertThat(this.conversionService.getDefaultValue(Number.class)).isNull();
    assertThat(this.conversionService.getDefaultValue(Short.class)).isEqualTo((short) 0);
    assertThat(this.conversionService.getDefaultValue(String.class)).isNull();
    assertThat(this.conversionService.isDefaultValuesEnabled()).isFalse();
  }

  @Test
  public void setGetAndUnsetDefaultValueForEnum() {

    assertThat(this.conversionService.getDefaultValue(Gender.class)).isNull();
    assertThat(this.conversionService.getDefaultValue(Race.class)).isNull();

    this.conversionService.setDefaultValue(Gender.class, Gender.FEMALE);
    this.conversionService.setDefaultValue(Race.class, Race.WHITE);

    assertThat(this.conversionService.getDefaultValue(Gender.class)).isEqualTo(Gender.FEMALE);
    assertThat(this.conversionService.getDefaultValue(Race.class)).isEqualTo(Race.WHITE);
    assertThat(this.conversionService.<Race>unsetDefaultValue(Race.class)).isEqualTo(Race.WHITE);
    assertThat(this.conversionService.getDefaultValue(Gender.class)).isEqualTo(Gender.FEMALE);
    assertThat(this.conversionService.getDefaultValue(Race.class)).isNull();

    this.conversionService.setDefaultValue(Gender.class, Gender.MALE);

    assertThat(this.conversionService.getDefaultValue(Gender.class)).isEqualTo(Gender.MALE);
    assertThat(this.conversionService.getDefaultValue(Race.class)).isNull();
    assertThat(this.conversionService.<Gender>unsetDefaultValue(Gender.class)).isEqualTo(Gender.MALE);
    assertThat(this.conversionService.getDefaultValue(Gender.class)).isNull();
    assertThat(this.conversionService.getDefaultValue(Race.class)).isNull();
  }

  @Test
  public void setGetAndUnsetDefaultValueForNumber() {

    assertThat(this.conversionService.getDefaultValue(Number.class)).isNull();

    this.conversionService.setDefaultValue(Number.class, 0.0d);

    assertThat(this.conversionService.getDefaultValue(Number.class)).isEqualTo(0.0d);

    this.conversionService.setDefaultValue(Number.class, 0L);

    assertThat(this.conversionService.getDefaultValue(Number.class)).isEqualTo(0L);

    this.conversionService.setDefaultValue(Number.class, 0.0f);

    assertThat(this.conversionService.getDefaultValue(Number.class)).isEqualTo(0.0f);

    this.conversionService.setDefaultValue(Number.class, 0);

    assertThat(this.conversionService.getDefaultValue(Number.class)).isEqualTo(0);
    assertThat(this.conversionService.<Integer>unsetDefaultValue(Number.class)).isEqualTo(0);
    assertThat(this.conversionService.getDefaultValue(Number.class)).isNull();
  }

  @Test
  public void setAndGetDefaultValueForNumber() {

    AtomicInteger counter = new AtomicInteger(0);

    this.conversionService.setDefaultValue(Number.class, (Supplier<Number>) counter::getAndIncrement);

    for (int count = 0; count < 10; count++) {
      assertThat(this.conversionService.getDefaultValue(Number.class)).isEqualTo(count);
    }

    assertThat(this.conversionService.<Supplier<Number>>unsetDefaultValue(Number.class).get()).isEqualTo(10);
    assertThat(this.conversionService.getDefaultValue(Number.class)).isNull();
  }

  @Test
  public void setGetAndUnsetDefaultValueForString() {

    assertThat(this.conversionService.getDefaultValue(String.class)).isNull();

    this.conversionService.setDefaultValue(String.class, "nil");

    assertThat(this.conversionService.getDefaultValue(String.class)).isEqualTo("nil");
    assertThat(this.conversionService.<String>unsetDefaultValue(String.class)).isEqualTo("nil");
    assertThat(this.conversionService.getDefaultValue(String.class)).isNull();
  }

  @Test(expected = IllegalArgumentException.class)
  public void setDefaultValueWithNullType() {

    try {
      this.conversionService.setDefaultValue(null, "test");
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Class type is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void setDefaultValueWithNullValue() {

    assertThat(this.conversionService.getDefaultValue(Integer.class)).isEqualTo(0);

    this.conversionService.setDefaultValue(Integer.class, (Integer) null);

    assertThat(this.conversionService.getDefaultValue(Integer.class)).isNull();
  }

  @Test
  @SuppressWarnings("all")
  public void useDefaultWhenValueIsNotNullReturnsFalse() {

    SimpleConversionService conversionServiceSpy = spy(this.conversionService);

    this.conversionService.setDefaultValuesEnabled(true);

    assertThat(this.conversionService.isDefaultValuesEnabled()).isTrue();
    assertThat(conversionServiceSpy.useDefault("test", String.class)).isFalse();

    verify(conversionServiceSpy, never()).isDefaultValuesEnabled();
  }

  @Test
  @SuppressWarnings("all")
  public void useDefaultWhenTypeIsNotPresentReturnsFalse() {

    SimpleConversionService conversionServiceSpy = spy(this.conversionService);

    this.conversionService.setDefaultValuesEnabled(true);

    assertThat(this.conversionService.isDefaultValuesEnabled()).isTrue();
    assertThat(conversionServiceSpy.useDefault(null, Gender.class)).isFalse();

    verify(conversionServiceSpy, times(1)).isDefaultValuesEnabled();
  }

  @Test
  @SuppressWarnings("all")
  public void useDefaultWhenDefaultValuesAreDisabledReturnsFalse() {

    SimpleConversionService conversionServiceSpy = spy(this.conversionService);

    assertThat(this.conversionService.isDefaultValuesEnabled()).isFalse();
    assertThat(conversionServiceSpy.useDefault(null, String.class)).isFalse();

    verify(conversionServiceSpy, times(1)).isDefaultValuesEnabled();
  }

  @Test
  @SuppressWarnings("all")
  public void useDefaultWhenValueIsNullDefaultValuesAreEnabledAndTypeIsPresentReturnsTrue() {

    SimpleConversionService conversionServiceSpy = spy(this.conversionService);

    when(conversionServiceSpy.isDefaultValuesEnabled()).thenReturn(true);

    assertThat(conversionServiceSpy.useDefault(null, String.class)).isTrue();

    verify(conversionServiceSpy, times(1)).isDefaultValuesEnabled();
  }

  @Test
  public void convert() {

    assertThat(this.conversionService.convert("true", Boolean.class)).isTrue();
    assertThat(this.conversionService.convert("X", Character.class)).isEqualTo('X');
    assertThat(this.conversionService.convert("1", Integer.class)).isEqualTo(1);
    assertThat(this.conversionService.convert(String.valueOf(Math.PI), Double.class)).isEqualTo(Math.PI);
    assertThat(this.conversionService.convert("test", String.class)).isEqualTo("test");
    assertThat(this.conversionService.convert("null", String.class)).isEqualTo("null");
    assertThat(this.conversionService.convert("nil", String.class)).isEqualTo("nil");
    assertThat(this.conversionService.convert(null, String.class)).isEqualTo("null");
  }

  @Test(expected = ConversionException.class)
  public void convertWithDefaultsDisabledThrowsException() {

    assertThat(this.conversionService.isDefaultValuesEnabled()).isFalse();

    try {
      this.conversionService.convert(null, Gender.class);
    }
    catch (ConversionException expected) {

      assertThat(expected).hasMessage("Cannot convert [null] into Object of type [%s]", Gender.class.getName());
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(this.conversionService.isDefaultValuesEnabled()).isFalse();
    }
  }

  @Test
  public void convertWithDefaultsEnabledPerformsConversion() {

    Date now = new Date();

    this.conversionService.setDefaultValuesEnabled(true);
    this.conversionService.setDefaultValue(Date.class, (Supplier<Date>) () -> now);
    this.conversionService.setDefaultValue(Gender.class, (Supplier<Gender>) () -> Gender.FEMALE);

    assertThat(conversionService.isDefaultValuesEnabled()).isTrue();
    assertThat(this.conversionService.convert(null, Boolean.class)).isFalse();
    assertThat(this.conversionService.convert(null, Character.class)).isEqualTo('\0');
    assertThat(this.conversionService.convert(null, Date.class)).isEqualTo(now);
    assertThat(this.conversionService.convert(null, Gender.class)).isEqualTo(Gender.FEMALE);
    assertThat(this.conversionService.convert(null, Integer.class)).isEqualTo(0);
    assertThat(this.conversionService.convert(null, Double.class)).isEqualTo(0.0d);
    assertThat(this.conversionService.convert(null, String.class)).isNull();
  }
}
