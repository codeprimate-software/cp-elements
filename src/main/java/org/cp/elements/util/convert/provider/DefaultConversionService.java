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

package org.cp.elements.util.convert.provider;

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.cp.elements.io.FileExtensionFilter;
import org.cp.elements.io.FileUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.AbstractConversionService;
import org.cp.elements.util.convert.Converter;
import org.cp.elements.util.convert.support.StringConverter;

/**
 * The DefaultConversionService class is a Service class/component that performs value type conversions.
 *
 * @author John J. Blum
 * @see org.cp.elements.util.convert.AbstractConversionService
 * @see org.cp.elements.util.convert.Converter
 * @see org.cp.elements.util.convert.support.StringConverter
 * @see <a href="http://stackoverflow.com/questions/176527/how-can-i-enumerate-all-classes-in-a-package-and-add-them-to-a-list">How can I enumerate all classes in a package and add them to a List?</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class DefaultConversionService extends AbstractConversionService {

  protected static final Class CONVERTER_CLASS = StringConverter.class;

  protected static final Package CONVERTERS_PACKAGE = CONVERTER_CLASS.getPackage();

  protected static final String CLASS_FILE_EXTENSION = ".class";

  private volatile boolean defaultsEnabled = false;

  private final Map<Class, Object> defaultValues = Collections.synchronizedMap(new HashMap<>(13, 0.95f));

  /**
   * Constructs a instance of the DefaultConversionService class to perform type conversions.
   */
  public DefaultConversionService() {
    String converterClassPathname = CONVERTER_CLASS.getName().replace(StringUtils.DOT_SEPARATOR, File.separator)
      .concat(CLASS_FILE_EXTENSION);

    URL converterClassResource = Thread.currentThread().getContextClassLoader().getResource(converterClassPathname);

    Assert.notNull(converterClassResource, "The URL for the Converter class ({0}) pathname ({1}) was null!",
      CONVERTER_CLASS.getName(), converterClassPathname);

    File convertersPackageDirectory;

    try {
      convertersPackageDirectory = new File(converterClassResource.toURI()).getParentFile();
      Assert.isTrue(convertersPackageDirectory.isDirectory(), "The converters package directory ({0}) does not exist!",
        convertersPackageDirectory);
    }
    catch (URISyntaxException e) {
      throw new RuntimeException(String.format("Failed to create a File reference to the converters package directory (%1$s)!",
        converterClassPathname.substring(0, converterClassPathname.lastIndexOf(File.separator) + 1)), e);
    }

    for (File classFile : convertersPackageDirectory.listFiles(new FileExtensionFilter(CLASS_FILE_EXTENSION))) {
      String className = CONVERTERS_PACKAGE.getName().concat(StringUtils.DOT_SEPARATOR).concat(
        FileUtils.getNameWithoutExtension(classFile));

      Class classType = ClassUtils.loadClass(className);

      if (ClassUtils.assignableTo(classType, Converter.class)) {
        try {
          Converter<?, ?> converter = (Converter<?, ?>) classType.newInstance();
          register(converter);
        }
        catch (Exception ignore) {
          ignore.printStackTrace(System.err);
          // TODO log warning!
        }
      }
    }

    initDefaultValues();
  }

  /**
   * Initializes the default values per Class type to use when the value to convert is null.
   */
  private void initDefaultValues() {
    defaultValues.put(BigDecimal.class, new BigDecimal(0.0d));
    defaultValues.put(BigInteger.class, new BigInteger("0"));
    defaultValues.put(Boolean.class, false);
    defaultValues.put(Byte.class, (byte) 0);
    defaultValues.put(Calendar.class, new CalendarValueGenerator());
    defaultValues.put(Character.class, '\0');
    defaultValues.put(Double.class, 0.0d);
    defaultValues.put(Float.class, 0.0f);
    defaultValues.put(Integer.class, 0);
    defaultValues.put(Long.class, 0l);
    defaultValues.put(Number.class, null);
    defaultValues.put(Short.class, (short) 0);
    defaultValues.put(String.class, null);
  }

  /**
   * Gets the default value for the specified Class type.
   *
   * @param <T> the classification/type of objects represented by the Class.
   * @param type the Class type to get the default value for.
   * @return the default value for the specified Class type.
   * @see ValueGenerator
   * @see java.lang.Class
   */
  public <T> T getDefaultValue(final Class<T> type) {
    Object value = defaultValues.get(type);

    if (value instanceof ValueGenerator) {
      value = ((ValueGenerator) value).generateValue();
    }

    return type.cast(value);
  }

  /**
   * Sets the default value for the specified Class type.
   *
   * @param <T> the classification/type of objects represented by the Class.
   * @param type the Class type to set the default value for.
   * @param defaultValue the default value for the specified Class type.
   * @throws NullPointerException if the Class type is null.
   * @see #setDefaultValue(Class, org.cp.elements.util.convert.provider.DefaultConversionService.ValueGenerator)
   * @see java.lang.Class
   */
  public <T> void setDefaultValue(final Class<T> type, final T defaultValue) {
    Assert.notNull(type, "The Class type to set the default value for cannot be null!");
    defaultValues.put(type, defaultValue);
  }

  /**
   * Sets the default value for the specified Class type using value generation.
   *
   * @param <T> the classification/type of objects represented by the Class.
   * @param type the Class type to set the default value for.
   * @param valueGenerator the ValueGenerator used to generate default values for the specified Class type.
   * @throws NullPointerException if the Class type is null.
   * @see #setDefaultValue(Class, Object)
   * @see java.lang.Class
   * @see org.cp.elements.util.convert.provider.DefaultConversionService.ValueGenerator
   */
  public <T> void setDefaultValue(final Class<T> type, final ValueGenerator<T> valueGenerator) {
    Assert.notNull(type, "The Class type to set the value generator for cannot be null!");
    defaultValues.put(type, valueGenerator);
  }

  /**
   * Unsets the default value for the specified Class type.
   *
   * @param type the Class type to remove the default value for.
   * @see java.lang.Class
   */
  public void unsetDefaultValue(final Class type) {
    defaultValues.remove(type);
  }

  /**
   * Determines whether default values will be used during conversion if the value to convert is null.
   *
   * @return a boolean value indicating whether to use default values during conversion when the value to convert
   * is null.
   */
  public boolean isDefaultValuesEnabled() {
    return defaultsEnabled;
  }

  /**
   * Sets whether default values will be used during conversion if the value to convert is null.
   *
   * @param defaultsEnabled a boolean value to indicate whether to use default values during conversion when
   * the value to convert is null.
   */
  public void setDefaultValuesEnabled(final boolean defaultsEnabled) {
    this.defaultsEnabled = defaultsEnabled;
  }

  /**
   * Determines whether the default value for the specified Class type should be used as the conversion value.
   *
   * @param value the Object value to convert.
   * @param toType the Class type to convert the value to.
   * @return a boolean value indicating whether the default value of the specified Class type should be used.
   */
  protected boolean useDefault(final Object value, final Class toType) {
    return (isDefaultValuesEnabled() && value == null && defaultValues.containsKey(toType));
  }

  /**
   * Converts the Object value into a value of the target Class type.  If the Object value is null and default values
   * are enabled, then the default value based on the Class type to convert to will be returned if the Class type
   * has been set with a default value.
   *
   * @param <T> the target Class type for the conversion.
   * @param value the Object value to convert.
   * @param toType the Class type to convert the Object value into.
   * @return an instance of the Object value converted into a value of the target Class type, or a default value if
   * the value is null, default values are enabled and the Class type has been set with a default value.
   * @throws org.cp.elements.util.convert.ConversionException if converting the Object value into a value of the target Class type results in error.
   * @see #getDefaultValue(Class)
   * @see java.lang.Class
   * @see org.cp.elements.util.convert.AbstractConversionService#convert(Object, Class)
   * @see org.cp.elements.util.convert.Converter#convert(Object)
   */
  @Override
  public <T> T convert(final Object value, final Class<T> toType) {
    return (useDefault(value, toType) ? getDefaultValue(toType) : super.convert(value, toType));
  }

  /**
   * The ValueGenerator interface defines contract for implementing objects to generate default values upon request.
   *
   * @param <T> the type of value to generate.
   */
  public interface ValueGenerator<T> {
    T generateValue();
  }

  /**
   * The CalendarValueGenerator class is a ValueGenerator that creates an instance of Calendar with the
   * current date/time.
   *
   * @see java.util.Calendar
   */
  public static class CalendarValueGenerator implements ValueGenerator<Calendar> {
    @Override
    public Calendar generateValue() {
      return Calendar.getInstance();
    }
  }

}
