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

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Calendar;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.cp.elements.io.FileExtensionFilter;
import org.cp.elements.io.FileUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.convert.AbstractConversionService;
import org.cp.elements.util.convert.Converter;
import org.cp.elements.util.convert.support.StringConverter;

/**
 * The DefaultConversionService class is a Service class/components for performing value type conversions.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.convert.AbstractConversionService
 * @see org.cp.elements.util.convert.Converter
 * @see org.cp.elements.util.convert.support.StringConverter
 * @since 1.0.0
 * @link http://stackoverflow.com/questions/176527/how-can-i-enumerate-all-classes-in-a-package-and-add-them-to-a-list
 */
@SuppressWarnings("unused")
public class DefaultConversionService extends AbstractConversionService {

  protected static final Class CONVERTER_CLASS = StringConverter.class;

  protected static final Package CONVERTERS_PACKAGE = CONVERTER_CLASS.getPackage();

  protected static final String CLASS_FILE_EXTENSION = ".class";

  private volatile boolean defaultsEnabled = false;

  private final Map<Class, Object> defaultValues = new ConcurrentHashMap<Class, Object>(13, 0.95f);

  /**
   * Constructs a instance of the DefaultConversionService class to perform type conversions.
   */
  public DefaultConversionService() {
    String converterClassPathname = CONVERTER_CLASS.getName().replace(StringUtils.DOT_SEPARATOR, File.separator)
      .concat(CLASS_FILE_EXTENSION);

    URL converterClassResource = Thread.currentThread().getContextClassLoader().getResource(converterClassPathname);

    Assert.notNull(converterClassResource, "The URL for the Converter class ({0}) pathname ({1}) was null!",
      CONVERTER_CLASS.getName(), converterClassPathname);

    assert converterClassResource != null;

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
   * <p/>
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
   * <p/>
   * @param <T> the classification/type of objects represented by the Class.
   * @param type the Class type to set the default value for.
   * @param defaultValue the default value for the specified Class type.
   * @throws NullPointerException if the Class type is null.
   * @see java.lang.Class
   */
  public <T> void setDefaultValue(final Class<T> type, final T defaultValue) {
    Assert.notNull(type, "The Class type to set the default value for cannot be null!");
    defaultValues.put(type, defaultValue);
  }

  /**
   * Unsets the default value for the specified Class type.
   * <p/>
   * @param type the Class type to remove the default value for.
   * @see java.lang.Class
   */
  public void unsetDefaultValue(final Class type) {
    defaultValues.remove(type);
  }

  /**
   * Determines whether default values will be used during conversion if the value to convert is null.
   * <p/>
   * @return a boolean value indicating whether to use default values during conversion when the value to convert
   * is null.
   */
  public boolean isDefaultValuesEnabled() {
    return defaultsEnabled;
  }

  /**
   * Sets whether default values will be used during conversion if the value to convert is null.
   * <p/>
   * @param defaultsEnabled a boolean value to indicate whether to use default values during conversion when
   * the value to convert is null.
   */
  public void setDefaultValuesEnabled(final boolean defaultsEnabled) {
    this.defaultsEnabled = defaultsEnabled;
  }

  /**
   * Determines whether the default value for the specified Class type should be used as the conversion value.
   * <p/>
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
   * <p/>
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
    return (useDefault(value, toType) ? getDefaultValue(toType) :  super.convert(value, toType));
  }

  /**
   * The ValueGenerator interface defines contract for implementing objects to generate default values upon request.
   * <p/>
   * @param <T> the type of value to generate.
   */
  public static interface ValueGenerator<T> {
    public T generateValue();
  }

  /**
   * The CalendarValueGenerator class is a ValueGenerator that creates an instance of Calendar with the
   * current date/time.
   * <p/>
   * @see java.util.Calendar
   */
  public static class CalendarValueGenerator implements ValueGenerator<Calendar> {
    @Override
    public Calendar generateValue() {
      return Calendar.getInstance();
    }
  }

}
