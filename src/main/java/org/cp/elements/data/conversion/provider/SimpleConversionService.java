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

package org.cp.elements.data.conversion.provider;

import static org.cp.elements.lang.ClassUtils.CLASS_FILE_EXTENSION;

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

import org.cp.elements.data.conversion.AbstractConversionService;
import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.data.conversion.Converter;
import org.cp.elements.data.conversion.converters.StringConverter;
import org.cp.elements.io.FileExtensionFilter;
import org.cp.elements.io.FileUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.service.annotation.Service;

/**
 * {@link SimpleConversionService} is an application {@link Service} class that performs {@link Class type} conversions
 * using pre-canned {@link Class type} {@link Converter Converters} defined in
 * {@link org.cp.elements.data.conversion.converters}.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.AbstractConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @see org.cp.elements.data.conversion.converters.StringConverter
 * @see <a href="http://stackoverflow.com/questions/176527/how-can-i-enumerate-all-classes-in-a-package-and-add-them-to-a-list">How can I enumerate all classes in a package and add them to a List?</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SimpleConversionService extends AbstractConversionService {

  protected static final Class CONVERTER_CLASS = StringConverter.class;

  protected static final Package CONVERTERS_PACKAGE = CONVERTER_CLASS.getPackage();

  private volatile boolean defaultsEnabled = false;

  private final Map<Class, Object> defaultValues =
    Collections.synchronizedMap(new HashMap<>(13, 0.95f));

  /**
   * Constructs a new instance of {@link SimpleConversionService} initialized with all the {@link Converter Converters}
   * defined in {@link org.cp.elements.data.conversion.converters}.
   *
   * @throws IllegalArgumentException if the resource {@link URL} of a chosen {@link Converter} cannot be resolved.
   */
  public SimpleConversionService() {

    String converterClassResourceName = toResourceName(CONVERTER_CLASS);

    URL converterClassResourceLocation = resolveResourceLocation(converterClassResourceName);

    Assert.notNull(converterClassResourceLocation,
      "Could not resolve URL for Converter class [%1$s] having resource name [%2$s]",
        CONVERTER_CLASS.getName(), converterClassResourceName);

    File convertersPackageDirectory;

    try {
      convertersPackageDirectory = new File(converterClassResourceLocation.toURI()).getParentFile();

      Assert.isTrue(convertersPackageDirectory.isDirectory(),
        "Directory for Converters package [%s] does not exist", convertersPackageDirectory);
    }
    catch (URISyntaxException cause) {
      throw new RuntimeException(String.format("Failed to create a File reference to the converters package directory (%1$s)!",
        converterClassResourceName.substring(0, converterClassResourceName.lastIndexOf(File.separator) + 1)), cause);
    }

    for (File classFile : convertersPackageDirectory.listFiles(new FileExtensionFilter(CLASS_FILE_EXTENSION))) {

      String className = CONVERTERS_PACKAGE.getName().concat(StringUtils.DOT_SEPARATOR).concat(
        FileUtils.getName(classFile));

      Class classType = ClassUtils.loadClass(className);

      if (ClassUtils.assignableTo(classType, Converter.class)) {
        try {
          Converter<?, ?> converter = (Converter<?, ?>) classType.newInstance();
          register(converter);
        }
        catch (Exception ignore) {
          ignore.printStackTrace(System.err);
        }
      }
    }

    initDefaultValues();
  }

  private String toResourceName(Class<?> type) {
    return ObjectUtils.getResourceName(type);
  }

  private URL resolveResourceLocation(String resourceName) {
    //return getClass().getResource(resourceName);
    return Thread.currentThread().getContextClassLoader().getResource(resourceName);
  }

  /**
   * Initializes default values to use for a specific {@link Class} type
   * when the {@link Object value} to convert is {@literal null}.
   */
  private void initDefaultValues() {

    this.defaultValues.put(BigDecimal.class, new BigDecimal(0.0d));
    this.defaultValues.put(BigInteger.class, new BigInteger("0"));
    this.defaultValues.put(Boolean.class, false);
    this.defaultValues.put(Byte.class, (byte) 0);
    this.defaultValues.put(Calendar.class, CalendarValueSupplier.INSTANCE);
    this.defaultValues.put(Character.class, '\0');
    this.defaultValues.put(Double.class, 0.0d);
    this.defaultValues.put(Enum.class, null);
    this.defaultValues.put(Float.class, 0.0f);
    this.defaultValues.put(Integer.class, 0);
    this.defaultValues.put(Identifiable.class, null);
    this.defaultValues.put(Long.class, 0L);
    this.defaultValues.put(Short.class, (short) 0);
    this.defaultValues.put(String.class, null);
  }

  /**
   * Unsets the {@link Object default value} for the specified {@link Class type}.
   *
   * @param <T> {@link Class type} of the {@link Object default value}.
   * @param type {@link Class type} to remove the {@link Object default value} for.
   * @see #setDefaultValue(Class, Supplier)
   * @see #setDefaultValue(Class, Object)
   * @see java.lang.Class
   */
  @SuppressWarnings("unchecked")
  public <T> T unsetDefaultValue(Class<?> type) {
    return (T) this.defaultValues.remove(type);
  }

  /**
   * Sets the {@link Object default value} for the specified {@link Class type}.
   *
   * @param <T> {@link Class classification/type} of objects which may have {@link Object default values}.
   * @param type {@link Class} type to define the {@link Object default value} for.
   * @param defaultValue {@link Object default value} to use for the specified {@link Class} type.
   * @throws IllegalArgumentException if {@link Class type} is {@literal null}.
   * @see #setDefaultValue(Class, Supplier)
   * @see java.lang.Class
   */
  public <T> void setDefaultValue(Class<T> type, T defaultValue) {
    Assert.notNull(type, "Class type is required");
    this.defaultValues.put(type, defaultValue);
  }

  /**
   * Sets the {@link Supplier} used to supply a {@link Object default value} for the specified {@link Class type}.
   *
   * This overloaded method is useful for dynamically generating new {@link Object values} at runtime,
   * such as date/time values
   *
   * @param <T> {@link Class classification/type} of objects which may have {@link Object default values}.
   * @param type {@link Class} type to define the {@link Object default value} for.
   * @param defaultValueSupplier {@link Supplier} used to supply a {@link Object default value}
   * for the specified {@link Class} type.
   * @throws IllegalArgumentException if {@link Class type} is {@literal null}.
   * @see #setDefaultValue(Class, Object)
   * @see java.util.function.Supplier
   * @see java.lang.Class
   */
  public <T> void setDefaultValue(Class<T> type, Supplier<T> defaultValueSupplier) {
    Assert.notNull(type, "Class type is required");
    this.defaultValues.put(type, defaultValueSupplier);
  }

  /**
   * Gets the {@link Object default value} for the specified {@link Class type}.
   *
   * @param <T> {@link Class classification/type} of objects which may have {@link Object default values}.
   * @param type {@link Class} type to get the {@link Object default value} for.
   * @return the {@link Object default value} for the specified {@link Class type}.
   * @see java.lang.Class
   */
  public <T> T getDefaultValue(Class<T> type) {

    Object value = this.defaultValues.get(type);

    if (value instanceof Supplier) {
      value = ((Supplier) value).get();
    }

    return type.cast(value);
  }

  /**
   * Sets whether default values will be used during conversion if the value to convert is null.
   *
   * @param defaultsEnabled a boolean value to indicate whether to use default values during conversion when
   * the value to convert is null.
   */
  public void setDefaultValuesEnabled(boolean defaultsEnabled) {
    this.defaultsEnabled = defaultsEnabled;
  }

  /**
   * Determines whether default values will be used during conversion if the value to convert is null.
   *
   * @return a boolean value indicating whether to use default values during conversion when the value to convert
   * is null.
   */
  public boolean isDefaultValuesEnabled() {
    return this.defaultsEnabled;
  }

  /**
   * Converts the {@link Object value} into an {@link Object} of the speicfied target {@link Class type}.
   *
   * If {@link Object value} is {@literal null} and {@link Object default values} have been
   * {@link #isDefaultValuesEnabled()} enabled}, then the {@link Object default value} will be based on
   * the {@link Class type} to convert to and the {@link Class type} has been initialized with
   * a {@link Object default value}.
   *
   * @param <T> {@link Class target type} of the conversion.
   * @param value {@link Object value} to convert.
   * @param toType {@link Class type} to convert the {@link Object value} into.
   * @return the {@link Object value} converted into an instance of the desired {@link Class target type},
   * or a {@link Object default value} if the {@link Object converted value} is {@literal null},
   * {@link Object default values} are {@link #isDefaultValuesEnabled()} enabled} and the {@link Class type}
   * has been set with a {@link Object default value}.
   * @throws ConversionException if converting the {@link Object value} into an instance of
   * the {@link Class target type} results in error.
   * @see org.cp.elements.data.conversion.AbstractConversionService#convert(Object, Class)
   * @see org.cp.elements.data.conversion.Converter#convert(Object)
   * @see #useDefault(Object, Class)
   * @see #getDefaultValue(Class)
   * @see java.lang.Class
   */
  @Override
  public <T> T convert(Object value, final Class<T> toType) {
    return useDefault(value, toType) ? getDefaultValue(toType) : super.convert(value, toType);
  }

  /**
   * Determines whether the {@link Object default value} for the specified {@link Class type}
   * should be used as the converted {@link Object value} when the converted {@link Object value}
   * is {@literal null}.
   *
   * @param value {@link Object value} to convert.
   * @param toType {@link Class type} to convert the {@link Object value} into.
   * @return a boolean value indicating whether the {@link Object default value} for the specified {@link Class type}
   * should be used as the converted {@link Object value} when the converted {@link Object value}
   * is {@literal null}.
   * @see #isDefaultValuesEnabled()
   */
  protected boolean useDefault(Object value, Class<?> toType) {
    return value == null && isDefaultValuesEnabled() && this.defaultValues.containsKey(toType);
  }

  /**
   * The {@link CalendarValueSupplier} class is a {@link Supplier} that creates a new instance of {@link Calendar}
   * with the current date/time for every invocaton of {@link Supplier#get()}.
   *
   * @see java.util.function.Supplier
   * @see java.util.Calendar
   */
  public static class CalendarValueSupplier implements Supplier<Calendar> {

    public static final CalendarValueSupplier INSTANCE = new CalendarValueSupplier();

    @Override
    public Calendar get() {
      return Calendar.getInstance();
    }
  }
}
