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

package org.cp.elements.data.conversion;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;

/**
 * The AbstractConversionService class is an abstract base class encapsulating functionality common to all service
 * classes/components that perform value type conversions.
 *
 * @author John J. Blum
 * @see org.cp.elements.data.conversion.ConversionService
 * @see org.cp.elements.data.conversion.Converter
 * @see <a href="http://stackoverflow.com/questions/8040362/class-name-of-type-parameters-in-java">Class name of type parameters in Java?</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConversionService implements ConversionService {

  private final Map<ConverterDescriptor, Converter> registry = Collections.synchronizedMap(
    new HashMap<>(19, 0.95f));

  /**
   * Gets a raw reference to the registry of Converters registered with this ConversionService.
   *
   * @return a mapping of ConverterDescriptors describing the registered Converter.
   * @see java.util.Map
   * @see org.cp.elements.data.conversion.AbstractConversionService.ConverterDescriptor
   * @see org.cp.elements.data.conversion.Converter
   */
  protected Map<ConverterDescriptor, Converter> getRegistry() {
    return registry;
  }

  /**
   * Determines whether this ConversionService can convert a given object into a value of the desired Class type.
   *
   * @param value the object to convert into a value of the target Class type.
   * @param toType the Class type to convert the Object value into.
   * @return a boolean value indicating whether this ConversionService can convert the object into a value of the
   * desired Class type.
   * @see #canConvert(Class, Class)
   */
  public boolean canConvert(final Object value, final Class<?> toType) {
    return (value != null && canConvert(value.getClass(), toType));
  }

  /**
   * Determines whether this ConversionService can convert values from a given Class type into the desired Class type.
   *
   * @param fromType the Class type to convert from.
   * @param toType the Class type to convert to.
   * @return a boolean value indicating whether this ConversionService can convert values from a given Class type
   * into the desired Class type.
   * @see #canConvert(Object, Class)
   * @see org.cp.elements.data.conversion.Converter#canConvert(Class, Class)
   */
  @SuppressWarnings("unchecked")
  public boolean canConvert(final Class<?> fromType, final Class<?> toType) {
    for (Converter converter : this) {
      if (converter.canConvert(fromType, toType)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Converts the Object value into a value of the target Class type.
   *
   * @param <T> the target Class type for the conversion.
   * @param value the Object value to convert.
   * @param toType the Class type to convert the Object value into.
   * @return an instance of the Object value converted into a value of the target Class type.
   * @throws ConversionException if converting the Object value into a value of the target Class type results in error.
   * @see java.lang.Class
   * @see org.cp.elements.data.conversion.Converter#convert(Object)
   * @see org.cp.elements.data.conversion.Converter#convert(Object, Class)
   */
  @SuppressWarnings("unchecked")
  public <T> T convert(final Object value, final Class<T> toType) {
    for (ConverterDescriptor descriptor : getRegistry().keySet()) {
      Converter converter = descriptor.getConverter();

      if (converter.canConvert(ClassUtils.getClass(value), toType)) {
        if (descriptor.isExactConversion(toType)) {
          return toType.cast(converter.convert(value));
        }
        else {
          return toType.cast(converter.convert(value, toType));
        }
      }
    }

    throw new ConversionException(String.format("Failed to convert value (%1$s) into an object of type (%2$s)!",
      value, toType.getName()));
  }

  /**
   * Describes the Converter in order to determine what type of conversion the Converter can perform.
   *
   * @param converter the Converter to describe.
   * @return a ConverterDescriptor describing the Converter and the type of conversion.
   * @see org.cp.elements.data.conversion.AbstractConversionService.ConverterDescriptor
   * @see org.cp.elements.data.conversion.Converter
   */
  protected ConverterDescriptor describe(final Converter<?, ?> converter) {
    ParameterizedType parameterizedType = null;

    for (Type genericInterface : converter.getClass().getGenericInterfaces()) {
      if (isParameterizedConverterType(genericInterface)) {
        parameterizedType = (ParameterizedType) genericInterface;
        break;
      }
    }

    Type genericSuperclass = converter.getClass().getGenericSuperclass();

    if (parameterizedType == null && isParameterizedConverterType(genericSuperclass)) {
      parameterizedType = (ParameterizedType) genericSuperclass;
    }

    Assert.notNull(parameterizedType, new IllegalArgumentException(String.format(
      "The Converter (%1$s) does not directly implement the Converter interface or extend either the AbstractConverter or ConverterAdapter class!",
        converter.getClass().getName())));

    Class fromType = getRawClassType(parameterizedType.getActualTypeArguments()[0]);
    Class toType = getRawClassType(parameterizedType.getActualTypeArguments()[1]);

    return new ConverterDescriptor(converter, fromType, toType);
  }

  /**
   * Gets the raw Class type of the Type parameter.
   *
   * @param type the Type parameter to inspect.
   * @return a raw Class type for the Type parameter.
   * @see java.lang.Class
   * @see java.lang.reflect.ParameterizedType#getRawType()
   * @see java.lang.reflect.Type
   */
  protected Class getRawClassType(final Type type) {
    return Class.class.cast((type instanceof ParameterizedType ? ((ParameterizedType) type).getRawType() : type));
  }

  /**
   * Determines whether the type is a generic, parameterized Converter type, such as the Converter interface, or the
   * AbstractConverter or ConverterAdapter class.
   *
   * @param type the Type being evaluated as a generic, parameterized Converter type.
   * @return a boolean if the Type represents a generic, parameterized Converter type.
   * @see java.lang.reflect.Type
   * @see org.cp.elements.data.conversion.AbstractConverter
   * @see org.cp.elements.data.conversion.Converter
   * @see org.cp.elements.data.conversion.ConverterAdapter
   */
  @SuppressWarnings("all")
  protected boolean isParameterizedConverterType(final Type type) {
    return (type instanceof ParameterizedType && ((Converter.class.equals(((ParameterizedType) type).getRawType())
      || AbstractConverter.class.equals(((ParameterizedType) type).getRawType())
      || ConverterAdapter.class.equals(((ParameterizedType) type).getRawType()))));
  }

  /**
   * Iterates over the registered Converters in the ConversionService registry.
   *
   * @return a Iterator over the Converters registered in the ConversionService registry.
   * @see java.util.Iterator
   * @see org.cp.elements.data.conversion.Converter
   */
  public Iterator<Converter> iterator() {
    return Collections.unmodifiableCollection(getRegistry().values()).iterator();
  }

  /**
   * Registers the Converter with the registry making it available to perform type conversions.  Any existing,
   * registered Converter converting from the same source type to the same target type will simply be overridden
   * with the incoming Converter registration.
   *
   * @param converter the Converter to register with the registry.
   * @see #unregister(Converter)
   * @see org.cp.elements.data.conversion.Converter
   */
  public void register(final Converter<?, ?> converter) {
    Assert.notNull(converter, "The Converter to register with this ConversionService ({0}) cannot be null!",
      this.getClass().getName());

    getRegistry().put(describe(converter), converter);
    converter.setConversionService(this);
  }

  /**
   * Unregisters the Converter from the registry.
   *
   * @param converter the Converter to unregister from the registry.
   * @see #register(Converter)
   * @see org.cp.elements.data.conversion.Converter
   */
  public void unregister(final Converter<?, ?> converter) {
    for (ConverterDescriptor descriptor : getRegistry().keySet()) {
      if (descriptor.getConverter().equals(converter)) {
        if (converter.equals(getRegistry().remove(descriptor))) {
          converter.setConversionService(null);
        }
      }
    }
  }

  /**
   * The ConverterDescriptor class is a descriptor for a Converter to describe the conversion from/to type.
   */
  protected static class ConverterDescriptor {

    private final Class fromType;
    private final Class toType;

    private final Converter converter;

    protected ConverterDescriptor(final Converter converter, final Class fromType, final Class toType) {
      Assert.notNull(converter, "The Converter to describe cannot be null!");
      Assert.notNull(fromType, "The Class type the Converter converts from cannot be null!");
      Assert.notNull(toType, "The Class type the Converter converts to cannot be null!");

      this.converter = converter;
      this.fromType = fromType;
      this.toType = toType;
    }

    public Converter getConverter() {
      return converter;
    }

    public Class getFromType() {
      return fromType;
    }

    public Class getToType() {
      return toType;
    }

    public boolean isExactConversion(final Class toType) {
      return getToType().equals(toType);
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof ConverterDescriptor)) {
        return false;
      }

      final ConverterDescriptor that = (ConverterDescriptor) obj;

      return ObjectUtils.equals(this.getFromType(), that.getFromType())
        && ObjectUtils.equals(this.getToType(), that.getToType());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getFromType());
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getToType());
      return hashValue;
    }

    @Override
    public String toString() {
      return String.format("Converter (%1$s) converting from (%2$s) to (%3$s)", getConverter().getClass().getName(),
        getFromType().getName(), getToType().getName());
    }
  }

}
