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

import static java.util.Arrays.stream;
import static org.cp.elements.data.conversion.AbstractConverterRegistry.ConverterDescriptor.describe;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Registry;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * The {@link AbstractConverterRegistry} class is an abstract base class for a registry of {@link Converter Converters}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.cp.elements.data.conversion.ConverterRegistry
 * @see org.cp.elements.lang.Registry
 * @see <a href="http://stackoverflow.com/questions/8040362/class-name-of-type-parameters-in-java">Class name of type parameters in Java?</a>
 * @since 1.0.0
 */
public abstract class AbstractConverterRegistry implements ConverterRegistry {

  private final Map<ConverterDescriptor, Converter<?, ?>> registry = new ConcurrentHashMap<>();

  /**
   * Returns a reference to the underlying data structure used by this {@link ConverterRegistry}
   * to manage the registry of {@link Converter Converters}.
   *
   * @return a {@link Map mapping} of {@link ConverterDescriptor ConverterDescriptors}
   * describing the {@link Converter} to the registered {@link Converter}.
   * @see org.cp.elements.data.conversion.AbstractConverterRegistry.ConverterDescriptor
   * @see org.cp.elements.data.conversion.Converter
   * @see java.util.Map
   */
  protected Map<ConverterDescriptor, Converter<?, ?>> getRegistry() {
    return this.registry;
  }

  /**
   * Iterates over the registered {@link Converter Converters} in this {@link ConverterRegistry}.
   *
   * @return an {@link Iterator} over the {@link Converter Converters} registered in this {@link ConverterRegistry}.
   * @see org.cp.elements.data.conversion.Converter
   * @see java.util.Iterator
   */
  @Override
  public Iterator<Converter<?, ?>> iterator() {
    return getRegistry().values().iterator();
  }

  /**
   * Registers the {@link Converter} with this {@link ConverterRegistry} making it available
   * to perform {@link Class type} conversions.
   *
   * Any existing, registered {@link Converter} converting from the same {@link Class source type}
   * to the same {@link Class target type} will simply be overridden with the incoming {@link Converter} registration.
   *
   * @param converter {@link Converter} to register.
   * @return this {@link ConverterRegistry}.
   * @throws IllegalArgumentException if {@link Converter} is {@literal null}.
   * @see org.cp.elements.data.conversion.Converter
   * @see #unregister(Converter)
   */
  @Override
  @SuppressWarnings("unchecked")
  public <R extends Registry<Converter<?, ?>>> R register(Converter<?, ?> converter) {

    Assert.notNull(converter, "Converter is required");

    getRegistry().put(describe(converter), converter);

    if (this instanceof ConversionService) {
      converter.setConversionService((ConversionService) this);
    }

    return (R) this;
  }

  /**
   * Unregisters the {@link Converter} from this {@link ConverterRegistry}.
   *
   * @param converter {@link Converter} to unregister.
   * @return this {@link ConverterRegistry}.
   * @see org.cp.elements.data.conversion.Converter
   * @see #register(Converter)
   */
  @NullSafe
  @Override
  @SuppressWarnings("all")
  public <R extends Registry<Converter<?, ?>>> R unregister(Converter<?, ?> converter) {

    for (Iterator<Converter<?, ?>> iterator = iterator(); iterator.hasNext(); ) {
      if (iterator.next().equals(converter)) {
        iterator.remove();
        converter.setConversionService(null);
      }
    }

    return (R) this;
  }

  /**
   * The {@link ConverterDescriptor} class encapsulates meta-data describing the {@link Class from type}
   * {@link Class to type} conversion performed by the {@link Converter}.
   *
   * @see org.cp.elements.data.conversion.Converter
   * @see java.lang.Class
   */
  protected static class ConverterDescriptor {

    private final Class<?> fromType;
    private final Class<?> toType;

    private final Converter<?, ?> converter;

    /**
     * Factory method used to construct a new instance of {@link ConverterDescriptor} that describes
     * the {@link Converter} in order to determine what type of conversion
     * the {@link Converter} can perform.
     *
     * @param converter {@link Converter} to describe.
     * @return a {@link ConverterDescriptor} describing the {@link Converter} and the type of conversion.
     * @throws IllegalArgumentException if {@link Converter} is {@literal null}.
     * @see org.cp.elements.data.conversion.AbstractConversionService.ConverterDescriptor
     * @see org.cp.elements.data.conversion.Converter
     */
    protected static ConverterDescriptor describe(Converter<?, ?> converter) {

      Assert.notNull(converter, "Converter is required");

      ParameterizedType parameterizedType =
        stream(nullSafeArray(converter.getClass().getGenericInterfaces(), Type.class))
          .filter(ConverterDescriptor::isParameterizedConverterType)
          .findFirst()
          .map(it -> (ParameterizedType) it)
          .orElseGet(() -> {

            Type genericSuperclass = converter.getClass().getGenericSuperclass();

            return isParameterizedConverterType(genericSuperclass) ? (ParameterizedType) genericSuperclass : null;
          });

      Assert.notNull(parameterizedType, "Converter [%s] was not properly parameterized",
        converter.getClass().getName());

      Class<?> fromType = ClassUtils.toRawType(parameterizedType.getActualTypeArguments()[0]);
      Class<?> toType = ClassUtils.toRawType(parameterizedType.getActualTypeArguments()[1]);

      return new ConverterDescriptor(converter, fromType, toType);
    }

    /**
     * Determines whether the {@link Type} is a generic, parameterized {@link Converter} {@link Class type}, such as
     * by implementing the {@link Converter} interface or extending the {@link AbstractConverter} base class.
     *
     * @param type {@link Type} to evaluate.
     * @return a boolean if the {@link Type} represents a generic, parameterized {@link Converter} {@link Class type}.
     * @see java.lang.reflect.Type
     */
    private static boolean isParameterizedConverterType(Type type) {
      return type instanceof ParameterizedType && ClassUtils.assignableTo(ClassUtils.toRawType(type), Converter.class);
    }

    /**
     * Constructs a new instance of the {@link ConverterDescriptor} describing the {@link Class from type}
     * {@link Class to type} conversion performed by {@link Converter}.
     *
     * @param converter {@link Converter} being described.
     * @param fromType {@link Class type} to convert from.
     * @param toType {@link Class type} to convert to.
     * @throws IllegalArgumentException if the {@link Converter}, {@link Class from type}
     * or {@link Class to type} are {@literal null}.
     * @see org.cp.elements.data.conversion.Converter
     * @see java.lang.Class
     */
    protected ConverterDescriptor(Converter<?, ?> converter, Class<?> fromType, Class<?> toType) {

      Assert.notNull(converter, "Converter is required");
      Assert.notNull(fromType, "From type is required");
      Assert.notNull(toType, "To type is required");

      this.converter = converter;
      this.fromType = fromType;
      this.toType = toType;
    }

    /**
     * Return a reference to the described {@link Converter}.
     *
     * @return a reference to the described {@link Converter}.
     * @see org.cp.elements.data.conversion.Converter
     */
    public Converter<?, ?> getConverter() {
      return this.converter;
    }

    /**
     * {@link Class Type} to convert from.
     *
     * @return the {@link Class type} to convert from.
     * @see java.lang.Class
     */
    public Class getFromType() {
      return this.fromType;
    }

    /**
     * {@link Class Type} to convert to.
     *
     * @return the {@link Class type} to convert to.
     * @see java.lang.Class
     */
    public Class getToType() {
      return this.toType;
    }

    /**
     * Determines whether the {@link Converter} performs an exact {@link Class type} conversion.
     *
     * The conversion is exact if the {@link Class qualified target type} matches the {@link Class to type}
     * of the {@link Converter}.
     *
     * @param targetType desired {@link Class target type} of the conversion.
     * @return a boolean indicating whether the {@link Converter} performs an exact {@link Class type} conversion.
     * @see java.lang.Class
     */
    public boolean isExactConversion(Class targetType) {
      return getToType().equals(targetType);
    }

    @Override
    public boolean equals(final Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof ConverterDescriptor)) {
        return false;
      }

      ConverterDescriptor that = (ConverterDescriptor) obj;

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
      return String.format("%1$s converts from [%2$s] to [%3$s]", getConverter().getClass().getName(),
        getFromType().getName(), getToType().getName());
    }
  }
}
