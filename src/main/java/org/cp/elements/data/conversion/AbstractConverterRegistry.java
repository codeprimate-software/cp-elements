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
package org.cp.elements.data.conversion;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Registry;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.support.SmartComparator;
import org.cp.elements.util.ArrayUtils;

/**
 * Abstract base class for a registry of {@link Converter Converters}.
 *
 * @author John Blum
 * @see java.lang.reflect.ParameterizedType
 * @see java.lang.reflect.Type
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
   * <p>
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

    getRegistry().put(ConverterDescriptor.describe(converter), converter);

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
  @SuppressWarnings("unchecked")
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
   * Abstract Data Type used to encapsulate metadata describing the {@link Class from type} {@link Class to type}
   * conversion performed by the {@link Converter}.
   *
   * @see org.cp.elements.data.conversion.Converter
   * @see java.lang.Comparable
   * @see java.lang.Class
   */
  protected static class ConverterDescriptor implements Comparable<ConverterDescriptor> {

    /**
     * Factory method used to construct a new {@link ConverterDescriptor} that describes the {@link Converter}
     * in order to determine what type of conversion the {@link Converter} can perform.
     *
     * @param converter {@link Converter} to describe.
     * @return a {@link ConverterDescriptor} describing the {@link Converter} and the type of conversion.
     * @throws IllegalArgumentException if {@link Converter} is {@literal null}.
     * @see org.cp.elements.data.conversion.AbstractConversionService.ConverterDescriptor
     * @see org.cp.elements.data.conversion.Converter
     */
    @SuppressWarnings("all")
    protected static @NotNull ConverterDescriptor describe(@NotNull Converter<?, ?> converter) {

      Assert.notNull(converter, "Converter is required");

      ParameterizedType parameterizedType =
        Arrays.stream(ArrayUtils.nullSafeArray(converter.getClass().getGenericInterfaces(), Type.class))
          .filter(ConverterDescriptor::isParameterizedConverterType)
          .findFirst()
          .map(ParameterizedType.class::cast)
          .orElseGet(() -> {

            Type genericSuperclass = converter.getClass().getGenericSuperclass();

            return isParameterizedConverterType(genericSuperclass)
              ? (ParameterizedType) genericSuperclass
              : null;

          });

      Assert.notNull(parameterizedType, "Converter [%s] was not properly parameterized",
        converter.getClass().getName());

      Type[] actualTypeArguments = parameterizedType.getActualTypeArguments();

      Assert.isTrue(actualTypeArguments.length > 1,
        "Expected the parameterized type [%s] to have 2 type parameters",
        parameterizedType.getClass().getName());

      Class<?> fromType = ClassUtils.toRawType(actualTypeArguments[0]);
      Class<?> toType = ClassUtils.toRawType(actualTypeArguments[1]);

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
    @NullSafe
    private static boolean isParameterizedConverterType(@Nullable Type type) {
      return type instanceof ParameterizedType && ClassUtils.assignableTo(ClassUtils.toRawType(type), Converter.class);
    }

    private final Class<?> fromType;
    private final Class<?> toType;

    private final Converter<?, ?> converter;

    /**
     * Constructs a new {@link ConverterDescriptor} describing the {@link Class from type}
     * and {@link Class to type} conversion performed by {@link Converter}.
     *
     * @param converter {@link Converter} being described.
     * @param fromType {@link Class type} to convert from.
     * @param toType {@link Class type} to convert to.
     * @throws IllegalArgumentException if the {@link Converter}, {@link Class from type}
     * or {@link Class to type} are {@literal null}.
     * @see org.cp.elements.data.conversion.Converter
     * @see java.lang.Class
     */
    protected ConverterDescriptor(@NotNull Converter<?, ?> converter,
        @NotNull Class<?> fromType, @NotNull Class<?> toType) {

      this.converter = ObjectUtils.requireObject(converter, "Converter is required");
      this.fromType = ObjectUtils.requireObject(fromType, "From type is required");
      this.toType = ObjectUtils.requireObject(toType, "To type is required");
    }

    /**
     * Return a reference to the described {@link Converter}.
     *
     * @return a reference to the described {@link Converter}.
     * @see org.cp.elements.data.conversion.Converter
     */
    public @NotNull Converter<?, ?> getConverter() {
      return this.converter;
    }

    /**
     * {@link Class Type} to convert from.
     *
     * @return the {@link Class type} to convert from.
     * @see java.lang.Class
     */
    @SuppressWarnings("rawtypes")
    public @NotNull Class getFromType() {
      return this.fromType;
    }

    /**
     * {@link Class Type} to convert to.
     *
     * @return the {@link Class type} to convert to.
     * @see java.lang.Class
     */
    @SuppressWarnings("rawtypes")
    public @NotNull Class getToType() {
      return this.toType;
    }

    /**
     * Determines whether the {@link Converter} performs an exact {@link Class type} conversion.
     * <p>
     * The conversion is exact if the {@link Class qualified target type} matches the {@link Class to type}
     * of the {@link Converter}.
     *
     * @param targetType desired {@link Class target type} of the conversion.
     * @return a boolean indicating whether the {@link Converter} performs an exact {@link Class type} conversion.
     * @see java.lang.Class
     */
    @NullSafe
    @SuppressWarnings("rawtypes")
    public boolean isExactConversion(@Nullable Class targetType) {
      return getToType().equals(targetType);
    }

    /**
     * Alias for {@link Converter#canConvert(Class, Class)}.
     *
     * @param fromType {@link Class type} to convert from.
     * @param toType {@link Class type} to convert to.
     * @return a boolean value indicating whether the given {@link Class fromType}
     * can be converted to the {@link Class toType}.
     * @see org.cp.elements.data.conversion.Converter#canConvert(Class, Class)
     * @see #getConverter()
     */
    public boolean canConvert(@Nullable Class<?> fromType, @Nullable Class<?> toType) {
      return getConverter().canConvert(fromType, toType);
    }

    /**
     * Alias for {@link Converter#canConvert(Object, Class)}.
     *
     * @param value {@link Object} to convert.
     * @param toType {@link Class type} to convert the given {@link Object} to.
     * @return a boolean value indicating whether the given {@link Object}
     * can be converted into the given {@link Class type}.
     * @see org.cp.elements.data.conversion.Converter#canConvert(Object, Class)
     * @see #getConverter()
     */
    @NullSafe
    public boolean canConvert(@Nullable Object value, @Nullable Class<?> toType) {
      return getConverter().canConvert(value, toType);
    }

    @Override
    public int compareTo(@NotNull ConverterDescriptor that) {
      return SmartComparator.newSmartComparator().compare(this.getConverter(), that.getConverter());
    }

    @Override
    public boolean equals(Object obj) {

      if (this == obj) {
        return true;
      }

      if (!(obj instanceof ConverterDescriptor that)) {
        return false;
      }

      return ObjectUtils.equals(this.getFromType(), that.getFromType())
        && ObjectUtils.equals(this.getToType(), that.getToType());
    }

    @Override
    public int hashCode() {
      return ObjectUtils.hashCodeOf(getFromType(), getToType());
    }

    @Override
    public String toString() {
      return String.format("%1$s converts from [%2$s] to [%3$s]", getConverter().getClass().getName(),
        getFromType().getName(), getToType().getName());
    }
  }
}
