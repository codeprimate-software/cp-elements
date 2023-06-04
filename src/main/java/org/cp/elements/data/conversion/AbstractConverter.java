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

import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalStateException;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Optional;
import java.util.function.Function;

import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.ArrayUtils;

/**
 * An abstract base class encapsulating functionality and behavior common to all {@link Converter} implementations.
 *
 * @author John J. Blum
 * @param <S> {@link Class source type} to convert from.
 * @param <T> {@link Class target type} to convert to.
 * @see java.util.function.Function
 * @see org.cp.elements.data.conversion.ConversionServiceAware
 * @see org.cp.elements.data.conversion.Converter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractConverter<S, T> implements Converter<S, T> {

  private ConversionService conversionService;

  private Class<?> sourceType;
  private Class<?> targetType;

  /**
   * Constructs a new {@link AbstractConverter}.
   *
   * @see #init(Class)
   */
  public AbstractConverter() {
    init(getClass());
  }

  /**
   * Constructs a new {@link AbstractConverter} initialized with the given {@link Class type}
   * to determine the {@link Class source} and {@link Class target} types of the conversion.
   *
   * @param type {@link Class} to evaluate for source and target {@link Class types}.
   * @see java.lang.Class
   * @see #init(Class)
   */
  public AbstractConverter(@NotNull Class<?> type) {
    init(type);
  }

  /**
   * Initializes the {@link Class source} and {@link Class target} types of the data conversion
   * performed by this {@link Converter}.
   *
   * @param type {@link Class} to evaluate for source and target {@link Class types}; must not be {@literal null}.
   * @see java.lang.Class
   */
  private void init(@NotNull Class<?> type) {

    ParameterizedType parameterizedType =
      Arrays.stream(ArrayUtils.nullSafeArray(type.getGenericInterfaces(), Type.class))
        .filter(this::isParameterizedFunctionType)
        .findFirst()
        .map(ParameterizedType.class::cast)
        .orElseGet(() -> {

          Type genericSuperclass = type.getGenericSuperclass();

          return isParameterizedFunctionType(genericSuperclass) ? (ParameterizedType) genericSuperclass : null;

        });

    this.sourceType = parameterizedType != null
      ? ClassUtils.toRawType(parameterizedType.getActualTypeArguments()[0])
      : Object.class;

    this.targetType = parameterizedType != null
      ? ClassUtils.toRawType(parameterizedType.getActualTypeArguments()[1])
      : Object.class;
  }

  /**
   * Sets a reference to the {@link ConversionService} used to perform data conversions.
   *
   * @param conversionService reference to the {@link ConversionService} used to perform data conversions.
   * @see org.cp.elements.data.conversion.ConversionService
   */
  public void setConversionService(@Nullable ConversionService conversionService) {
    this.conversionService = conversionService;
  }

  /**
   * Returns an {@link Optional} reference to the {@link ConversionService} used to perform data conversions.
   *
   * @return an {@link Optional} reference to the {@link ConversionService} used to perform data conversions.
   * @see org.cp.elements.data.conversion.ConversionService
   * @see java.util.Optional
   */
  protected Optional<ConversionService> getConversionService() {
    return Optional.ofNullable(this.conversionService);
  }

  /**
   * Resolves the reference to the {@link ConversionService} used to perform data conversions.
   *
   * @return the resolved reference to the {@link ConversionService} used to perform data conversions.
   * @throws IllegalStateException if no {@link ConversionService} was configured.
   * @see org.cp.elements.data.conversion.ConversionService
   * @see #getConversionService()
   */
  protected @NotNull ConversionService resolveConversionService() {
    return getConversionService()
      .orElseThrow(() -> newIllegalStateException("No ConversionService was configured"));
  }

  /**
   * Determines whether the {@link Class from type} is {@link ClassUtils#assignableTo(Class, Class) assignable to}
   * any of the {@link Class to types}.
   *
   * @param fromType {@link Class type} to evaluate.
   * @param toTypes {@link Class types} checked for assignment compatibility.
   * @return {@literal true} if {@link Class from type} is assignable to any of the {@link Class to types}.
   * @see org.cp.elements.lang.ClassUtils#assignableTo(Class, Class)
   * @see java.lang.Class
   */
  @NullSafe
  protected boolean isAssignableTo(Class<?> fromType, Class<?>... toTypes) {

    for (Class<?> toType : ArrayUtils.nullSafeArray(toTypes, Class.class)) {
      if (ClassUtils.assignableTo(fromType, toType)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Determines whether the {@link Type} is a generic, parameterized {@link Converter} {@link Class type},
   * such as by implementing the {@link Converter} interface or extending the {@link AbstractConverter} base class.
   *
   * @param type {@link Type} to evaluate.
   * @return a boolean if the {@link Type} represents a generic, parameterized {@link Converter} {@link Class type}.
   * @see java.lang.reflect.Type
   */
  @NullSafe
  protected boolean isParameterizedFunctionType(@Nullable Type type) {

    return Optional.ofNullable(type)
      .filter(ParameterizedType.class::isInstance)
      .map(it -> {

        Class<?> rawType = ObjectUtils.safeGetValue(() -> ClassUtils.toRawType(it), Object.class);

        return isAssignableTo(rawType, Converter.class, Function.class);

      })
      .orElse(false);
  }

  /**
   * Returns an {@link Optional} {@link Class source type} of the data conversion performed by this {@link Converter}.
   *
   * @return an {@link Optional} {@link Class source type} of the data conversion performed by this {@link Converter}.
   * @see java.util.Optional
   * @see java.lang.Class
   */
  protected Optional<Class<?>> getSourceType() {
    return Optional.ofNullable(this.sourceType);
  }

  /**
   * Returns an {@link Optional} {@link Class target type} of the data conversion performed by this {@link Converter}.
   *
   * @return an {@link Optional} {@link Class target type} of the data conversion performed by this {@link Converter}.
   * @see java.util.Optional
   * @see java.lang.Class
   */
  protected Optional<Class<?>> getTargetType() {
    return Optional.ofNullable(this.targetType);
  }

  /**
   * Determines whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   *
   * @param fromType {@link Class type} to convert from.
   * @param toType {@link Class type} to convert to.
   * @return a boolean indicating whether this {@link Converter} can convert {@link Object Objects}
   * {@link Class from type} {@link Class to type}.
   * @see org.cp.elements.data.conversion.ConversionService#canConvert(Class, Class)
   * @see #getSourceType()
   * @see #getTargetType()
   */
  @Override
  public boolean canConvert(Class<?> fromType, Class<?> toType) {

    boolean canConvert = getSourceType()
      .filter(this::notObjectTypePredicate)
      .map(sourceType -> ClassUtils.assignableTo(fromType, sourceType))
      .orElse(true);

    // targetType should be assignable to or a subclass of toType
    // toType should be assignable from targetType
    canConvert &= getTargetType()
      .filter(this::notObjectTypePredicate)
      .map(targetType -> ClassUtils.assignableTo(targetType, toType))
      .orElse(true);

    return canConvert;
  }

  private boolean notObjectTypePredicate(@Nullable Class<?> type) {
    return !Object.class.equals(type);
  }

  /**
   * Converts an {@link Object} of {@link Class type S} into an {@link Object} of {@link Class type T}.
   *
   * @param value {@link Object} to convert.
   * @return the converted {@link Object} of {@link Class type T}.
   * @throws ConversionException if the {@link Object} cannot be converted.
   * @see org.cp.elements.data.conversion.ConversionService#convert(Object, Class)
   * @see #convert(Object, Class)
   */
  @Override
  public T convert(S value) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }
}
