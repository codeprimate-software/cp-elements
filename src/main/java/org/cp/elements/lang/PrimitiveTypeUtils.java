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
package org.cp.elements.lang;

import java.util.Set;
import java.util.function.Function;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract utility class for processing Java primitive types.
 *
 * @author John Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class PrimitiveTypeUtils {

  private static final Set<PrimitiveToWrapperTypeAssociation> PRIMITIVE_WRAPPER_TYPES = CollectionUtils.asSet(
    PrimitiveToWrapperTypeAssociation.Builder.from(boolean.class).to(Boolean.class),
    PrimitiveToWrapperTypeAssociation.Builder.from(byte.class).to(Byte.class),
    PrimitiveToWrapperTypeAssociation.Builder.from(short.class).to(Short.class),
    PrimitiveToWrapperTypeAssociation.Builder.from(int.class).to(Integer.class),
    PrimitiveToWrapperTypeAssociation.Builder.from(long.class).to(Long.class),
    PrimitiveToWrapperTypeAssociation.Builder.from(float.class).to(Float.class),
    PrimitiveToWrapperTypeAssociation.Builder.from(double.class).to(Double.class),
    PrimitiveToWrapperTypeAssociation.Builder.from(char.class).to(Character.class)
  );

  @SuppressWarnings({ "rawtypes", "unchecked" })
  private static final Function<Class, Class> PRIMITIVE_TO_WRAPPER_TYPE = primitiveType ->
    PRIMITIVE_WRAPPER_TYPES.stream()
      .filter(it -> it.getPrimitiveType().equals(primitiveType))
      .findAny()
      .map(PrimitiveToWrapperTypeAssociation::getWrapperType)
      .orElseGet(() -> ObjectUtils.returnFirstNonNullValue(primitiveType, Object.class));

  @SuppressWarnings({ "rawtypes", "unchecked" })
  private static final Function<Class, Class> WRAPPER_TO_PRIMITIVE_TYPE = wrapperType ->
    PRIMITIVE_WRAPPER_TYPES.stream()
      .filter(it -> it.getWrapperType().equals(wrapperType))
      .findAny()
      .map(PrimitiveToWrapperTypeAssociation::getPrimitiveType)
      .orElseGet(() -> ObjectUtils.returnFirstNonNullValue(wrapperType, Object.class));

  /**
   * Gets a {@link Function} that converts {@link Class primitive types} to {@link Class wrapper types}.
   *
   * For example, if the returned {@link Function} is given {@link Integer#TYPE}, then the {@link Function}
   * will return {@link Integer} {@link Class}.
   *
   * @return a {@link Class primitive type} to {@link Class wrapper type} {@link Function}.
   * @see java.util.function.Function
   * @see #wrapperToPrimitiveType()
   */
  @NullSafe
  @SuppressWarnings("rawtypes")
  public static @NotNull Function<Class, Class> primitiveToWrapperType() {
    return PRIMITIVE_TO_WRAPPER_TYPE;
  }

  /**
   * Gets a {@link Function} that converts {@link Class wrapper types} to {@link Class primitive types}.
   *
   * For example, if the returned {@link Function} is given the {@link Integer} {@link Class},
   * then the {@link Function} will return {@link Integer#TYPE}.
   *
   * @return a {@link Class wrapper type} to {@link Class primitive type} {@link Function}.
   * @see java.util.function.Function
   * @see #primitiveToWrapperType()
   */
  @NullSafe
  @SuppressWarnings("rawtypes")
  public static @NotNull Function<Class, Class> wrapperToPrimitiveType() {
    return WRAPPER_TO_PRIMITIVE_TYPE;
  }

  /**
   * Mapping of Java primitive types (for example: {@link Integer#TYPE int})
   * to Java's wrapper types (for example: {@link Integer}).
   */
  public static class PrimitiveToWrapperTypeAssociation {

    private final Class<?> primitiveType;
    private final Class<?> wrapperType;

    /**
     * Constructs a new {@link PrimitiveToWrapperTypeAssociation} initialized with the given,
     * required primitive type and associated wrapper type.
     *
     * @param primitiveType Java primitive type; must not be {@literal null}.
     * @param wrapperType Java wrappter type; must not be {@literal null}.
     */
    protected PrimitiveToWrapperTypeAssociation(@NotNull Class<?> primitiveType, @NotNull Class<?> wrapperType) {

      this.primitiveType = ObjectUtils.requireObject(primitiveType, "Primitive type is required");
      this.wrapperType = ObjectUtils.requireObject(wrapperType, "Wrapper type is required");
    }

    /**
     * Gets the Java primitive type in this association.
     *
     * @return the Java primitive type in this association.
     * @see #getWrapperType()
     */
    public @NotNull Class<?> getPrimitiveType() {
      return this.primitiveType;
    }

    /**
     * Gets the Java wrapper type in this association.
     *
     * @return the Java wrapper type in this association.
     * @see #getPrimitiveType()
     */
    public @NotNull Class<?> getWrapperType() {
      return this.wrapperType;
    }

    /**
     * Builder used to construct and initialize a new instance of {@link PrimitiveToWrapperTypeAssociation}.
     */
    public static class Builder {

      /**
       * Factory method used to construct a new instance of this {@link Builder} used to construct and initialize
       * a new {@link PrimitiveToWrapperTypeAssociation}.
       *
       * @param primitiveType Java primitive type.
       * @return a new {@link Builder}.
       */
      public static Builder from(@NotNull Class<?> primitiveType) {
        return new Builder(primitiveType);
      }

      private final Class<?> primitiveType;

      /**
       * Constructs a new this {@link Builder} used to construct and initialize
       * a new {@link PrimitiveToWrapperTypeAssociation}.
       *
       * @param primitiveType Java primitive type.
       */
      protected Builder(@NotNull Class<?> primitiveType) {
        this.primitiveType = primitiveType;
      }

      /**
       * Builder method used to capture the Java wrapper type to associate with the configured Java primitive type.
       *
       * @param wrapperType Java wrapper type.
       * @return this {@link Builder}.
       */
      public @NotNull PrimitiveToWrapperTypeAssociation to(@NotNull Class<?> wrapperType) {
        return new PrimitiveToWrapperTypeAssociation(this.primitiveType, wrapperType);
      }
    }
  }
}
