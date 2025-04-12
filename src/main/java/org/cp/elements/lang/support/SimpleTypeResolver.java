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
package org.cp.elements.lang.support;

import static org.cp.elements.lang.ElementsExceptionsFactory.newTypeNotFoundException;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.stream.Stream;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.TypeResolver;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.stream.StreamUtils;
import org.cp.elements.util.stream.Streamable;

/**
 * Simple, default implementation of {@link TypeResolver}.
 *
 * @author John Blum
 * @see java.lang.Class
 * @see java.lang.reflect.ParameterizedType
 * @see java.lang.reflect.Type
 * @see org.cp.elements.lang.TypeResolver
 * @since 2.0.0
 */
@SuppressWarnings("unused")
public class SimpleTypeResolver implements TypeResolver {

  public static final SimpleTypeResolver INSTANCE = new SimpleTypeResolver();

  private static final Type[] EMPTY_TYPES = new Type[0];

  @Override
  public Class<?> resolveType(Object target) {

    if (target != null) {

      Class<?> resolvedType = isArrayType(target) ? resolveArrayType(target.getClass())
        : isIterableType(target) ? resolveTypeArgument(target.getClass())
        : isStreamType(target) ? resolveTypeArgument(target.getClass())
        : null;

      return resolvedType != null ? resolvedType
        : finalResolveType(target);
    }

    // Most likely, target is null.
    throw newTypeNotFoundException("Cannot resolve type for null Object");
  }

  /**
   * Perform the final attempt to resolve the {@link Class type} of the given {@link Object}.
   *
   * @param target {@link Object} from which to resolve its {@link Class type}.
   * @return the resolved {@link Class type} of the given {@link Object}.
   */
  Class<?> finalResolveType(Object target) {

    Stream<?> resolvedStream = resolveStream(target);

    return resolvedStream != null
      ? resolveElementType(resolvedStream)
      : resolveObjectType(target);
  }

  private static Stream<?> resolveStream(Object target) {

    return target instanceof Stream<?> stream ? stream
      : target instanceof Streamable<?> streamable ? streamable.stream()
      : target instanceof Iterable<?> iterable ? StreamUtils.stream(iterable)
      : null;
  }

  private boolean isArrayType(Object target) {
    return ObjectUtils.isArray(resolveObjectType(target));
  }

  private boolean isIterableType(Object target) {
    return target instanceof Iterable<?>;
  }

  private boolean isStreamType(Object target) {
    return target instanceof Stream<?> || target instanceof Streamable<?>;
  }

  private boolean isStreamType(Class<?> type) {
    return Stream.class.equals(type);
  }

  private boolean isNotStreamType(Class<?> type) {
    return !isStreamType(type);
  }

  private boolean isNotStreamType(Type type) {
    return type instanceof Class<?> classType && isNotStreamType(classType);
  }

  private Class<?> resolveArrayType(Class<?> arrayType) {

    Class<?> componentType = arrayType.getComponentType();

    while (componentType.isArray()) {
      componentType = componentType.getComponentType();
    }

    return componentType;
  }

  private Class<?> resolveElementType(Stream<?> stream) {

    return StreamUtils.nullSafeStream(stream)
      .findFirst()
      .map(this::resolveObjectType)
      .orElseThrow(() -> newTypeNotFoundException("Unable to resolve element type from [%s]", stream));
  }

  private Class<?> resolveObjectType(Object target) {
    return ObjectUtils.getClass(target);
  }

  private ParameterizedType resolveParameterizedType(Type type) {

    if (type instanceof ParameterizedType parameterizedType) {
      return parameterizedType;
    }

    if (type instanceof Class<?> classType) {

      Type[] genericInterfaces = classType.getGenericInterfaces();

      for (Type genericInterface : ArrayUtils.nullSafeArray(genericInterfaces)) {
        ParameterizedType parameterizedType = resolveParameterizedType(genericInterface);
        if (parameterizedType != null) {
          return parameterizedType;
        }
      }

      Type genericSuperclass = classType.getGenericSuperclass();

      if (genericSuperclass != null) {
        return resolveParameterizedType(genericSuperclass);
      }
    }

    return null;
  }

  private static Type resolveRawType(Type type) {

    return type instanceof ParameterizedType parameterizedType
      ? parameterizedType.getRawType()
      : type;
  }

  private Class<?> resolveTypeArgument(Class<?> type) {

    ParameterizedType parameterizedType = resolveParameterizedType(type);

    if (parameterizedType != null) {

      Type[] typeArguments = ArrayUtils.nullSafeArray(parameterizedType.getActualTypeArguments());

      Assert.isTrue(isNotStreamType(type) || typeArguments.length == 1,
        "Expected only 1 type argument in type [%s], but was [%s]", type, typeArguments.length);

      Type typeArgument = typeArguments[0];

      typeArgument = resolveRawType(typeArgument);
      typeArgument = resolveTypeVariable(typeArgument);

      return (Class<?>) typeArgument;
    }

    return null;
  }

  private Type resolveTypeVariable(Type type) {

    if (type instanceof TypeVariable<?> typeVariable) {

      Type[] bounds = ArrayUtils.nullSafeArray(typeVariable.getBounds());

      Assert.isTrue(isNotStreamType(type) || bounds.length == 1,
        "Expected only 1 type variable in type [%s], but was [%d]", type, bounds.length);

      Type boundedType = bounds[0];

      if (Object.class.equals(boundedType)) {
        return null;
      }
    }

    return type;
  }
}
