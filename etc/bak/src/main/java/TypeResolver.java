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

/**
 * {@link TypeResolver} is a strategy interface that determines the {@link Class type} of an {@link Object}.
 */
public interface TypeResolver {
  Class resolveType(Object obj);
}

/**
 * {@link SimpleTypeResolver} is a {@link TypeResolver} implementation that determines the {@link Class type}
 * of an {@link Object} using the null-safe implementation of {@link Object#getClass()}.
 *
 * @see SimpleBloomFilter.TypeResolver
 * @see org.cp.elements.lang.ClassUtils#getClass(Object)
 */
public static class SimpleTypeResolver implements TypeResolver {

  /* (non-Javadoc) */
  @Override
  public Class resolveType(Object obj) {
    return ClassUtils.getClass(obj);
  }
}

/**
 * {@link SmartTypeResolver} is an extension of {@link SimpleTypeResolver} and an implementation
 * of the {@link TypeResolver} interface that compresses the {@link Class type} of a {@link Number}
 * based on its value.
 *
 * @see SimpleBloomFilter.SimpleTypeResolver
 */
public static class SmartTypeResolver extends SimpleTypeResolver {

  /* (non-Javadoc) */
  @Override
  public Class resolveType(Object obj) {

    if (obj instanceof Number) {

      Number value = (Number) obj;

      if (isDecimal(value)) {
        return (isFloat(value) ? Float.class : Double.class);
      }
      else {
        return (isByte(value) ? Byte.class
          : (isShort(value) ? Short.class
          : (isInteger(value) ? Integer.class : Long.class)));
      }
    }

    return super.resolveType(obj);
  }
}
