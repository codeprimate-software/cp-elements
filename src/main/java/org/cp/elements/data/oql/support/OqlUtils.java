/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql.support;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.cp.elements.data.oql.Oql;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Builder;
import org.cp.elements.lang.ClassUtils;
import org.cp.elements.lang.TypeResolver;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.util.ArrayUtils;
import org.cp.elements.util.CollectionUtils;

/**
 * Abstract utility class for processing {@link Oql}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.Oql
 * @since 2.0.0
 */
public abstract class OqlUtils {

  public static <T> ArrayBuilder<T> asArray(Iterable<T> iterable) {
    return ArrayBuilder.from(iterable);
  }

  public static class ArrayBuilder<T> implements Builder<T[]> {

    public static <T> ArrayBuilder<T> empty() {
      return from(new ArrayList<>());
    }

    @SafeVarargs
    public static <T> ArrayBuilder<T> from(T... elements) {
      return from(Arrays.asList(ArrayUtils.nullSafeArray(elements)));
    }

    public static <T> ArrayBuilder<T> from(Iterable<T> elements) {
      Assert.notNull(elements, "An Iterable collection of elements is required");
      return new ArrayBuilder<>(new ArrayList<>(CollectionUtils.asList(elements)));
    }

    private final List<T> list;

    public ArrayBuilder(List<T> list) {
      this.list = list;
    }

    public boolean isEmpty() {
      return this.list.isEmpty();
    }

    public ArrayBuilder<T> add(@NotNull T element) {
      Assert.notNull(element, "Element is required");
      this.list.add(element);
      return this;
    }

    public T remove() {
      int index = toIndex(size());
      Assert.state(index > -1, "Array is empty");
      return this.list.remove(index);
    }

    public int size() {
      return this.list.size();
    }

    private int toIndex(int number) {
      return number - 1;
    }

    @SuppressWarnings("unchecked")
    private Class<T> resolveElementType() {

      Class<T> elementType = (Class<T>) TypeResolver.getInstance().resolveType(this.list);
      String elementTypeName = elementType.getName();
      int index = elementTypeName.indexOf("$$Lambda");

      return index > -1
        ? ClassUtils.loadClass(elementTypeName.substring(0, index))
        : elementType;
    }

    @SuppressWarnings("unchecked")
    private T[] newArray(int length) {
      Class<T> componentType = resolveElementType();
      return (T[]) Array.newInstance(componentType, length);
    }

    @Override
    public T[] build() {
      return this.list.toArray(newArray(size()));
    }
  }
}
