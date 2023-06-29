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
package org.cp.elements.util;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.FluentApiExtension;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.Dsl;
import org.cp.elements.lang.annotation.FluentApi;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract extension class providing methods to write natural language expressions when performing various
 * {@link Collection} oriented operations.
 *
 * @author John J. Blum
 * @see java.util.Arrays
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.List
 * @see java.util.Set
 * @see org.cp.elements.lang.FluentApiExtension
 * @see org.cp.elements.lang.annotation.FluentApi
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CollectionExtensions {

  /**
   * The {@literal from} operator performs conversions on the given array and its elements.
   * <p>
   * For instance, the array can be converted into an ordered {@link List} of elements,
   * or a unique {@link Set} of elements originating from the array.
   *
   * @param <T> {@link Class} type of the elements in the array.
   * @param array array of elements all of {@link Class} type T to be converted.
   * @return a {@link From} operator to perform the conversions.
   * @see org.cp.elements.lang.annotation.FluentApi
   */
  @Dsl
  @SafeVarargs
  public static <T> From<T> from(T... array) {
    return new FromExpression<>(array);
  }

  /**
   * The {@link From} interface defines operations for conversion of array to a {@link List}, {@link Set}
   * or {@link String}.
   *
   * @param <T> {@link Class} type of elements in the {@link List} or {@link Set}.
   * @see org.cp.elements.lang.FluentApiExtension
   */
  @FluentApi
  public interface From<T> extends DslExtension, FluentApiExtension {

    /**
     * Converts an object array to a {@link List}.
     *
     * @return a {@link List} implementation containing all the elements in the given object array
     * to the {@link #from(Object[]) from} operator.
     * @see java.util.List
     */
    List<T> toList();

    /**
     * Converts an object array to a {@link Set}.
     *
     * @return a {@link Set} implementation containing all the elements of the given object array
     * to the {@link #from(Object[]) from} operator.
     * @see java.util.Set
     */
    Set<T> toSet();

  }

  /**
   * The {@link FromExpression} class is an implementation of the {@link From} interface,
   * {@link #from(Object[]) from} operator.
   *
   * @param <T> {@link Class type} of elements in the {@link List} or {@link Set}.
   * @see org.cp.elements.util.CollectionExtensions.From
   */
  private static final class FromExpression<T> implements From<T> {

    private final T[] array;

    @SafeVarargs
    public FromExpression(T... array) {
      this.array = array;
    }

    @Override
    public @NotNull List<T> toList() {

      return this.array == null ? Collections.emptyList()
        : this.array.length == 1 ? Collections.singletonList(this.array[0])
        : Arrays.asList(this.array);
    }

    @Override
    public @NotNull Set<T> toSet() {

      return this.array == null ? Collections.emptySet()
        : this.array.length == 1 ? Collections.singleton(this.array[0])
        : new HashSet<>(toList());
    }

    @Override
    public String toString() {

      StringBuilder buffer = new StringBuilder("[");

      T[] array = this.array;

      if (array != null) {
        for (int index = 0; index < array.length; index++) {
          buffer.append(index > 0 ? StringUtils.COMMA_SPACE_DELIMITER : StringUtils.EMPTY_STRING);
          buffer.append(array[index]);
        }
      }

      return buffer.append("]").toString();
    }
  }
}
