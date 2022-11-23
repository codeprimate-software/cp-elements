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

import org.cp.elements.lang.Composite;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Transformer;
import org.cp.elements.lang.annotation.NotNull;

/**
 * The ComposableTransformer class is a Transformer implementation combining two or more Transformer objects
 * in a composition delegate.
 *
 * @author John J. Blum
 * @param <T> {@link Class} type of the object to transform.
 * @see java.lang.Iterable
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableTransformer<T> implements Composite<Transformer<T>>, Transformer<T> {

  protected static final ComposableTransformer<?> INSTANCE = new ComposableTransformer<>();

  @SuppressWarnings("unchecked")
  public static <T> ComposableTransformer<T> builder() {
    return (ComposableTransformer<T>) INSTANCE;
  }

  private final Transformer<T> transformerOne;
  private final Transformer<T> transformerTwo;

  /**
   * Constructs a new, default instance of the {@link ComposableTransformer} class initialized with
   * {@link Transformer#identity() Identity Transformers}.
   *
   * @see org.cp.elements.lang.Transformer#identity()
   */
  private ComposableTransformer() {

    this.transformerOne = Transformer.identity();
    this.transformerTwo = Transformer.identity();
  }

  /**
   * Constructs a new instance of the {@link ComposableTransformer} class composed of the given array
   * of {@link Transformer Transformers} delegated to in the transformation operation.
   *
   * @param transformerOne first {@link Transformer} to apply in the transformation.
   * @param transformerTwo second {@link Transformer} to apply in the transformation.
   * @throws IllegalArgumentException if either {@link Transformer} argument is {@literal null}.
   * @see org.cp.elements.lang.Transformer
   */
  protected ComposableTransformer(@NotNull Transformer<T> transformerOne, @NotNull Transformer<T> transformerTwo) {

    this.transformerOne = ObjectUtils.requireObject(transformerOne, "Transformer one is required");
    this.transformerTwo = ObjectUtils.requireObject(transformerTwo, "Transformer two is required");
  }

  /**
   * Composes the given {@link Transformer Transformers} into a composite {@link Transformer}.
   *
   * @param one {@link Transformer} to compose.
   * @param two {@link Transformer} to compose.
   * @return a composite {@link Transformer} composed of the given {@link Transformer Transformers}.
   * Returns {@code one} if {@code two} is {@literal null}; returns {@code two} if {@code one} is {@literal null}.
   * @see #ComposableTransformer(Transformer, Transformer)
   */
  @Override
  public Transformer<T> compose(Transformer<T> one, Transformer<T> two) {
    return one == null ? two : (two == null ? one : new ComposableTransformer<>(one, two));
  }

  /**
   * Returns a reference to the first {@link Transformer} in this composition.
   *
   * @return a reference to the first {@link Transformer}.
   * @see org.cp.elements.lang.Transformer
   */
  protected Transformer<T> getTransformerOne() {
    return this.transformerOne;
  }

  /**
   * Returns a reference to the second {@link Transformer} in this composition.
   *
   * @return a reference to the second {@link Transformer}.
   * @see org.cp.elements.lang.Transformer
   */
  protected Transformer<T> getTransformerTwo() {
    return this.transformerTwo;
  }

  /**
   * Transforms the given value of {@link Class} type T with the {@link Transformer Transformers}.
   *
   * @param value {@link Object} value to transform.
   * @return the transformed value.
   */
  public T transform(T value) {
    return getTransformerTwo().apply(getTransformerOne().apply(value));
  }
}
