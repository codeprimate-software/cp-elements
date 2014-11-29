/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang.support;

import java.util.Iterator;

import org.cp.elements.lang.Transformer;
import org.cp.elements.util.ArrayUtils;

/**
 * The ComposableTransformer class is a Transformer implementation combining two or more Transformer objects
 * in a composition delegate.
 *
 * @author John J. Blum
 * @param <T> the Class type of the object to transform.
 * @see java.lang.Iterable
 * @see org.cp.elements.lang.Transformer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ComposableTransformer<T> implements Transformer<T>, Iterable<Transformer<T>> {

  private final Iterable<Transformer<T>> transformers;

  /**
   * Constructs an instance of the ComposableTransformer class composed of the specified Transformers delegated to
   * in the transformation operation.
   *
   * @param transformers the array of Transformers used as delegates in this Transformer composition.
   */
  private ComposableTransformer(final Transformer<T>... transformers) {
    this.transformers = ArrayUtils.iterable(transformers.clone());
  }

  /**
   * Composes the array of Transformers into a Transformer composition.
   *
   * @param <T> the Class type of the values transformed by the Transformers.
   * @param transformers the array of Transformers to combine into a composition.
   * @return a Transformer composition composed of the specified Transformers.  Returns null if the array reference
   * is null, or a single Transformer if the array is of length 1, otherwise a ComposableTransformer composed
   * of the Transformers in the array.
   */
  public static <T> Transformer<T> compose(final Transformer<T>... transformers) {
    return (ArrayUtils.isEmpty(transformers) ? null : (transformers.length == 1 ? transformers[0]
      : new ComposableTransformer<T>(transformers)));
  }

  /**
   * Iterates over the Transformers in this composition.
   *
   * @return an Iterator over the Transformers in this composition.
   * @see java.lang.Iterable#iterator()
   */
  @Override
  public Iterator<Transformer<T>> iterator() {
    return transformers.iterator();
  }

  /**
   * Transforms the given value of Class type T with the delegating Transformers.
   *
   * @param value the value to transform.
   * @return the transformed value.
   */
  @Override
  public T transform(T value) {
    for (Transformer<T> transformer : transformers) {
      value = transformer.transform(value);
    }

    return value;
  }

}
