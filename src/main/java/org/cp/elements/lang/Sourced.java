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

/**
 * Java {@link FunctionalInterface} defining a contract for {@link Object Objects} that have a {@literal source}.
 * <p>
 * For instance, {@link java.util.Properties} might be sourced from a {@link java.io.File}.
 * A {@link java.sql.Connection} is created from a {@link javax.sql.DataSource}.
 * And, a {@link java.util.Map.Entry} is sourced from the {@link java.util.Map} from which it originated.
 *
 * @author John Blum
 * @param <T> {@link Class type} of the {@literal source}.
 * @see java.lang.FunctionalInterface
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Sourced<T> {

  /**
   * Gets the {@literal source} from which this {@link Object} originated.
   *
   * @return the {@literal source} of this {@link Object}.
   */
  T getSource();

}
