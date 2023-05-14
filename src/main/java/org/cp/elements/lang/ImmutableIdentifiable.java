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
 * Java {@link FunctionalInterface} defining a contract for {@link Object Objects} that can be identified.
 *
 * @author John Blum
 * @param <ID> {@link Class type} of the {@link Comparable} {@link Object identifier}.
 * @see java.lang.Comparable
 * @since 1.0.2
 */
@FunctionalInterface
public interface ImmutableIdentifiable<ID extends Comparable<ID>> {

  /**
   * Returns the {@link ID identifier} uniquely identifying {@literal this} {@link Object}.
   *
   * @return the {@link ID identifier} uniquely identifying {@literal this} {@link Object}.
   */
  ID getId();

}
