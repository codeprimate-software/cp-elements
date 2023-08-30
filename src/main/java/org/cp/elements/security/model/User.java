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
package org.cp.elements.security.model;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.annotation.NotNull;

/**
 * Abstract Data Type (ADT) defining and modeling a user of an application, program or system.
 *
 * @author John Blum
 * @param <ID> {@link Class type} of the identifier.
 * @see java.lang.Comparable
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.Nameable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface User<ID extends Comparable<ID>> extends Comparable<User<ID>>, Identifiable<ID>, Nameable<String> {

  /**
   * Factory method used to construct a new {@link User} with the given, required {@link String name}.
   *
   * @param <ID> {@link Class Type} of the {@link User} {@literal ID}.
   * @param name {@link String} containing the {@literal username}; must not be {@literal null} or {@literal empty}.
   * @return a new {@link User} with the given {@link String name}.
   * @throws IllegalArgumentException if the given {@link String name} is {@literal null} or {@literal empty}.
   */
  static <ID extends Comparable<ID>> User<ID> named(@NotNull String name) {

    Assert.hasText(name, "Username [%s] is required", name);

    return new User<>() {

      @Override
      public ID getId() {
        return null;
      }

      @Override
      public String getName() {
        return name;
      }
    };
  }

  @Override
  default int compareTo(User<ID> that) {
    return this.getName().compareTo(that.getName());
  }

  @Override
  @SuppressWarnings("unchecked")
  default <IDX extends Identifiable<ID>> IDX identifiedBy(ID id) {

    return (IDX) new User<ID>() {

      @Override
      public ID getId() {
        return id;
      }

      @Override
      public String getName() {
        return User.this.getName();
      }
    };
  }
}
