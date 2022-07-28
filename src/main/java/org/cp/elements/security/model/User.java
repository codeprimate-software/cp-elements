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

import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Nameable;

/**
 * The {@link User} interface is an Abstract Data Type (ADT) defining and modeling a user
 * of an application or a system.
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

  @Override
  default int compareTo(User<ID> that) {
    return this.getName().compareTo(that.getName());
  }
}
