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
package org.cp.elements.function;

import java.util.Objects;
import java.util.function.Predicate;

/**
 * An Enumeration (enum) of canned (provided) {@link Predicate Predicates}.
 *
 * @author John Blum
 * @see java.util.function.Predicate
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum CannedPredicates implements Predicate<Object> {

  ACCEPT_ALL {

    @Override
    public boolean test(Object o) {
      return true;
    }
  },

  IS_FALSE {

    @Override
    public boolean test(Object value) {
      return Boolean.FALSE.equals(value);
    }
  },

  IS_TRUE {

    @Override
    public boolean test(Object value) {
      return Boolean.TRUE.equals(value);
    }
  },

  NOT_NULL {

    @Override
    public boolean test(Object value) {
      return Objects.nonNull(value);
    }
  },

  REJECT_ALL {

    @Override
    public boolean test(Object value) {
      return false;
    }
  };

  public static final CannedPredicates DEFAULT = ACCEPT_ALL;

}
