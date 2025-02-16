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
package org.cp.elements.data.oql;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract Data Type (ADT) modeling a OQL {@link Query} argument.
 *
 * @author John Blum
 * @see org.cp.elements.lang.Nameable
 * @since 2.0.0
 */
public record QueryArgument<T>(@NotNull String name, @Nullable T value) implements Nameable<String> {

  static <T> QueryArgument<T> from(@NotNull String name, @Nullable T value) {
    Assert.hasText(name, "Name [%s] is required", name);
    return new QueryArgument<>(name, value);
  }

  public QueryArgument {
    Assert.hasText(name, "Name [%s] is required", name);
  }

  @Override
  public String getName() {
    return name();
  }
}
