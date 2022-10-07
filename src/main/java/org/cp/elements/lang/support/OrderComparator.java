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

import java.util.Comparator;
import java.util.Optional;

import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Order;

/**
 * {@link Comparator} implementation that order {@link Object objects} whose {@link Class type} is annotated with
 * the {@link Order} annotation.
 *
 * @author John Blum
 * @see java.util.Comparator
 * @see org.cp.elements.lang.annotation.Order
 * @since 1.0.0
 */
public class OrderComparator implements Comparator<Object> {

  public static final OrderComparator INSTANCE = new OrderComparator();

  @Override
  public int compare(@NotNull Object one, @NotNull Object two) {
    return Integer.compare(getOrder(one), getOrder(two));
  }

  private int getOrder(@Nullable Object target) {

    return Optional.ofNullable(target)
      .map(Object::getClass)
      .filter(type -> type.isAnnotationPresent(Order.class))
      .map(type -> type.getAnnotation(Order.class))
      .map(Order::value)
      .orElse(Ordered.DEFAULT);
  }
}
