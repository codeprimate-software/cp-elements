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

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Order;

/**
 * Abstract utility class used to assess the order of an {@link Object}.
 *
 * @author John Blum
 * @see org.cp.elements.lang.Orderable
 * @see org.cp.elements.lang.Ordered
 * @see org.cp.elements.lang.annotation.Order
 * @since 1.0.0
 */
public abstract class OrderUtils {

  /**
   * Determines the given {@link Object Object's} {@link Integer order}.
   * <p>
   * Order is first determined by whether the {@link Object} is {@link Orderable} and if the {@literal order} returned
   * from the {@link Orderable#getOrder()} method is a {@link Number}. If not, then order is next determined by whether
   * the {@link Object} is {@link Ordered} and returning the {@literal order} from {@link Ordered#getIndex()}. If not,
   * then order is next determined by whether the {@link Object} is {@link Order} annotated and returning the
   * {@literal order} from the declared, annotated {@link Order#value()}.
   * <p>
   * If none of the above is {@literal true} or the {@link Object} reference is {@literal null},
   * then {@link Ordered#DEFAULT} is returned.
   *
   * @param target {@link Object} to evaluate.
   * @return a {@link Integer order} for the given {@link Object}.
   */
  public static int getOrder(@Nullable Object target) {

    return isNumericallyOrderable(target) ? getNumericallyOrderableObjectOrder(target)
      : isOrdered(target) ? getOrderedObjectOrder(target)
      : isOrderAnnotated(target) ? getOrderAnnotatedObjectOrder(target)
      : target != null ? Ordered.DEFAULT
      : Ordered.LAST;
  }

  @NullSafe
  private static boolean isNumericallyOrderable(@Nullable Object target) {
    return target instanceof Orderable && ((Orderable<?>) target).getOrder() instanceof Number;
  }

  private static int getNumericallyOrderableObjectOrder(@NotNull Object target) {
    return ((Number) ((Orderable<?>) target).getOrder()).intValue();
  }

  @NullSafe
  private static boolean isOrderAnnotated(@Nullable Object target) {
    return target != null && target.getClass().isAnnotationPresent(Order.class);
  }

  private static int getOrderAnnotatedObjectOrder(@NotNull Object target) {
    return target.getClass().getAnnotation(Order.class).value();
  }

  @NullSafe
  private static boolean isOrdered(@Nullable Object target) {
    return target instanceof Ordered;
  }

  private static int getOrderedObjectOrder(@NotNull Object target) {
    return ((Ordered) target).getIndex();
  }
}
