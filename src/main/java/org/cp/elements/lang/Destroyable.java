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

/**
 * Interface defining a contract for {@link Object Objects} that can be destroyed, releasing any and all resources
 * held prior to destruction followed by garbage collection initiated by the JVM.
 *
 * @author John J. Blum
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.Initable
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface Destroyable {

  /**
   * Determines whether {@literal this} {@link Object} has been destroyed.
   * <p>
   * The {@link Object} is considered destroyed when its {@link #destroy} method has been invoked
   * and returns successfully. Defaults to {@literal false}.
   *
   * @return a boolean value indicating whether {@literal this} {@link Object} has been destroyed.
   * @see #destroy()
   */
  default boolean isDestroyed() {
    return false;
  }

  /**
   * Destroys {@literal this} {@link Object} by releasing any and all resources held by this {@link Destroyable Object}
   * during execution.
   *
   * @see #destroy(Runnable)
   */
  default void destroy() {
    destroy(RunnableUtils.NOOP_RUNNABLE);
  }

  /**
   * Destroys {@literal this} {@link Object} by releasing any and all resources held by this {@link Destroyable Object}
   * during execution.
   * <p>
   * The implementation must invoke the given {@link Runnable} at the end of the destruction operation.
   *
   * @param runnable {@link Runnable} callback to invoke at the end of the destruction operation.
   * @see java.lang.Runnable
   */
  void destroy(@NotNull Runnable runnable);

}
