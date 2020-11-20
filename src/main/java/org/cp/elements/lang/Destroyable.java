/*
 * Copyright 2016 Author or Authors.
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
 * The {@link Destroyable} interface defines a contract for objects that can be destroyed, releasing any resources
 * held prior to destruction followed by garbage collection by the JVM.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Destroyable {

  /**
   * Determines whether this object has been destroyed.  The object is considered destroyed when it's destroy method
   * has been invoked.
   *
   * @return a boolean value indicating whether this object has been destroyed or not.
   */
  boolean isDestroyed();

  /**
   * Destroys this {@link Object} releasing any and all resources held by this {@link Destroyable object}.
   *
   * @see #destroy(Runnable)
   */
  default void destroy() {
    destroy(RunnableUtils.NOOP_RUNNABLE);
  }

  /**
   * Destroys this {@link Object} releasing any and all resources held by this {@link Destroyable object}.
   *
   * The implementation must invoke the given {@link Runnable} at the end of the destruction operation.
   *
   * @param runnable {@link Runnable} callback to invoke at the end of the destruction operation.
   * @see java.lang.Runnable
   */
  void destroy(Runnable runnable);

}
