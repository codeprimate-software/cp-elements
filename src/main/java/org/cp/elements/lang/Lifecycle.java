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
 * The Lifecycle interface defines a contract for objects that live out a cycle of events from instantiation to
 * configuration and initialization, transitioning to a running state and finally reaching its end-of-life
 * and being consumed, or destroyed.
 *
 * @author John J. Blum
 * @param <T> the type of configuration meta-data and settings.
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Initable
 * @see java.lang.Runnable
 * @see org.cp.elements.lang.Interruptable
 * @see org.cp.elements.lang.Destroyable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Lifecycle<T> extends Configurable<T>, Initable, Runnable, Interruptable, Destroyable {

  /**
   * Determines whether this object is currently executing.
   *
   * @return a boolean value indicating whether this object is running.
   * @see java.lang.Runnable
   */
  boolean isRunning();

}
