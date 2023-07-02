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
 * Interface defining a contract for {@link Object Objects} that perform some complex or long-running computation
 * that can be interrupted while processing.
 *
 * @author John J. Blum
 * @see java.lang.Runnable
 * @see java.util.concurrent.Callable
 * @see org.cp.elements.lang.Executable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Interruptable {

  /**
   * Determines whether {@literal this} {@link Object} has been interrupted during execution through an invocation
   * of the {@link #interrupt} method.
   *
   * @return a boolean value indicating whether {@literal this} {@link Object} was interrupted during execution.
   */
  boolean isInterrupted();

  /**
   * Interrupts {@literal this} {@link Object} during execution.
   * <p>
   * The interrupt could be issued by another {@link Thread} while {@literal this} {@link Object} is performing
   * a complex, long-running and intensive computation.
   * <p>
   * This method has no effect if {@literal this} {@link Object} is not blocked in execution. This method
   * is also idempotent.
   */
  void interrupt();

}
