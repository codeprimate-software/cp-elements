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
 * The Interruptable interface defines a contract for objects that perform some complex or long running computation
 * that can be interrupted while processing.
 *
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Interruptable {

  /**
   * Determines whether the object who's class implements this interface has been interrupted through an invocation
   * of the interrupt method.
   *
   * @return a boolean value indicating whether this object was interrupted.
   */
  boolean isInterrupted();

  /**
   * Interrupts the object who's class implements this interface.  The interrupt could be issued from another Thread
   * while this object is performing a complex, long running and intensive computation.
   */
  void interrupt();

}
