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

package org.cp.elements.process;

import org.cp.elements.lang.Assert;

/**
 * The {@link ProcessAdapter} class is a Adapter (wrapper) around a Java {@link Process} object.
 *
 * @author John J. Blum
 * @see java.lang.Process
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ProcessAdapter {

  private final Process process;

  /**
   * Factory method to construct an instance of {@link ProcessAdapter} initialized with
   * the given {@link Process} object.
   *
   * @param process {@link Process} object to adapt/wrap with an instance of the {@link ProcessAdapter} class.
   * @return a newly constructed {@link ProcessAdapter} adapting/wrapping the given {@link Process} object.
   * @throws IllegalArgumentException if {@link Process} is {@literal null}.
   * @see #ProcessAdapter(Process)
   * @see java.lang.Process
   */
  public static ProcessAdapter newProcessAdapter(Process process) {
    return new ProcessAdapter(process);
  }

  /**
   * Constructs an instance of {@link ProcessAdapter} initialized with the given {@link Process} object.
   *
   * @param process {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   * @throws IllegalArgumentException if {@link Process} is {@literal null}.
   * @see java.lang.Process
   */
  public ProcessAdapter(Process process) {
    Assert.notNull(process, "Process cannot be null");
    this.process = process;
  }

  /**
   * Returns a reference to the {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   *
   * @return a reference to the {@link Process} object adapted/wrapped by this {@link ProcessAdapter}.
   * @see java.lang.Process
   */
  public Process getProcess() {
    return this.process;
  }
}
