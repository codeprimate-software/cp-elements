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

package org.cp.elements.process.event;

import java.util.EventListener;

/**
 * The {@link ProcessInputStreamListener} class is an {@link EventListener} used to listen for input events
 * originating from a {@link Process Process's} standard out or standard error streams.
 *
 * @author John Blum
 * @see java.util.EventListener
 * @since 1.0.0
 */
public interface ProcessInputStreamListener extends EventListener {

  /**
   * Event callback method invoked when a line of input is sent from a {@link Process Process's} standard output
   * or standard error streams.
   *
   * @param line {@link String} containing the contents of the {@link Process Process's} standard output
   * or standard error streams.
   * @see java.lang.Process#getErrorStream()
   * @see java.lang.Process#getInputStream()
   */
  void onInput(String line);

}
