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
package org.cp.elements.process;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Nameable;
import org.cp.elements.process.util.ProcessUtils;

/**
 * Abstract base class extending {@link Process} in order to simplify the implementation
 * of different {@link Process} types.
 *
 * Additionally, this {@link Process} base class implementation adds {@link Integer ID}
 * and {@link String name} properties to the {@link Process} object.
 *
 * @author John Blum
 * @see java.lang.Process
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractBaseProcess extends Process implements Identifiable<Integer>, Nameable<String> {

  /**
   * Gets the Operating System (OS) assigned {@link Integer ID} for this {@link Process}.
   *
   * @return the {@link Integer ID} for this {@link Process}; may be {@literal null} if the {@literal PID}
   * cannot be determined.
   */
  @Override
  public Integer getId() {

    return Optional.ofNullable(ProcessUtils.findPidFile(FileSystemUtils.WORKING_DIRECTORY))
      .map(ProcessUtils::readPid)
      .orElseGet(ProcessUtils::getProcessId);
  }

  /**
   * Setting the {@link Process} {@literal ID} is not supported.
   *
   * @throws UnsupportedOperationException the {@literal ID} of a {@link Process}
   * is assigned by the Operating System (OS).
   */
  @Override
  public final void setId(Integer id) {
    throw new UnsupportedOperationException("Setting the ID of the Process is not supported;"
      + " a Process ID (PID) is assigned by the Operating System");
  }

  /**
   * Gets the Operating System (OS) assigned {@link String name} for this {@link Process}.
   *
   * @return the {@link String name} for this {@link Process}; may be {@literal null}
   * if the {@link Process} {@link String name} cannot be determined.
   */
  @Override
  public String getName() {
    return null;
  }

  @Override
  public InputStream getErrorStream() {
    throw new UnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
  }

  @Override
  public InputStream getInputStream() {
    throw new UnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
  }

  @Override
  public OutputStream getOutputStream() {
    throw new UnsupportedOperationException(Constants.UNSUPPORTED_OPERATION);
  }

  @Override
  public int exitValue() {
    return -1;
  }

  @Override
  public void destroy() {
    ProcessUtils.kill(this);
  }

  @Override
  public int waitFor() throws InterruptedException {

    while (isAlive()) {
      synchronized (this) {
        waitFor(100, TimeUnit.MILLISECONDS);
      }
    }

    return exitValue();
  }
}
