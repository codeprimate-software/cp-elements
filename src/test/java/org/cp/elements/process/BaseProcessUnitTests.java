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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import org.junit.jupiter.api.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.io.FileUtils;

/**
 * Unit Tests for {@link AbstractBaseProcess}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.lang.Process
 * @see org.junit.jupiter.api.Test
 * @see AbstractBaseProcess
 * @since 1.0.0
 */
public class BaseProcessUnitTests {

  private final AbstractBaseProcess testProcess = new TestBaseProcess();

  @Test
  public void getIdFromPidFile() throws IOException {

    File pid = new File(FileSystemUtils.WORKING_DIRECTORY, ".pid");

    pid.deleteOnExit();

    try {
      assertThat(pid.createNewFile()).isTrue();
      assertThat(pid.isFile()).isTrue();
      assertThat(FileUtils.write(new ByteArrayInputStream(String.valueOf(248).getBytes()), pid)).isSameAs(pid);
      assertThat(this.testProcess.getId()).isEqualTo(248);
    }
    finally {
      FileSystemUtils.delete(pid);
      assertThat(pid.exists()).isFalse();
    }
  }

  @Test
  public void setIdIsNotAllowed() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(() -> this.testProcess.setId(123))
      .withMessage("Setting the ID of the Process is not supported;"
        + " a Process ID (PID) is assigned by the Operating System")
      .withNoCause();
  }

  @Test
  public void getNameReturnsNull() {
    assertThat(this.testProcess.getName()).isNull();
  }

  @Test
  public void getErrorStreamIsUnsupported() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(this.testProcess::getErrorStream)
      .withNoCause();
  }

  @Test
  public void getInputStreamIsUnsupported() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(this.testProcess::getInputStream)
      .withNoCause();
  }

  @Test
  public void getOutputStreamIsUnsupported() {

    assertThatExceptionOfType(UnsupportedOperationException.class)
      .isThrownBy(this.testProcess::getOutputStream)
      .withNoCause();
  }

  @Test
  public void exitValueIsMinusOne() {
    assertThat(this.testProcess.exitValue()).isEqualTo(-1);
  }

  static final class TestBaseProcess extends AbstractBaseProcess { }

}
