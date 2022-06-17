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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.io.FileUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link BaseProcess}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.lang.Process
 * @see org.junit.Test
 * @see org.cp.elements.process.BaseProcess
 * @since 1.0.0
 */
public class BaseProcessUnitTests {

  private final BaseProcess testProcess = new TestProcess();

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

  @Test(expected = UnsupportedOperationException.class)
  public void setIdIsNotAllowed() {

    try {
      this.testProcess.setId(123);
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage("Setting the ID of the Process is not supported;"
        + " a Process ID (PID) is assigned by the Operating System");

      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void getNameReturnsNull() {
    assertThat(this.testProcess.getName()).isNull();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void getErrorStreamIsUnsupported() {
    this.testProcess.getErrorStream();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void getInputStreamIsUnsupported() {
    this.testProcess.getInputStream();
  }

  @Test(expected = UnsupportedOperationException.class)
  public void getOutputStreamIsUnsupported() {
    this.testProcess.getOutputStream();
  }

  @Test
  public void exitValueIsMinusOne() {
    assertThat(this.testProcess.exitValue()).isEqualTo(-1);
  }

  static final class TestProcess extends BaseProcess { }

}
