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

package org.cp.elements.process.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.process.support.RuntimeProcessExecutor.newRuntimeProcessExecutor;

import java.util.Scanner;
import java.util.concurrent.TimeUnit;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.process.ProcessAdapter;
import org.junit.Test;

/**
 * Integration tests for {@link RuntimeProcessExecutor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.process.support.RuntimeProcessExecutor
 * @since 1.0.0
 */
public class RuntimeProcessExecutorIntegrationTests {

  @Test
  public void executeProgram() {
    String[] commandLine = { "java", "-server", "-ea", "-classpath",
      System.getProperty("java.class.path"), TestProgram.class.getName() };

    StringBuilder buffer = new StringBuilder();

    ProcessAdapter process = newRuntimeProcessExecutor().execute(FileSystemUtils.WORKING_DIRECTORY, commandLine);

    assertThat(process).isNotNull();

    process.register(buffer::append);
    process.init();

    assertThat(process.isInitialized()).isTrue();
    assertThat(process.isRunning()).isTrue();

    process.stopAndWait(5L, TimeUnit.SECONDS);

    assertThat(process.isRunning()).isFalse();
    assertThat(buffer.toString()).isEqualTo("I am here!");
  }

  public static final class TestProgram {

    public static void main(String[] args) throws Exception {
      System.out.println("I am here!");
      System.out.flush();
      new Scanner(System.in).nextLine();
    }
  }
}
