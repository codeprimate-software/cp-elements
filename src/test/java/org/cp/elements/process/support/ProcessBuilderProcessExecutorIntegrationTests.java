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

package org.cp.elements.process.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.concurrent.ThreadUtils.waitFor;
import static org.cp.elements.process.support.ProcessBuilderProcessExecutor.newProcessBuilderProcessExecutor;
import static org.cp.elements.tools.net.ConnectionTester.newConnectionTester;
import static org.cp.elements.tools.net.EchoClient.newEchoClient;

import java.util.concurrent.TimeUnit;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.net.NetworkUtils;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.tools.net.EchoServer;
import org.junit.Test;

/**
 * Integration tests for {@link ProcessBuilderProcessExecutor}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.process.support.ProcessBuilderProcessExecutor
 * @since 1.0.0
 */
public class ProcessBuilderProcessExecutorIntegrationTests {

  private static final long TIMEOUT = 5;

  private static final TimeUnit TIMEOUT_TIME_UNIT = TimeUnit.SECONDS;

  @Test
  public void executeProgram() {

    int availablePort = NetworkUtils.availablePort();

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", System.getProperty("java.class.path"), EchoServer.class.getName(),
      String.valueOf(availablePort)
    };

    ProcessAdapter process = newProcessBuilderProcessExecutor().execute(FileSystemUtils.WORKING_DIRECTORY, commandLine);

    assertThat(process).isNotNull();

    waitFor(TIMEOUT, TIMEOUT_TIME_UNIT).checkEvery(500, TimeUnit.MILLISECONDS).on(process::isRunning);
    waitFor(TIMEOUT, TIMEOUT_TIME_UNIT).checkEvery(500, TimeUnit.MICROSECONDS).on(newConnectionTester(availablePort));

    assertThat(process.isRunning()).isTrue();
    assertThat(newEchoClient(availablePort).sendMessage("test")).isEqualTo("test");

    process.stopAndWait(TIMEOUT, TIMEOUT_TIME_UNIT);

    assertThat(process.isRunning()).isFalse();
  }
}
