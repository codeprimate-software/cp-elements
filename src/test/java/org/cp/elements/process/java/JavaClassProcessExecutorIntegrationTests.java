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

package org.cp.elements.process.java;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.concurrent.ThreadUtils.waitFor;
import static org.cp.elements.process.java.JavaProcessExecutor.newJavaProcessExecutor;
import static org.cp.elements.tools.net.ConnectionTester.newConnectionTester;
import static org.cp.elements.tools.net.EchoClient.newEchoClient;

import java.util.concurrent.TimeUnit;

import org.cp.elements.net.NetworkUtils;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.tools.net.EchoClient;
import org.cp.elements.tools.net.EchoServer;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Integration tests for {@link JavaProcessExecutor} testing the Java Class execution functionality.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.process.ProcessAdapter
 * @see org.cp.elements.process.java.JavaProcessExecutor
 * @see org.cp.elements.tools.net.EchoClient
 * @see org.cp.elements.tools.net.EchoServer
 * @since 1.0.0
 */
public class JavaClassProcessExecutorIntegrationTests {

  private static int availablePort;

  private static final long TIMEOUT = 5;

  private static ProcessAdapter process;

  private static final TimeUnit TIMEOUT_TIME_UNIT = TimeUnit.SECONDS;

  @BeforeClass
  public static void setup() {
    availablePort = NetworkUtils.availablePort();
    process = newJavaProcessExecutor().execute(EchoServer.class, String.valueOf(availablePort));

    assertThat(process).isNotNull();

    waitFor(TIMEOUT, TIMEOUT_TIME_UNIT).checkEvery(500, TimeUnit.MILLISECONDS).on(process::isRunning);

    assertThat(waitFor(TIMEOUT, TIMEOUT_TIME_UNIT).checkEvery(500, TimeUnit.MICROSECONDS)
      .on(newConnectionTester(availablePort))).isTrue();
  }

  @AfterClass
  public static void tearDown() {
    process.stopAndWait(TIMEOUT, TIMEOUT_TIME_UNIT);

    assertThat(process.isRunning()).isFalse();
  }

  @Test
  public void forkedJavaClassProcessIsRunning() {
    EchoClient echoClient = newEchoClient(availablePort);

    assertThat(echoClient.sendMessage("hello")).isEqualTo("hello");
    assertThat(echoClient.sendMessage("whatchaDoin")).isEqualTo("whatchaDoin");
    assertThat(echoClient.sendMessage("good-bye")).isEqualTo("good-bye");
  }
}
