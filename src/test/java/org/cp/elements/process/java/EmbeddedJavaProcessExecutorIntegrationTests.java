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
package org.cp.elements.process.java;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.net.NetworkUtils.availablePort;
import static org.cp.elements.net.NetworkUtils.parsePort;
import static org.cp.elements.process.java.EmbeddedJavaProcessExecutor.newEmbeddedJavaProcessExecutor;
import static org.cp.elements.tools.net.EchoClient.newEchoClient;

import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Executable;
import org.cp.elements.tools.net.EchoClient;
import org.cp.elements.tools.net.EchoServer;

/**
 * Integration Tests for the {@link EmbeddedJavaProcessExecutor}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.process.java.EmbeddedJavaProcessExecutor
 * @since 1.0.0
 */
public class EmbeddedJavaProcessExecutorIntegrationTests {

  @BeforeAll
  public static void setup() {

    String[] commandLine = {
      "java", "-server", "-ea", "-classpath", System.getProperty("java.class.path"),
      EchoServerExecutable.class.getName(), String.valueOf(availablePort())
    };

    newEmbeddedJavaProcessExecutor().execute(FileSystemUtils.WORKING_DIRECTORY, commandLine);

    assertThat(EchoServerExecutable.echoServer().isRunning()).isTrue();
  }

  @AfterAll
  public static void tearDown() {
    assertThat(EchoServerExecutable.shutdown()).isTrue();
  }

  @Test
  public void embeddedJavaProcessIsRunning() {

    EchoClient echoClient = newEchoClient(EchoServerExecutable.getPort());

    assertThat(echoClient.sendMessage("hello")).isEqualTo("hello");
    assertThat(echoClient.sendMessage("whatchaDoin")).isEqualTo("whatchaDoin");
    assertThat(echoClient.sendMessage("good-bye")).isEqualTo("good-bye");
  }

  public static class EchoServerExecutable implements Executable<Object> {

    static final AtomicReference<EchoServer> echoServerReference = new AtomicReference<>();

    private static EchoServer echoServer() {
      EchoServer echoServer = echoServerReference.get();
      Assert.state(echoServer != null, "EchoServer was not properly initialized");
      return echoServer;
    }

    private static int getPort() {
      return echoServer().getPort();
    }

    private static boolean shutdown() {
      EchoServer echoServer = echoServer();
      echoServer.shutdown();
      return echoServer.isNotRunning();
    }

    @Override
    public boolean isRunning() {
      return echoServer().isRunning();
    }

    @Override
    public Object execute(Object... args) {

      EchoServer echoServer = new EchoServer(parsePort(String.valueOf(args[0]))) {
        @Override protected Logger getLogger() {
          Logger logger = super.getLogger();
          logger.setLevel(Level.OFF);
          return logger;
        }
      };

      echoServerReference.set(echoServer.runAndWaitFor());

      return null;
    }
  }
}
