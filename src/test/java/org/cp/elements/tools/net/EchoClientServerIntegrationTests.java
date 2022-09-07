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
package org.cp.elements.tools.net;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.net.NetworkUtils;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Integration tests for {@link EchoClient} and {@link EchoServer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.net.NetworkUtils
 * @see org.cp.elements.tools.net.EchoClient
 * @see org.cp.elements.tools.net.EchoServer
 * @since 1.0.0
 */
public class EchoClientServerIntegrationTests {

  private static EchoClient echoClient;
  private static EchoServer echoServer;

  @BeforeClass
  public static void setup() {

    echoServer = new EchoServer(NetworkUtils.availablePort()) {
      @Override protected Logger getLogger() {
        Logger logger = super.getLogger();
        logger.setLevel(Level.OFF);
        return logger;
      }
    };

    assertThat(echoServer.runAndWaitFor()).isSameAs(echoServer);
    assertThat(echoServer.isRunning()).isTrue();

    echoClient = EchoClient.newEchoClient(echoServer.getPort());
  }

  @AfterClass
  public static void tearDown() {
    echoServer.shutdown();
    assertThat(echoServer.isRunning()).isFalse();
  }

  @Test
  public void sendReceiveMessagesIsSuccessful() {
    assertThat(echoClient.sendMessage("Hello")).isEqualTo("Hello");
    assertThat(echoClient.sendMessage("Test")).isEqualTo("Test");
    assertThat(echoClient.sendMessage("Good-bye")).isEqualTo("Good-bye");
  }
}
