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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.cp.elements.lang.NumberUtils.intValue;
import static org.cp.elements.tools.net.EchoClient.newEchoClient;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.net.Socket;
import java.net.SocketAddress;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.test.annotation.SubjectUnderTest;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link EchoClient}.
 *
 * @author John Blum
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.cp.elements.tools.net.EchoClient
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class EchoClientTests {

  @Mock
  private OutputStream mockOutputStream;

  @Mock
  private Socket mockSocket;

  @SubjectUnderTest
  private EchoClient testEchoClient;

  @BeforeEach
  public void setup() {
    this.testEchoClient = spy(new TestEchoClient(1234));
  }

  @Test
  public void newEchoClientWithHostAndPortIsSuccessful() {

    EchoClient echoClient = newEchoClient("localhost", 1234);

    assertThat(echoClient).isNotNull();
    assertThat(echoClient.getHost()).isEqualTo("localhost");
    assertThat(echoClient.getPort()).isEqualTo(1234);
  }

  @Test
  public void newEchoClientWithNullHostIsValid() {

    EchoClient echoClient = newEchoClient(1234);

    assertThat(echoClient).isNotNull();
    assertThat(echoClient.getHost()).isNull();
    assertThat(echoClient.getPort()).isEqualTo(1234);
  }

  @Test
  public void newEchoClientWithNegativePortThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newEchoClient(-1))
      .withMessage("Port [-1] must be greater than 0 and less than equal to 65535")
      .withNoCause();
  }

  @Test
  public void newEchoClientWithOverflowPortThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> newEchoClient(65536))
      .withMessage("Port [65536] must be greater than 0 and less than equal to 65535")
      .withNoCause();
  }

  @Test
  public void sendMessageReceiveResponse() throws IOException {

    PipedOutputStream pipedOutputStream = new PipedOutputStream();
    PipedInputStream pipedInputStream = new PipedInputStream(pipedOutputStream);

    when(mockSocket.getInputStream()).thenReturn(pipedInputStream);
    when(mockSocket.getOutputStream()).thenReturn(pipedOutputStream);
    doNothing().when(mockSocket).connect(any(SocketAddress.class));

    assertThat(testEchoClient.sendMessage("test")).isEqualTo("test");

    verify(mockSocket, times(1)).setReuseAddress(eq(EchoClient.DEFAULT_REUSE_ADDRESS));
    verify(mockSocket, times(1)).setSoTimeout(eq(intValue(EchoClient.DEFAULT_SO_TIMEOUT)));
    verify(mockSocket, times(1)).connect(isA(SocketAddress.class));
    verify(mockSocket, times(1)).close();
  }

  @Test
  public void sendMessageFailToReceiveResponse() throws IOException {

    when(mockSocket.getInputStream()).thenThrow(newIOException("test"));
    when(mockSocket.getOutputStream()).thenReturn(mockOutputStream);
    doNothing().when(mockSocket).connect(any(SocketAddress.class));

    assertThat(testEchoClient.sendMessage("test")).isEqualTo("No Reply");

    verify(mockSocket, times(1)).setReuseAddress(eq(EchoClient.DEFAULT_REUSE_ADDRESS));
    verify(mockSocket, times(1)).setSoTimeout(eq(intValue(EchoClient.DEFAULT_SO_TIMEOUT)));
    verify(mockSocket, times(1)).connect(isA(SocketAddress.class));
    verify(mockSocket, times(1)).close();
  }

  class TestEchoClient extends EchoClient {

    TestEchoClient(int port) {
      super(port);
    }

    @SuppressWarnings("unused")
    TestEchoClient(String host, int port) {
      super(host, port);
    }

    @Override
    protected Logger getLogger() {
      Logger logger = super.getLogger();
      logger.setLevel(Level.OFF);
      return logger;
    }

    @Override
    protected Socket newSocket() {
      return EchoClientTests.this.mockSocket;
    }
  }
}
