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

package org.cp.elements.net;

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link NetworkUtils} class.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.net.NetworkUtils
 * @since 1.0.0
 */
public class NetworkUtilsTests {

  @Test
  public void availablePortReturnsNonZeroPortGreaterThan1024AndLessThan65536() {
    assertThat(NetworkUtils.availablePort(), is(greaterThan(ServicePort.MIN_PORT)));
    assertThat(NetworkUtils.availablePort(), is(lessThanOrEqualTo(ServicePort.MAX_PORT)));
  }

  @Test
  public void closeServerSocketReturnsTrue() throws Exception {
    ServerSocket mockServerSocket = mock(ServerSocket.class);

    assertThat(NetworkUtils.close(mockServerSocket), is(true));

    verify(mockServerSocket, times(1)).close();
  }

  @Test
  public void closeServerSocketReturnsFalse() throws Exception {
    ServerSocket mockServerSocket = mock(ServerSocket.class);

    doThrow(new IOException("test")).when(mockServerSocket).close();

    assertThat(NetworkUtils.close(mockServerSocket), is(false));

    verify(mockServerSocket, times(1)).close();
  }

  @Test
  public void closeNullServerSocketReturnsFalse() {
    assertThat(NetworkUtils.close((ServerSocket) null), is(false));
  }

  @Test
  public void closeSocketReturnsTrue() throws Exception {
    Socket mockSocket = mock(Socket.class);

    assertThat(NetworkUtils.close(mockSocket), is(true));

    verify(mockSocket, times(1)).close();
  }

  @Test
  public void closeSocketReturnsFalse() throws Exception {
    Socket mockSocket = mock(Socket.class);

    doThrow(new IOException("test")).when(mockSocket).close();

    assertThat(NetworkUtils.close(mockSocket), is(false));

    verify(mockSocket, times(1)).close();
  }

  @Test
  public void closeNullSocketReturnsFalse() {
    assertThat(NetworkUtils.close((Socket) null), is(false));
  }
}
