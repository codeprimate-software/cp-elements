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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.easymock.EasyMock;
import org.junit.Test;

/**
 * The NetworkUtilsTest class is a test suite for testing the contract and functionality of the NetworkUtils class.
 *
 * @author John J. Blum
 * @since 1.0.0
 * @see org.cp.elements.net.NetworkUtils
 * @see org.easymock.EasyMock
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class NetworkUtilsTest {

  @Test
  public void testCloseServerSocket() throws Exception {
    final ServerSocket mockServerSocket = EasyMock.createMock(ServerSocket.class);
    mockServerSocket.close();

    EasyMock.expectLastCall().once();
    EasyMock.replay(mockServerSocket);

    assertTrue(NetworkUtils.close(mockServerSocket));

    EasyMock.verify(mockServerSocket);
  }

  @Test
  public void testCloseServerSocketThrowsIOException() throws Exception {
    final ServerSocket mockServerSocket = EasyMock.createMock(ServerSocket.class);
    mockServerSocket.close();

    EasyMock.expectLastCall().andThrow(new IOException("IO Error!"));
    EasyMock.replay(mockServerSocket);

    assertFalse(NetworkUtils.close(mockServerSocket));

    EasyMock.verify(mockServerSocket);
  }

  @Test
  public void testCloseServerSocketWithNull() throws Exception {
    assertFalse(NetworkUtils.close((ServerSocket) null));
  }

  @Test
  public void testCloseSocket() throws Exception {
    final Socket mockSocket = EasyMock.createMock(Socket.class);
    mockSocket.close();

    EasyMock.expectLastCall().once();
    EasyMock.replay(mockSocket);

    assertTrue(NetworkUtils.close(mockSocket));

    EasyMock.verify(mockSocket);
  }

  @Test
  public void testCloseSocketThrowsIOException() throws Exception {
    final Socket mockSocket = EasyMock.createMock(Socket.class);
    mockSocket.close();

    EasyMock.expectLastCall().andThrow(new IOException("IO Error!"));
    EasyMock.replay(mockSocket);

    assertFalse(NetworkUtils.close(mockSocket));

    EasyMock.verify(mockSocket);
  }

  @Test
  public void testCloseSocketWithNull() throws Exception {
    assertFalse(NetworkUtils.close((Socket) null));
  }

}
