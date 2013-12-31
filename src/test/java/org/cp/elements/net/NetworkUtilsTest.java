/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.net;

import static org.junit.Assert.*;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import org.easymock.EasyMock;
import org.junit.Test;

/**
 * The NetworkUtilsTest class is a test suite for testing the contract and functionality of the NetworkUtils class.
 * </p>
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
