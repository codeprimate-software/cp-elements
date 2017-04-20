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

package org.cp.elements.tools.net;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.lang.CheckedExceptionsFactory.newIOException;
import static org.cp.elements.test.TestUtils.timeIt;
import static org.cp.elements.tools.net.EchoServer.newEchoServer;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyLong;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Collections;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cp.elements.test.annotation.SubjectUnderTest;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import edu.umd.cs.mtc.MultithreadedTestCase;
import edu.umd.cs.mtc.TestFramework;

/**
 * Unit tests for {@link EchoServer}.
 *
 * @author John Blum
 * @see java.net.ServerSocket
 * @see java.net.Socket
 * @see java.util.concurrent.ExecutorService
 * @see org.cp.elements.test.annotation.SubjectUnderTest
 * @see org.cp.elements.tools.net.EchoServer
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see edu.umd.cs.mtc.MultithreadedTestCase
 * @see edu.umd.cs.mtc.TestFramework
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class EchoServerTests {

  @Mock
  private ExecutorService mockExecutorService;

  @SubjectUnderTest
  private EchoServer testEchoServer;

  @Mock
  private ServerSocket mockServerSocket;

  @Mock
  private Socket mockSocket;

  @Before
  public void setup() {
    testEchoServer = spy(new TestEchoServer(1234));

    doReturn(mockExecutorService).when(testEchoServer).newExecutorService();

    when(mockExecutorService.submit(any(Runnable.class))).thenAnswer(invocationOnMock -> {
      Runnable runnable = invocationOnMock.getArgument(0);
      runnable.run();
      return null;
    });
  }

  @Test
  public void constructServerSocketIsSuccessful() {
    EchoServer echoServer = new EchoServer(1234) {
      @Override public ServerSocket newServerSocket(int port) {
        when(mockServerSocket.getLocalPort()).thenReturn(port);
        return mockServerSocket;
      }
    };

    assertThat(echoServer).isNotNull();
    assertThat(echoServer.getPort()).isEqualTo(1234);
    assertThat(echoServer.getServerSocket()).isNotNull();
    assertThat(echoServer.getServerSocket().getLocalPort()).isEqualTo(1234);
  }

  @Test(expected = IllegalArgumentException.class)
  public void newEchoServerWithNegativePortThrowsIllegalArgumentException() {
    try  {
      newEchoServer(-1);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("Port [-1] must be greater than 1024 and less than equal to 65535");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void newEchoServerWithOverflowPortThrowsIllegalArgumentException() {
    try  {
      newEchoServer(123456789);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("Port [123456789] must be greater than 1024 and less than equal to 65535");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void newEchoServerIsNotRunning() {
    when(mockServerSocket.isClosed()).thenReturn(false);
    when(mockServerSocket.isBound()).thenReturn(false);

    assertThat(testEchoServer.isNotRunning()).isTrue();
    assertThat(testEchoServer.isRunning()).isFalse();

    verify(mockServerSocket, times(2)).isClosed();
    verify(mockServerSocket, times(2)).isBound();
  }

  @Test
  public void newEchoServerIsRunning() {
    when(mockServerSocket.isClosed()).thenReturn(false);
    when(mockServerSocket.isBound()).thenReturn(true);

    assertThat(testEchoServer.isNotRunning()).isFalse();
    assertThat(testEchoServer.isRunning()).isTrue();

    verify(mockServerSocket, times(2)).isClosed();
    verify(mockServerSocket, times(2)).isBound();
  }

  @Test
  public void runCallsRunEchoServiceWithServerSocket() {
    doNothing().when(testEchoServer).runEchoService(any(ServerSocket.class));

    testEchoServer.run();

    verify(testEchoServer, times(1)).runEchoService(eq(mockServerSocket));
  }

  @Test
  public void runAndWaitForCallsRunAndWaitForWithDefaultDuration() {
    doReturn(testEchoServer).when(testEchoServer).runAndWaitFor(anyLong());

    assertThat(testEchoServer.runAndWaitFor()).isSameAs(testEchoServer);

    verify(testEchoServer, times(1))
      .runAndWaitFor(eq(TestEchoServer.DEFAULT_DURATION_MILLISECONDS));
  }

  @Test
  public void runAndWaitForWithDurationCallsRunThenWaitForWithDuration() {
    doNothing().when(testEchoServer).runEchoService(any(ServerSocket.class));
    doReturn(true).when(testEchoServer).waitFor(anyLong());

    assertThat(testEchoServer.runAndWaitFor(1000L)).isSameAs(testEchoServer);

    verify(testEchoServer, times(1)).run();
    verify(testEchoServer, times(1)).runEchoService(eq(mockServerSocket));
    verify(testEchoServer, times(1)).waitFor(eq(1000L));
  }

  @Test
  public void runEchoServiceRunsCorrectly() throws IOException {
    String expectedMessage = "This is the end of the line!";

    ByteArrayOutputStream outputStream = new ByteArrayOutputStream(expectedMessage.getBytes().length);

    doReturn(true).when(mockServerSocket).isBound();
    doReturn(false).doReturn(false).doReturn(true).when(mockServerSocket).isClosed();
    doReturn(mockSocket).when(mockServerSocket).accept();
    doReturn(new ByteArrayInputStream(expectedMessage.getBytes())).when(mockSocket).getInputStream();
    doReturn(outputStream).when(mockSocket).getOutputStream();

    testEchoServer.runEchoService(mockServerSocket);

    assertThat(new String(outputStream.toByteArray()).trim()).isEqualTo(expectedMessage);

    verify(mockExecutorService, times(2)).submit(isA(Runnable.class));
    verify(mockServerSocket, times(1)).accept();
    verify(mockServerSocket, times(2)).isBound();
    verify(mockServerSocket, times(3)).isClosed();
    verify(mockSocket, times(1)).getInputStream();
    verify(mockSocket, times(1)).getOutputStream();
    verify(mockSocket, times(1)).close();
  }

  @Test
  public void runEchoServiceQuietlyHandlesIOException() throws IOException {
    when(mockServerSocket.accept()).thenThrow(newIOException("test"));
    doReturn(true).when(mockServerSocket).isBound();
    doReturn(false).doReturn(false).doReturn(true).when(mockServerSocket).isClosed();

    testEchoServer.runEchoService(mockServerSocket);

    verify(testEchoServer, never()).receiveMessage(any(Socket.class));
    verify(testEchoServer, never()).sendResponse(any(Socket.class), anyString());
    verify(mockExecutorService, times(1)).submit(isA(Runnable.class));
    verify(mockServerSocket, times(1)).accept();
    verify(mockServerSocket, times(2)).isBound();
    verify(mockServerSocket, times(3)).isClosed();
  }

  @Test
  public void receiveMessageSaysHello() throws IOException {
    doReturn(new ByteArrayInputStream("Hello".getBytes())).when(mockSocket).getInputStream();

    assertThat(testEchoServer.receiveMessage(mockSocket)).isEqualTo("Hello");

    verify(mockSocket, times(1)).getInputStream();
  }

  @Test
  public void receiveMessageSaysWhat() throws IOException {
    doThrow(newIOException("test")).when(mockSocket).getInputStream();

    assertThat(testEchoServer.receiveMessage(mockSocket)).isEqualTo("What?");

    verify(mockSocket, times(1)).getInputStream();
  }

  @Test
  public void sendResponseCallsSendMessage() throws IOException {
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

    doReturn(outputStream).when(mockSocket).getOutputStream();

    testEchoServer.sendResponse(mockSocket, "Good-bye");

    assertThat(new String(outputStream.toByteArray())).isEqualTo("Good-bye\n");

    verify(mockSocket, times(1)).getOutputStream();
  }

  @Test
  public void shutdownClosesServerSocketAndStopsEchoService() {
    doNothing().when(testEchoServer).closeServerSocket();
    doReturn(false).when(testEchoServer).stopEchoService();

    testEchoServer.shutdown();

    verify(testEchoServer, times(1)).closeServerSocket();
    verify(testEchoServer, times(1)).stopEchoService();
  }

  @Test
  public void closeServerSocketIsCorrect() throws IOException {
    testEchoServer.closeServerSocket();

    verify(mockServerSocket, times(1)).close();
  }

  @Test
  @SuppressWarnings("all")
  public void stopEchoServiceDoesNothingWhenEchoServiceIsNull() {
    doReturn(null).when(testEchoServer).getEchoService();
    assertThat(testEchoServer.stopEchoService()).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void stopEchoServiceStopsOnShutdown() throws InterruptedException {
    doReturn(mockExecutorService).when(testEchoServer).getEchoService();
    when(mockExecutorService.awaitTermination(anyLong(), any(TimeUnit.class))).thenReturn(true);
    when(mockExecutorService.isShutdown()).thenReturn(true);

    assertThat(testEchoServer.stopEchoService()).isTrue();

    verify(mockExecutorService, times(1)).shutdown();
    verify(mockExecutorService, times(1)).awaitTermination(eq(30L), eq(TimeUnit.SECONDS));
    verify(mockExecutorService, never()).shutdownNow();
    verify(mockExecutorService, times(1)).isShutdown();
  }

  @Test
  @SuppressWarnings("all")
  public void stopEchoServiceAwaitsTerminationThenStopsOnShutdownNow() throws InterruptedException {
    doReturn(mockExecutorService).when(testEchoServer).getEchoService();
    when(mockExecutorService.awaitTermination(anyLong(), any(TimeUnit.class))).thenReturn(false).thenReturn(true);
    when(mockExecutorService.isShutdown()).thenReturn(true);

    assertThat(testEchoServer.stopEchoService()).isTrue();

    verify(mockExecutorService, times(1)).shutdown();
    verify(mockExecutorService, times(2)).awaitTermination(eq(30L), eq(TimeUnit.SECONDS));
    verify(mockExecutorService, times(1)).shutdownNow();
    verify(mockExecutorService, times(1)).isShutdown();
  }

  @Test
  @SuppressWarnings("all")
  public void stopEchoServiceFailsToShutdown() throws InterruptedException {
    doReturn(mockExecutorService).when(testEchoServer).getEchoService();
    when(mockExecutorService.awaitTermination(anyLong(), any(TimeUnit.class))).thenReturn(false).thenReturn(false);
    when(mockExecutorService.isShutdown()).thenReturn(false);

    testEchoServer.stopEchoService();

    verify(mockExecutorService, times(1)).shutdown();
    verify(mockExecutorService, times(2)).awaitTermination(eq(30L), eq(TimeUnit.SECONDS));
    verify(mockExecutorService, times(1)).shutdownNow();
    verify(mockExecutorService, times(1)).isShutdown();
  }

  @Test
  public void stopEchoServiceHandlesInterruptedExceptionProperly() throws Throwable {
    TestFramework.runOnce(new StopEchoServiceInterruptedTest());
  }

  @Test
  public void waitForCallsWaitForWithDuration() {
    doReturn(true).when(testEchoServer).waitFor(anyLong());
    assertThat(testEchoServer.waitFor()).isTrue();
    verify(testEchoServer, times(1)).waitFor(eq(EchoServer.DEFAULT_DURATION_MILLISECONDS));
  }

  @Test
  public void waitForWaitsForDuration() {
    doReturn(false).when(testEchoServer).isRunning();
    assertThat(timeIt(() -> testEchoServer.waitFor(750L))).isGreaterThanOrEqualTo(750L);
    verify(testEchoServer, atLeast(2)).isRunning();
  }

  class StopEchoServiceInterruptedTest extends MultithreadedTestCase {

    private final Object mutex = new Object();

    @Override
    @SuppressWarnings("all")
    public void initialize() {
      try {
        super.initialize();

        doReturn(mockExecutorService).when(testEchoServer).getEchoService();
        when(mockExecutorService.awaitTermination(anyLong(), any(TimeUnit.class))).thenReturn(false);
        when(mockExecutorService.isShutdown()).thenReturn(false);

        when(mockExecutorService.shutdownNow()).thenAnswer(invocationOnMock -> {
          synchronized (mutex) {
            mutex.wait();
          }

          return Collections.emptyList();
        }).thenReturn(Collections.emptyList());
      }
      catch (InterruptedException ignore) {
      }
    }

    @SuppressWarnings("unused")
    public void thread1() {
      Thread.currentThread().setName("Stop EchoService Thread");

      assertThat(Thread.currentThread().isInterrupted()).isFalse();
      assertThat(testEchoServer.stopEchoService()).isFalse();
      assertThat(Thread.interrupted()).isTrue();
    }

    @SuppressWarnings("unused")
    public void thread2() {
      Thread.currentThread().setName("Interrupting Thread");
      waitForTick(1);
      getThread(1).interrupt();
    }

    @Override
    public void finish() {
      try {
        verify(mockExecutorService, times(1)).shutdown();
        verify(mockExecutorService, times(1)).awaitTermination(eq(30L), eq(TimeUnit.SECONDS));
        verify(mockExecutorService, times(1)).shutdownNow();
        verify(mockExecutorService, times(1)).isShutdown();
      }
      catch (InterruptedException ignore) {
      }
    }
  }

  class TestEchoServer extends EchoServer {

    TestEchoServer(int port) {
      super(port);
    }

    @Override
    protected Logger getLogger() {
      Logger logger = super.getLogger();
      logger.setLevel(Level.OFF);
      return logger;
    }

    @Override
    protected ServerSocket newServerSocket() throws IOException {
      return mockServerSocket;
    }
  }
}
