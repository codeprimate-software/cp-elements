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
import static org.assertj.core.api.Assertions.fail;
import static org.cp.elements.lang.concurrent.ThreadUtils.waitFor;
import static org.cp.elements.process.java.JavaProcessExecutor.newJavaProcessExecutor;
import static org.cp.elements.tools.net.ConnectionTester.newConnectionTester;
import static org.cp.elements.tools.net.EchoClient.newEchoClient;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.net.NetworkUtils;
import org.cp.elements.process.ProcessAdapter;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.tools.net.EchoClient;
import org.cp.elements.tools.net.EchoServer;

/**
 * Integration Tests for {@link JavaProcessExecutor} testing the JAR execution functionality.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.process.ProcessAdapter
 * @see org.cp.elements.process.java.JavaProcessExecutor
 * @see org.cp.elements.test.AbstractBaseTestSuite
 * @see org.cp.elements.tools.net.EchoClient
 * @see org.cp.elements.tools.net.EchoServer
 * @since 1.0.0
 */
public class JarFileProcessExecutorIntegrationTests extends AbstractBaseTestSuite {

  private static final long TIMEOUT = 5;

  private static final TimeUnit TIMEOUT_TIME_UNIT = TimeUnit.SECONDS;

  private int availablePort;

  private ProcessAdapter process;

  @BeforeEach
  public void setup() {

    this.availablePort = NetworkUtils.availablePort();
    this.process = newJavaProcessExecutor().execute(buildJarFile(), String.valueOf(this.availablePort));

    assertThat(this.process).isNotNull();

    waitFor(TIMEOUT, TIMEOUT_TIME_UNIT)
      .checkEvery(500L)
      .on(() -> this.process.isRunning());

    assertThat(waitFor(TIMEOUT, TIMEOUT_TIME_UNIT)
      .checkEvery(1, TIMEOUT_TIME_UNIT)
      .on(newConnectionTester(this.availablePort))).isTrue();

  }

  private File buildJarFile() {

    File jarFile = newJarFile("echoServer.jar");

    JarOutputStream jarFileOutputStream = null;

    try {
      jarFileOutputStream = new JarOutputStream(new FileOutputStream(jarFile), newManifest(jarFile.getParentFile()));
      jarFileOutputStream.flush();
    }
    catch (IOException cause) {
      fail("Failed to write JAR file [%s] to the file system", jarFile);
    }
    finally {
      FileSystemUtils.close(jarFileOutputStream);
    }

    assertThat(jarFile).exists();

    return jarFile;
  }

  private File newJarFile(String name) {

    File jarFile = new File(getBuildDirectory(), name);

    jarFile.deleteOnExit();

    return jarFile;
  }

  private Manifest newManifest(File jarFileDirectory) {

    Manifest manifest = new Manifest();

    Attributes mainAttributes = manifest.getMainAttributes();

    mainAttributes.putValue("Manifest-Version", "1.0");
    mainAttributes.putValue("Name", "EchoServer");
    mainAttributes.putValue("Created-by", "Codeprimate Software, LLC");
    mainAttributes.putValue("Main-Class", EchoServer.class.getName());
    mainAttributes.putValue("Class-Path", resolveClassPath(jarFileDirectory));

    return manifest;
  }

  private String resolveClassPath(File jarFileDirectory) {

    String relativeClassPath = getProjectHomeDirectory().equals(jarFileDirectory)
      ? getBuildDirectoryName()
      : "";

    relativeClassPath += relativeClassPath.isEmpty() ? StringUtils.EMPTY_STRING : File.separator;
    relativeClassPath += getClassesDirectoryName();
    relativeClassPath += File.separator;

    return relativeClassPath;
  }

  @AfterEach
  public void tearDown() {

    this.process.stopAndWait();

    assertThat(this.process.isRunning()).isFalse();
  }

  @Test
  public void forkedJarFileProcessIsRunning() {

    EchoClient echoClient = newEchoClient(this.availablePort);

    assertThat(echoClient.sendMessage("Hello")).isEqualTo("Hello");
    assertThat(echoClient.sendMessage("Whatcha doin?")).isEqualTo("Whatcha doin?");
    assertThat(echoClient.sendMessage("Good-bye")).isEqualTo("Good-bye");
  }
}
