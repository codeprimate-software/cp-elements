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
import static org.cp.elements.process.ProcessAdapter.newProcessAdapter;
import static org.cp.elements.process.java.JavaProcessExecutor.newJavaProcessExecutor;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.io.IOException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.process.ProcessAdapter;
import org.mockito.Mock;
import org.mockito.Mock.Strictness;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link JavaProcessExecutor}.
 *
 * @author John Blum
 * @see java.io.File
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.cp.elements.process.java.JavaProcessExecutor
 * @since 1.0.0
 */
@ExtendWith(MockitoExtension.class)
public class JavaProcessExecutorTests {

  @Mock(strictness = Strictness.LENIENT)
  private File mockFile;

  @Mock(strictness = Strictness.LENIENT)
  private Process mockProcess;

  private ProcessAdapter processAdapter;

  @BeforeEach
  @SuppressWarnings("all")
  public void setup() throws IOException {
    processAdapter = newProcessAdapter(mockProcess);
    doReturn("/path/to/file.jar").when(mockFile).getCanonicalPath();
  }

  @Test
  public void newJavaProcessExecutorIsNotNull() {
    assertThat(newJavaProcessExecutor()).isNotNull();
  }

  @Test
  public void executeWithClassAndArgumentsCallsExecuteWithDirectoryClassAndArguments() {

    JavaProcessExecutor processExecutor = spy(newJavaProcessExecutor());

    doReturn(processAdapter).when(processExecutor).execute(any(File.class), any(Class.class), any(String[].class));

    assertThat(processExecutor.execute(TestApplication.class, "argOne", "argTwo")).isEqualTo(processAdapter);

    verify(processExecutor, times(1)).execute(eq(FileSystemUtils.WORKING_DIRECTORY),
      eq(TestApplication.class), eq("argOne"), eq("argTwo"));
  }

  @Test
  public void executeWithJarFileAndArgumentsCallsExecuteWithDirectoryJarFileAndArguments() {

    JavaProcessExecutor processExecutor = spy(newJavaProcessExecutor());

    doReturn(processAdapter).when(processExecutor).execute(any(File.class), any(File.class), any(String[].class));

    assertThat(processExecutor.execute(mockFile, "argOne", "argTwo")).isEqualTo(processAdapter);

    verify(processExecutor, times(1)).execute(eq(FileSystemUtils.WORKING_DIRECTORY),
      eq(mockFile), eq("argOne"), eq("argTwo"));
  }

  @Test
  public void toJavaCommandLineWithClass() {

    String[] javaCommandLine = newJavaProcessExecutor().toJavaCommandLine(TestApplication.class);

    assertThat(javaCommandLine).isNotNull();
    assertThat(javaCommandLine).isNotEmpty();
    assertThat(javaCommandLine).contains(FileSystemUtils.JAVA_EXE.getAbsolutePath(), TestApplication.class.getName());
  }

  @Test
  public void toJavaCommandLineWithClassAndArguments() {

    String[] javaCommandLine = newJavaProcessExecutor().toJavaCommandLine(TestApplication.class,
      "argOne", "argTwo");

    assertThat(javaCommandLine).isNotNull();
    assertThat(javaCommandLine).isNotEmpty();
    assertThat(javaCommandLine).contains(FileSystemUtils.JAVA_EXE.getAbsolutePath(), "-server", "-classpath",
      System.getProperty("java.class.path"), TestApplication.class.getName(), "argOne", "argTwo");
  }

  @Test
  public void toJavaCommandLineWithJarFile() {

    String[] javaCommandLine = newJavaProcessExecutor().toJavaCommandLine(mockFile);

    assertThat(javaCommandLine).isNotNull();
    assertThat(javaCommandLine).isNotEmpty();
    assertThat(javaCommandLine).contains(FileSystemUtils.JAVA_EXE.getAbsolutePath(), "-jar", "/path/to/file.jar");
  }

  @Test
  public void toJavaCommandLineWithJarFileAndArguments() {

    String[] javaCommandLine = newJavaProcessExecutor().toJavaCommandLine(mockFile, "argOne", "argTwo");

    assertThat(javaCommandLine).isNotNull();
    assertThat(javaCommandLine).isNotEmpty();
    assertThat(javaCommandLine).contains(FileSystemUtils.JAVA_EXE.getAbsolutePath(), "-jar", "/path/to/file.jar",
      "argOne", "argTwo");
  }

  private static class TestApplication { }

}
