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
package org.cp.elements.util.zip;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.test.AbstractBaseTestSuite;
import org.cp.elements.test.annotation.IntegrationTest;
import org.junit.Test;

/**
 * Integration Tests for {@link ZipUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.util.zip.ZipUtils
 * @since 1.0.0
 */
@IntegrationTest
public class ZipUtilsIntegrationTests extends AbstractBaseTestSuite {

  @Test
  public void zipThenUnzipSrcMainAsciidocDirectory() throws IOException {

    File asciidocZip = null;
    File sourceDirectory = new File(getSourceDirectory(), "main/asciidoc");
    File targetDirectory = FileSystemUtils.TEMPORARY_DIRECTORY;
    File unzippedAsciidocDirectory = new File(targetDirectory, "asciidoc");

    try {

      assertThat(sourceDirectory).isDirectory();

      asciidocZip = ZipUtils.zip(sourceDirectory);

      assertThat(asciidocZip).isFile();
      assertThat(asciidocZip.length()).isGreaterThan(0L);

      asciidocZip.deleteOnExit();

      ZipUtils.unzip(asciidocZip, targetDirectory);

      assertThat(unzippedAsciidocDirectory).isDirectory();
      assertThat(new File(unzippedAsciidocDirectory, "index.adoc")).isFile();
      assertThat(new File(unzippedAsciidocDirectory, "preface.adoc")).isFile();

      File introductionDirectory = new File(unzippedAsciidocDirectory, "introduction");

      assertThat(introductionDirectory).isDirectory();
      assertThat(new File(introductionDirectory, "introduction.adoc")).isFile();
      assertThat(new File(introductionDirectory, "requirements.adoc")).isFile();
    }
    finally {
      FileSystemUtils.delete(asciidocZip);
      FileSystemUtils.deleteRecursive(unzippedAsciidocDirectory);
    }
  }
}
