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

import static java.util.Arrays.stream;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSystemException;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Optional;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import org.cp.elements.io.FileUtils;
import org.cp.elements.io.IOUtils;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.SystemException;

/**
 * Abstract utility class used to process {@link File ZIP files}.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.util.zip.ZipEntry
 * @see java.util.zip.ZipFile
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class ZipUtils {

  public static final String ZIP_FILE_EXTENSION = ".zip";

  /**
   * Constructs a new {@link ZipEntry} from the given {@link File}.
   *
   * @param zipDirectory {@link File} referring to the directory being zipped.
   * @param file {@link File} used to construct the new {@link ZipEntry}.
   * @return a new {@link ZipEntry} constructed from the given {@link File}.
   * @throws IllegalArgumentException if {@link File} is {@literal null}.
   * @see #resolveZipEntryName(File, File)
   * @see java.util.zip.ZipEntry
   * @see java.io.File
   */
  protected static ZipEntry newZipEntry(File zipDirectory, File file) {

    Assert.notNull(file, "File is required");

    ZipEntry zipEntry = new ZipEntry(resolveZipEntryName(zipDirectory, file));

    zipEntry.setSize(file.getTotalSpace());
    zipEntry.setTime(file.lastModified());

    return zipEntry;
  }

  /**
   * Tries to resolve the {@link String} of the {@link ZipEntry} given the {@link File directory} being zipped
   * and the {@link File} from when the {@link ZipEntry} is being constructed.
   * <p>
   * The resolved {@link ZipEntry} {@link String name} will be the relative path of the given {@link File}
   * if the {@link File} is relative to the given {@link File directory}, otherwise the {@link ZipEntry}
   * {@link String name} will be the {@link String name} of the given {@link File}.
   * <p>
   * If {@link File diretory} is {@literal null}, or is not a valid {@link File directory},
   * or the {@link File directory} refers to the root of the file system (i.e. the {@link File#getName() name}
   * of the {@link File directory} is {@link String empty}), then the resolved {@link ZipEntry} {@link String name}
   * will be the {@link String name} of the given {@link File}.
   *
   * @param zipDirectory {@link File directory} being zipped.
   * @param file {@link File} used to construct the {@link ZipEntry}; must not be {@literal null}.
   * @return the resolved {@link String name} of the {@link ZipEntry}.
   * @see java.io.File
   */
  static String resolveZipEntryName(File zipDirectory, File file) {

    return Optional.ofNullable(zipDirectory)
      .filter(File::isDirectory)
      .map(File::getName)
      .filter(StringUtils::hasText)
      .map(zipDirectoryName -> {

        String filePath = file.getAbsolutePath();

        int index = filePath.indexOf(zipDirectoryName);

        return index > -1 ? filePath.substring(index) : null;
      })
      .orElseGet(file::getName);
  }

  /**
   * Unzips the given {@link File ZIP file} to the specified {@link File directory}.
   *
   * @param zip {@link File ZIP file} to unzip.
   * @param directory {@link File} referring to the file system path location in which to
   * unzip the {@link File ZIP file}.
   * @throws IllegalArgumentException if {@link File ZIP file} is {@literal null}
   * or the specified {@link File directory} is not a valid directory.
   * @throws IOException if an IO error occurs while reading the {@link File ZIP file}.
   * @throws SystemException if the {@link File ZIP file} could not be read or its contents unzipped.
   * @see java.util.zip.ZipFile
   * @see java.io.File
   */
  public static void unzip(File zip, File directory) throws IOException {

    Assert.notNull(zip, "ZIP file is required");

    Assert.isTrue(FileUtils.createDirectory(directory), String.format("[%s] is not a valid directory", directory));

    try (ZipFile zipFile = new ZipFile(zip, ZipFile.OPEN_READ)) {

      zipFile.stream().forEach(zipEntry -> {

        if (zipEntry.isDirectory()) {
          Assert.state(FileUtils.createDirectory(new File(directory, zipEntry.getName())),
            newSystemException("Failed to create directory [%s] for ZIP entry", zipEntry.getName()));
        }
        else {

          DataInputStream entryInputStream = null;

          DataOutputStream entryOutputStream = null;

          try {

            File zipEntryFile = new File(directory, zipEntry.getName());

            Assert.state(FileUtils.createDirectory(zipEntryFile.getParentFile()),
              newSystemException("Failed to create directory [%1$s] for entry [%2$s]",
                zipEntryFile.getParent(), zipEntry.getName()));

            Assert.state(zipEntryFile.createNewFile(),
              newSystemException("Filed to create file [%1$s] for entry [%2$s]", zipEntryFile, zipEntry.getName()));

            entryInputStream = new DataInputStream(zipFile.getInputStream(zipEntry));
            entryOutputStream = new DataOutputStream(new FileOutputStream(zipEntryFile));

            IOUtils.copy(entryInputStream, entryOutputStream);

          }
          catch (IOException cause) {
            throw newSystemException(cause, "Failed to unzip entry [%s]", zipEntry.getName());
          }
          finally {
            IOUtils.close(entryInputStream);
            IOUtils.close(entryOutputStream);
          }
        }
      });
    }
  }

  /**
   * Zips the contents of the specified {@link File directory}.
   *
   * @param directory {@link File} referring to the file system path/location containing the contents to zip.
   * @return a {@link File ZIP file} containing the compressed contents of the specified {@link File directory}.
   * @throws IllegalArgumentException if {@link File directory} is not a valid directory.
   * @throws IllegalStateException if the {@link File ZIP file } could not be created/initialized.
   * @throws IOException if the contents of the specified {@link File directory} could not be zipped.
   * @throws SystemException if a file system entry could not be added to the {@link File ZIP file}.
   * @see #zip(File, File, ZipOutputStream)
   * @see java.io.File
   */
  public static File zip(File directory) throws IOException {

    Assert.isTrue(FileUtils.isDirectory(directory), "[%s] is not a valid directory", directory);

    File zip = new File(directory.getParent(), directory.getName().concat(ZIP_FILE_EXTENSION));

    Assert.state(zip.createNewFile(), "Failed to create new ZIP file [%s]", zip);

    try (ZipOutputStream outputStream = new ZipOutputStream(new FileOutputStream(zip, false))) {
      zip(directory, directory, outputStream);
      outputStream.finish();
    }

    return zip;
  }

  /**
   * Zips the contents of the specified {@link File directory} to the supplied {@link ZipOutputStream}.
   *
   * @param zipDirectory {@link File directory} being zipped.
   * @param relativeDirectory the current {@link File directory} with contents to zip.
   * @param outputStream {@link ZipOutputStream} used to zip the contents of the relative {@link File directory}.
   * @throws IllegalArgumentException if relative {@link File directory} is not a valid directory.
   * @throws SystemException if a file system entry could not be added to the {@link File ZIP file}.
   * @see #zipEntry(File, File, ZipOutputStream)
   * @see java.util.zip.ZipOutputStream
   * @see java.io.File
   */
  @SuppressWarnings("all")
  private static void zip(File zipDirectory, File relativeDirectory, ZipOutputStream outputStream) {

    Assert.isTrue(FileUtils.isDirectory(relativeDirectory),
      "[%s] is not a valid directory", relativeDirectory);

    stream(relativeDirectory.listFiles()).forEach(file -> {

      if (file.isDirectory()) {
        zip(zipDirectory, file, outputStream);
      }
      else {
        zipEntry(zipDirectory, file, outputStream);
      }
    });
  }

  /**
   * Zips the contents of the individual {@link File file system path} to the supplied {@link ZipOutputStream}.
   *
   * @param zipDirectory {@link File directory} being zipped.
   * @param path {@link File} to zip and add to the supplied {@link ZipOutputStream}.
   * @param outputStream {@link ZipOutputStream} used to zip the contents of the given {@link File path}.
   * @return the given {@link ZipOutputStream}.
   * @throws SystemException if {@link File path} could not be zipped and added to the supplied {@link ZipOutputStream}.
   * @see #zipEntry(ZipEntry, ZipOutputStream)
   * @see java.util.zip.ZipOutputStream
   * @see java.io.File
   */
  static ZipOutputStream zipEntry(File zipDirectory, File path, ZipOutputStream outputStream) {
    return zipEntry(newZipEntry(zipDirectory, path), outputStream);
  }

  /**
   * Zips the contents of the individual {@link ZipEntry} to the supplied {@link ZipOutputStream}.
   *
   * @param entry {@link ZipEntry} to compress and add to the supplied {@link ZipOutputStream}.
   * @param outputStream {@link ZipOutputStream} used to zip the contents of the given {@link ZipEntry}.
   * @return the given {@link ZipOutputStream}.
   * @throws SystemException if {@link ZipEntry} could not be compressed and added to
   * the supplied {@link ZipOutputStream}.
   * @see java.util.zip.ZipOutputStream
   * @see java.util.zip.ZipEntry
   */
  static ZipOutputStream zipEntry(ZipEntry entry, ZipOutputStream outputStream) {

    try {
      outputStream.putNextEntry(entry);
    }
    catch (IOException cause) {
      throw newSystemException(cause, "Failed to zip entry [%s]", entry.getName());
    }

    IOUtils.doSafeIo(outputStream::closeEntry);

    return outputStream;
  }
}
