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

package org.cp.elements.tools.io;

import static java.util.Arrays.stream;
import static org.cp.elements.lang.RuntimeExceptionsFactory.newIllegalArgumentException;
import static org.cp.elements.util.ArrayUtils.nullSafeArray;

import java.io.File;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Optional;

import org.cp.elements.io.FileSystemUtils;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link ListFiles} class is a Codeprimate Elements Tool used to list the contents of a directory
 * in a hierarchical form in a TUI, such as a CLI.
 *
 * @author John Blum
 * @see java.io.File
 * @see java.lang.Runnable
 * @see org.cp.elements.io.FileSystemUtils
 * @see org.cp.elements.lang.StringUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ListFiles implements Runnable {

  protected static final File DEFAULT_DIRECTORY = FileSystemUtils.WORKING_DIRECTORY;

  protected static final String DIRECTORY_MARKER = "+++";
  protected static final String DIRECTORY_MARKER_WITH_DIRECTORY_NAME = DIRECTORY_MARKER + " %s";
  protected static final String DIRECTORY_SWIM_LANE = "|";
  protected static final String FILE_MARKER = "---";
  protected static final String FILE_MARKER_WITH_FILENAME = FILE_MARKER + " %s";
  protected static final String SUB_DIRECTORY_DASH_PLUS_OFFSET = "- ";
  protected static final String SUB_DIRECTORY_OFFSET = "  ";

  /**
   * Executes the {@link ListFiles} program.
   *
   * @param args array of {@link String} arguments passed to the {@link ListFiles} program from the command-line.
   * @see org.cp.elements.tools.io.ListFiles#run()
   * @see #newListFiles(File)
   */
  public static void main(String[] args) {
    newListFiles(resolveDirectory(args)).run();
  }

  /* (non-Javadoc) */
  static String resolveArgument(String[] args) {
    return Optional.ofNullable(args).map(ArrayUtils::getFirst).filter(StringUtils::hasText)
      .orElseThrow(() -> newIllegalArgumentException("Directory is required"));
  }

  /* (non-Javadoc) */
  static File resolveDirectory(String[] args) {
    return Optional.of(resolveArgument(args)).map(File::new).filter(File::isDirectory)
      .orElseThrow(() -> newIllegalArgumentException("Argument [%s] is not a valid directory", args[0]));
  }

  /**
   * Factory method used to construct a new instance of {@link ListFiles} that will list the contents
   * of the {@link FileSystemUtils#WORKING_DIRECTORY current working directory}.
   *
   * @return a new instance of {@link ListFiles} listing the contents
   * of the {@link FileSystemUtils#WORKING_DIRECTORY current working directory}.
   * @see org.cp.elements.io.FileSystemUtils#WORKING_DIRECTORY
   * @see #newListFiles(File)
   * @see java.io.File
   */
  public static ListFiles newListFiles() {
    return newListFiles(DEFAULT_DIRECTORY);
  }

  /**
   * Factory method used to construct a new instance of {@link ListFiles} with the given {@link File directory}
   * for which the contents will be listed.
   *
   * @param directory {@link File} referring to the directory for which the contents will be listed.
   * @return a new instance of {@link ListFiles} initialized with the given {@link File directory}.
   * @throws IllegalArgumentException if the given {@link File} is not a valid directory.
   * @see #ListFiles(File)
   * @see java.io.File
   */
  public static ListFiles newListFiles(File directory) {
    return new ListFiles(directory);
  }

  private final File directory;

  /**
   * Constructs a new instance of {@link ListFiles} initialized with the given {@link File directory}
   * for which the contents will be listed.
   *
   * @param directory {@link File} referring to the directory for which the contents will be listed.
   * @throws IllegalArgumentException if the given {@link File} is not a valid directory.
   * @see #validateDirectory(File)
   * @see java.io.File
   */
  public ListFiles(File directory) {
    this.directory = validateDirectory(directory);
  }

  /**
   * Returns a reference to the {@link File directory} for which the contents will be listed.
   *
   * @return a reference to the {@link File directory} for which the contents will be listed.
   * @see java.io.File
   */
  protected File getDirectory() {
    return Optional.ofNullable(this.directory).orElseGet(() -> DEFAULT_DIRECTORY);
  }

  /**
   * Runs the {@link File directory} content listing.
   *
   * This program and list files algorithm operates recursively in depth-first order, listing the contents or all
   * sub-directories and files in a hierarchical view.
   *
   * @see java.lang.Runnable#run()
   * @see #listFiles(File, String)
   * @see #printHeader(File)
   * @see #getDirectory()
   */
  public void run() {
    listFiles(printHeader(getDirectory()), StringUtils.EMPTY_STRING);
  }

  /**
   * Lists the contents of the given {@link File directory} displayed from the given {@link String indent}.
   *
   * @param directory {@link File} referring to the directory for which the contents will be listed.
   * @param indent {@link String} containing the characters of the indent in which to begin the view
   * of the directory hierarchy/tree.
   * @throws IllegalArgumentException if the given {@link File} is not a valid directory.
   * @see #validateDirectory(File)
   * @see java.io.File
   */
  public void listFiles(File directory, String indent) {

    directory = validateDirectory(directory);
    indent = Optional.ofNullable(indent).filter(StringUtils::hasText).orElse(StringUtils.EMPTY_STRING);

    printDirectoryName(indent, directory);

    String directoryContentIndent = buildDirectoryContentIndent(indent);

    stream(sort(nullSafeArray(directory.listFiles(), File.class))).forEach(file -> {
      if (FileSystemUtils.isDirectory(file)) {
        listFiles(file, directoryContentIndent);
      }
      else {
        printFileName(directoryContentIndent, file);
      }
    });
  }

  /* (non-Javadoc) */
  String buildDirectoryContentIndent(String indent) {

    return Optional.ofNullable(indent).filter(StringUtils::hasText)
      .map(it -> it + StringUtils.SINGLE_SPACE + SUB_DIRECTORY_OFFSET + DIRECTORY_SWIM_LANE)
      .orElseGet(() ->StringUtils.SINGLE_SPACE + DIRECTORY_SWIM_LANE);
  }

  /* (non-Javadoc) */
  String concatIndentAndDirectoryName(String indent, File directory) {

    indent = Optional.ofNullable(indent).filter(StringUtils::hasText)
      .map(it -> it + SUB_DIRECTORY_DASH_PLUS_OFFSET)
      .orElse(StringUtils.EMPTY_STRING);

    return String.format(indent + DIRECTORY_MARKER_WITH_DIRECTORY_NAME, directory.getName());
  }

  /* (non-Javadoc) */
  String concatIndentAndFileName(String indent, File file) {
    return String.format(indent + SUB_DIRECTORY_OFFSET + FILE_MARKER_WITH_FILENAME, file.getName());
  }

  /* (non-Javadoc) */
  private File printDirectoryName(String indent, File directory) {

    System.out.printf(String.format("%s%n", concatIndentAndDirectoryName(indent, directory)));

    return directory;
  }

  /* (non-Javadoc) */
  private File printFileName(String indent, File file) {

    System.out.printf(String.format("%s%n", concatIndentAndFileName(indent, file)));

    return file;
  }

  /* (non-Javadoc) */
  File printHeader(File directory) {

    System.out.printf("Listing contents for directory [%s]...%n%n", directory.getAbsolutePath());

    return directory;
  }

  /**
   * Sorts the given array of {@link File Files}, ordered by directory first, files second and name last.
   *
   * @param files array of {@link File Files} to sort; array must not be {@literal null}
   * or contain {@literal null} {@link File Files}.
   * @return the sorted array of {@link File Files}.
   * @see java.util.Arrays#sort(Object[], Comparator)
   * @see java.io.File
   */
  protected File[] sort(File[] files) {

    Arrays.sort(files, (fileOne, fileTwo) -> {

      if (fileOne.isDirectory() && fileTwo.isDirectory()) {
        return fileOne.getName().compareTo(fileTwo.getName());
      }
      else if (fileOne.isDirectory()) {
        return -1;
      }
      else if (fileTwo.isDirectory()) {
        return 1;
      }
      else {
        return fileOne.getName().compareTo(fileTwo.getName());
      }
    });

    return files;
  }

  /**
   * Validates the given {@link File} is a valid directory.
   *
   * @param directory {@link File} to evaluate.
   * @return the given {@link File}.
   * @throws IllegalArgumentException if the given {@link File} is not a valid directory.
   * @see java.io.File#isDirectory()
   */
  @NullSafe
  private File validateDirectory(File directory) {
    return Optional.ofNullable(directory).filter(File::isDirectory)
      .orElseThrow(() -> newIllegalArgumentException("File [%s] is not a valid directory", directory));
  }
}
