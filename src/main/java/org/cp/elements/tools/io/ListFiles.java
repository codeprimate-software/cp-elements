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
import java.util.Optional;

import org.cp.elements.lang.StringUtils;
import org.cp.elements.util.ArrayUtils;

/**
 * The {@link ListFiles} class is a Codeprimate Elements Tool used to list the contents of a directory
 * in a hierarchical form in a TUI, such as a CLI.
 *
 * @author John Blum
 * @see java.io.File
 * @see org.cp.elements.lang.StringUtils
 * @since 1.0.0
 */
public class ListFiles {

  protected static final String DIRECTORY_MARKER = "+++";
  protected static final String DIRECTORY_MARKER_WITH_DIRECTORY_NAME = DIRECTORY_MARKER + " %s";
  protected static final String DIRECTORY_SWIM_LANE = "|";
  protected static final String FILE_MARKER = "---";
  protected static final String FILE_MARKER_WITH_FILENAME = FILE_MARKER + " %s";
  protected static final String SUB_DIRECTORY_DASH_PLUS_OFFSET = "- ";
  protected static final String SUB_DIRECTORY_OFFSET = "  ";

  public static void main(String[] args) {
    listFiles(resolveDirectory(args));
  }

  /* (non-Javadoc) */
  private static String resolveArgument(String[] args) {
    return Optional.ofNullable(args).map(ArrayUtils::getFirst).filter(StringUtils::hasText)
      .orElseThrow(() -> newIllegalArgumentException("Directory is required"));
  }

  /* (non-Javadoc) */
  private static File resolveDirectory(String[] args) {
    return Optional.of(resolveArgument(args)).map(File::new).filter(File::isDirectory)
      .orElseThrow(() -> newIllegalArgumentException("Argument [%s] is not a valid directory", args[0]));
  }

  /* (non-Javadoc) */
  protected static void listFiles(File directory) {

    System.out.printf("Listing contents for directory [%s]...%n%n", directory.getAbsolutePath());

    listFiles(directory, "");
  }

  /* (non-Javadoc) */
  private static void listFiles(File directory, String indent) {

    printDirectoryName(indent, directory);

    String directoryContentIndent = buildDirectoryContentIndent(indent);

    stream(sort(nullSafeArray(directory.listFiles(), File.class))).forEach(file -> {
      if (file.isDirectory()) {
        listFiles(file, directoryContentIndent);
      }
      else {
        printFileName(directoryContentIndent, file);
      }
    });
  }

  /* (non-Javadoc) */
  private static String buildDirectoryContentIndent(String indent) {

    return Optional.ofNullable(indent).filter(StringUtils::hasText)
      .map(it -> it + StringUtils.SINGLE_SPACE + SUB_DIRECTORY_OFFSET + DIRECTORY_SWIM_LANE)
      .orElseGet(() ->StringUtils.SINGLE_SPACE + DIRECTORY_SWIM_LANE);
  }

  /* (non-Javadoc) */
  private static String concatIndentAndDirectoryName(String indent, File directory) {

    indent = Optional.ofNullable(indent).filter(StringUtils::hasText)
      .map(it -> it + SUB_DIRECTORY_DASH_PLUS_OFFSET)
      .orElse(StringUtils.EMPTY_STRING);

    return String.format(indent + DIRECTORY_MARKER_WITH_DIRECTORY_NAME, directory.getName());
  }

  /* (non-Javadoc) */
  private static String concatIndentAndFileName(String indent, File file) {
    return String.format(indent + SUB_DIRECTORY_OFFSET + FILE_MARKER_WITH_FILENAME, file.getName());
  }

  /* (non-Javadoc) */
  private static void printDirectoryName(String indent, File directory) {
    System.out.printf(String.format("%s%n", concatIndentAndDirectoryName(indent, directory)));
  }

  /* (non-Javadoc) */
  private static void printFileName(String indent, File file) {
    System.out.printf(String.format("%s%n", concatIndentAndFileName(indent, file)));
  }

  /* (non-Javadoc) */
  private static File[] sort(File[] files) {

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
}
