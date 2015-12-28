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

package org.cp.elements.io.support;

import org.cp.elements.io.FileExtensionFilter;

/**
 * The ArchivingAndCompressionFileExtensionFilter class is a FileExtensionFilter implementation...
 *
 * @author John J. Blum
 * @see org.cp.elements.io.FileExtensionFilter
 * @link http://en.wikipedia.org/wiki/List_of_archive_formats
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ArchiveAndCompressionFileExtensionFilter extends FileExtensionFilter {

  protected static final String[] ARCHIVE_AND_COMPRESSION_FILE_EXTENSIONS = {
    "7z",
    "apk",
    "arc",
    "arj",
    "b1",
    "ba",
    "cab",
    "cfs",
    "cpt",
    "dar",
    "dgc",
    "dmg",
    "ear",
    "jar",
    "kgb",
    "pak",
    "partimg",
    "pea",
    "pkg",
    "rar",
    "rpm",
    "tar.bz2",
    "tar.gz",
    "tar.lzma",
    "tar.z",
    "tbz2",
    "tgz",
    "tlz",
    "war",
    "wim",
    "zip",
    "zipx"
  };

  public ArchiveAndCompressionFileExtensionFilter() {
    super(ARCHIVE_AND_COMPRESSION_FILE_EXTENSIONS);
  }

}
