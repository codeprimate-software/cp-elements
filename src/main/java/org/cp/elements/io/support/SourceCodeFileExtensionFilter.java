/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.io.support;

import org.cp.elements.io.FileExtensionFilter;

/**
 * The SourceCodeFileExtensionFilter is a FileExtensionFilter implementation...
 *
 * @author John J. Blum
 * @see org.cp.elements.io.FileExtensionFilter
 * @link http://www.file-extensions.org/filetype/extension/name/source-code-and-script-files
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class SourceCodeFileExtensionFilter extends FileExtensionFilter {

  protected static final String[] SOURCE_CODE_FILE_EXTENSIONS = {
    "ada",
    "bin",
    "c",
    "cpp",
    "csharp",
    "ddl",
    "dtd",
    "htm",
    "html",
    "html5",
    "java",
    "js",
    "json",
    "jsp",
    "perl",
    "phl",
    "php2",
    "rb",
    "rpy",
    "sql",
    "tcl",
    "vb",
    "vbscript",
    "xml",
    "xsd",
  };

  public SourceCodeFileExtensionFilter() {
    super(SOURCE_CODE_FILE_EXTENSIONS);
  }

}
