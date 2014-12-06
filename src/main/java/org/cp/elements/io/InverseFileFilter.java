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

package org.cp.elements.io;

import java.io.File;
import java.io.FileFilter;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;

/**
 * The InverseFileFilter class is a FileFilter implementation that returns the inverse result
 * of the delegating FileFilter from which the InverseFileFilter is composed.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class InverseFileFilter implements FileFilter, Filter<File> {

  private final FileFilter delegate;

  /**
   * Constructs an instance of the InverseFileFilter class initialized with the required, delegating FileFilter.
   *
   * @param delegate the FileFilter used as the delegate for the actual file filtering operation.
   * @throws java.lang.NullPointerException if the delegating FileFilter reference is null.
   * @see java.io.FileFilter
   */
  public InverseFileFilter(final FileFilter delegate) {
    Assert.notNull(delegate, "The delegating FileFilter must not be null!");
    this.delegate = delegate;
  }

  /**
   * Gets the FileFilter used as the delegate to the InverseFilterFilter.
   *
   * @return the backing, delegating FileFilter instance.
   * @see java.io.File
   */
  protected FileFilter getDelegate() {
    return delegate;
  }

  /**
   * Determines whether the specified File matches the criteria of this FileFilter.
   *
   * @param file the File to filter.
   * @return a boolean value indicating whether the specified File matches the criteria of this FileFilter.
   * @see #getDelegate()
   * @see java.io.File
   */
  @Override
  public boolean accept(final File file) {
    return !getDelegate().accept(file);
  }

}
