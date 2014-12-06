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

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.RelationalOperator;

/**
 * The FileSizeFilter class is a FileFilter implementation that filters files based on their size in bytes.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.RelationalOperator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileSizeFilter implements FileFilter, Filter<File> {

  /**
   * Creates an instance of the FileSizeFilter class initialized with the RelationOperator used in the evaluation
   * of the File's size in bytes during he filtering operation.
   *
   * @param operator the RelationOperator used to evaluate the File.
   * @return an instance of this FileSizeFilter initialized with an instance of the RelationOperator used as criteria
   * for evaluating the File.
   * @see org.cp.elements.lang.RelationalOperator#evaluate(Comparable)
   */
  protected static FileSizeFilter create(final RelationalOperator<Long> operator) {
    return new FileSizeFilter() {
      @Override public boolean accept(final File file) {
        return operator.evaluate(file.length());
      }
    };
  }

  /**
   * Determines whether the given File matches the criteria of and is accepted by this FileFilter.  The File is a match
   * if the File's length property (size) matches the expected size criteria of this FileFilter.
   *
   * @param file the File to filter.
   * @return a boolean value indicating whether the File's size property satisfies the criteria of this FileFilter.
   * @see java.io.FileFilter#accept(java.io.File)
   * @see java.io.File
   */
  public abstract boolean accept(File file);

  /**
   * Creates an instance of the FileSizeFilter that evaluates a File's size in bytes based on a size range, between
   * the given minimum and maximum size in bytes, inclusive.
   *
   * @param minSize a long value indicating the minimum file size in bytes.
   * @param maxSize a long value indicating the maximum file size in bytes.
   * @return an instance of the FileSizeFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#greaterThanEqualToAndLessThanEqualTo(Comparable, Comparable)
   */
  public static FileSizeFilter between(final long minSize, final long maxSize) {
    return create(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(minSize, maxSize));
  }

  /**
   * Creates an instance of the FileSizeFilter evaluating a File's size in bytes based on it's equality to
   * the given size in bytes.
   *
   * @param size a long vlaue indicating the exact file size in bytes.
   * @return an instance of the FileSizeFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#equalTo(Comparable)
   */
  public static FileSizeFilter equalTo(final long size) {
    return create(RelationalOperator.equalTo(size));
  }

  /**
   * Creates an instance of the FileSizeFilter evaluating a File's size in bytes based on it's relation (greater than)
   * to the given size in bytes.
   *
   * @param size a long value indicating the lower bound file size in bytes.
   * @return an instance of the FileSizeFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#greaterThan(Comparable)
   */
  public static FileSizeFilter greaterThan(final long size) {
    return create(RelationalOperator.greaterThan(size));
  }

  /**
   * Creates an instance of the FileSizeFilter evaluating a File's size in bytes based on it's relation (less than)
   * to the given size in bytes.
   *
   * @param size a long value indicating the upper bound file size in bytes.
   * @return an instance of the FileSizeFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#lessThan(Comparable)
   */
  public static FileSizeFilter lessThan(final long size) {
    return create(RelationalOperator.lessThan(size));
  }

}
