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
import java.util.Calendar;

import org.cp.elements.lang.Filter;
import org.cp.elements.lang.RelationalOperator;

/**
 * The FileLastModifiedFilter class is a FileFilter implementation that filters files based on their last modified
 * timestamp (date/time).
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.io.FileFilter
 * @see java.util.Calendar
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.RelationalOperator
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class FileLastModifiedFilter implements FileFilter, Filter<File> {

  /**
   * Creates an instance of the FileLastModifiedFilter class initialized with the RelationOperator used
   * in the evaluation of the File's last modified timestamp during the filtering operation.
   *
   * @param operator the RelationOperator used to evaluate the File.
   * @return an instance of this FileLastModifiedFilter initialized with an instance of the RelationOperator
   * used as criteria for evaluating the File.
   * @see org.cp.elements.lang.RelationalOperator#evaluate(Comparable)
   */
  protected static FileLastModifiedFilter create(final RelationalOperator<Long> operator) {
    return new FileLastModifiedFilter() {
      @Override public boolean accept(final File file) {
        return operator.evaluate(file.lastModified());
      }
    };
  }

  /**
   * Determines whether the given File matches the criteria of and is accepted by this FileFilter.  The File is a match
   * if the File's lastModified property matches the expected date/time criteria of this FileFilter.
   *
   * @param file the File to filter.
   * @return a boolean value indicating whether the File's lastModified property satisfies the criteria
   * of this FileFilter.
   * @see java.io.FileFilter#accept(java.io.File)
   * @see java.io.File
   */
  public abstract boolean accept(File file);

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified after the given timestamp.
   *
   * @param lastModified a long value indicating the last modified timestamp measured in milliseconds after which
   * the file must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#greaterThan(Comparable)
   */
  public static FileLastModifiedFilter after(final long lastModified) {
    return create(RelationalOperator.greaterThan(lastModified));
  }

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified after the given date/time, expressed a java.util.Calendar value.
   *
   * @param dateTime a Calendar value indicating the last modified date/time after which the file
   * must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #after(long)
   * @see java.util.Calendar#getTimeInMillis()
   */
  public static FileLastModifiedFilter after(final Calendar dateTime) {
    return after(dateTime.getTimeInMillis());
  }

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified before the given timestamp.
   *
   * @param lastModified a long value indicating the last modified timestamp measured in milliseconds before which
   * the file must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#lessThan(Comparable)
   */
  public static FileLastModifiedFilter before(final long lastModified) {
    return create(RelationalOperator.lessThan(lastModified));
  }

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified before the given date/time, expressed a java.util.Calendar value.
   *
   * @param dateTime a Calendar value indicating the last modified date/time before which the file
   * must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #before(long)
   * @see java.util.Calendar#getTimeInMillis()
   */
  public static FileLastModifiedFilter before(final Calendar dateTime) {
    return before(dateTime.getTimeInMillis());
  }

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified between a given time span represented as a timestamp range.
   *
   * @param lastModifiedStart a long value indicating the last modified timestamp measured in milliseconds after which
   * the file must have been modified.
   * @param lastModifiedEnd a long value indicating the last modified timestamp measured in milliseconds before which
   * the file must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#greaterThanEqualToAndLessThanEqualTo(Comparable, Comparable)
   */
  public static FileLastModifiedFilter during(final long lastModifiedStart, final long lastModifiedEnd) {
    return create(RelationalOperator.greaterThanEqualToAndLessThanEqualTo(lastModifiedStart, lastModifiedEnd));
  }

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified between a given time span represented as a date/time range.
   *
   * @param startDateTime a Calendar value indicating the last modified date/time after which the file
   * must have been modified.
   * @param endDateTime a Calendar value indicating the last modified date/time before which the file
   * must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #during(long, long)
   * @see java.util.Calendar#getTimeInMillis()
   */
  public static FileLastModifiedFilter during(final Calendar startDateTime, final Calendar endDateTime) {
    return during(startDateTime.getTimeInMillis(), endDateTime.getTimeInMillis());
  }

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified on a given timestamp.
   *
   * @param lastModified a long value indicating the last modified timestamp measured in milliseconds on which
   * the file must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #create(org.cp.elements.lang.RelationalOperator)
   * @see org.cp.elements.lang.RelationalOperator#equalTo(Comparable)
   */
  public static FileLastModifiedFilter on(final long lastModified) {
    return create(RelationalOperator.equalTo(lastModified));
  }

  /**
   * Creates an instance of the FileLastModifiedFilter to evaluate and filter Files based on whether they were
   * last modified on the given date/time, expressed a java.util.Calendar value.
   *
   * @param dateTime a Calendar value indicating the last modified date/time on which the file
   * must have been modified.
   * @return an instance of the FileLastModifiedFilter.
   * @see #on(long)
   * @see java.util.Calendar#getTimeInMillis()
   */
  public static FileLastModifiedFilter on(final Calendar dateTime) {
    return on(dateTime.getTimeInMillis());
  }

}
