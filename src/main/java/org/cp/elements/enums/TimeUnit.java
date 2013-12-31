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

package org.cp.elements.enums;

/**
 * The TimeUnit enum defines constants for units of time.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum TimeUnit {
  NANOSECOND("ns", "Nanosecond", "1 billionth of a second"),
  MICROSECOND("us", "Microsecond", "1 millionth of a second"),
  MILLISECOND("ms", "Millisecond", "1 thousandth of a second"),
  SECOND("s", "Second", "1 second"),
  MINUTE("mi", "Minute", "60 seconds"),
  HOUR("hr", "Hour", "60 minutes"),
  DAY("day", "Day", "24 hours"),
  WEEK("wk", "Week", "7 days"),
  MONTH("mon", "Month", "28-31 days"),
  YEAR("yr", "Year", "12 months, 365 days"),
  DECADE("dec", "Decade", "10 years"),
  SCORE("Score", "Score", "20 years"),
  CENTURY("cent", "Century", "100 years"),
  MILLENIA("Millenia", "Millenia", "1000 years");

  private final String abbreviation;
  private final String description;
  private final String name;

  TimeUnit(final String abbreviation, final String name, final String description) {
    this.abbreviation = abbreviation;
    this.name = name;
    this.description = description;
  }

  public static TimeUnit valueOfAbbreviation(final String abbreviation) {
    for (final TimeUnit unit : values()) {
      if (unit.getAbbreviation().equalsIgnoreCase(abbreviation)) {
        return unit;
      }
    }

    return null;
  }

  public static TimeUnit valueOfName(final String name) {
    for (final TimeUnit unit : values()) {
      if (unit.getName().equalsIgnoreCase(name)) {
        return unit;
      }
    }

    return null;
  }

  public String getAbbreviation() {
    return abbreviation;
  }

  public String getDescription() {
    return description;
  }

  public String getName() {
    return name;
  }

  @Override
  public String toString() {
    return this.name;
  }

}
