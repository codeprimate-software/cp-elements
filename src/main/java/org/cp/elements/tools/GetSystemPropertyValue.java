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

package org.cp.elements.tools;

/**
 * The GetSystemPropertyValue class is a command-line utility class for getting the value of a Java System Property.
 *
 * @author John J. Blum
 * @see java.lang.System#getProperty(String)
 * @since 1.0.0
 */
public class GetSystemPropertyValue {

  public static void main(final String[] args) {
    if (args.length < 1) {
      System.err.printf(">java %1$s systemProperty [systemProperty]*%n", GetSystemPropertyValue.class.getName());
      System.exit(1);
    }

    for (String arg : args) {
      System.out.printf("%1$s = %2$s%n", arg, System.getProperty(arg));
    }
  }

}
