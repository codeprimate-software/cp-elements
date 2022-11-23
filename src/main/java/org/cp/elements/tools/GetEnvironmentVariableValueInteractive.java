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
package org.cp.elements.tools;

import java.io.IOException;
import java.util.Scanner;

import org.cp.elements.ElementsVersion;

/**
 * Elements tool used to interactively query the values of {@link System#getenv(String) System environment variables}.
 *
 * @author John Blum
 * @see java.lang.System#getenv()
 * @see java.lang.System#out
 * @see java.util.Scanner
 * @since 1.0.0
 */
public class GetEnvironmentVariableValueInteractive implements Runnable {

  private static final int EXIT_OPTION = 3;
  private static final int GET_ENVIRONMENT_VARIABLE_OPTION = 1;
  private static final int SET_ENVIRONMENT_VARIABLE_OPTION = 2;

  public static void main(String[] args) {

    log("%nWelcome to the Codeprimate Elements Interactive Environment Variable Tool%n");
    new ElementsVersion().run();
    new GetEnvironmentVariableValueInteractive().run();
  }

  private static void log(String message, Object... arguments) {
    System.out.printf(message, arguments);
    System.out.flush();
  }

  private boolean isExit(int userSelection) {
    return userSelection == EXIT_OPTION;
  }

  private boolean isGetEnvironmentVariable(int userSelection) {
    return userSelection == GET_ENVIRONMENT_VARIABLE_OPTION;
  }

  private boolean isSetEnvironmentVariable(int userSelection) {
    return userSelection == SET_ENVIRONMENT_VARIABLE_OPTION;
  }

  private void printMenu() {

    log("1. Get value of environment variable%n");
    log("2. Set value of environment variable%n");
    log("3. Exit%n");
    log("Please select an option: ");
  }

  @Override
  public void run() {

    Scanner userInputReader = new Scanner(System.in);

    int userSelection;

    do {
      printMenu();
      userSelection = userInputReader.nextInt();

      if (isGetEnvironmentVariable(userSelection)) {

        log("Enter the name of the environment variable: ");
        String environmentVariableName = userInputReader.next();

        log("Environment Variable ['%s'] = [%s]%n%n",
          environmentVariableName, System.getenv(environmentVariableName));
      }
      else if (isSetEnvironmentVariable(userSelection)) {

        log("Enter the name of the environment variable: ");
        String environmentVariableName = userInputReader.next();

        log("Enter the value of the environment variable ['%s']: ", environmentVariableName);
        String environmentVariableValue = userInputReader.next();

        try {
          Runtime.getRuntime().exec(String.format("bash export %s=%s",
            environmentVariableName, environmentVariableValue));
        }
        catch (IOException cause) {
          System.err.printf("Failed to set environment variable ['%s'] to value ['%s']; %s",
            environmentVariableName, environmentVariableValue, cause.getMessage());
        }
      }
    }
    while (!isExit(userSelection));
  }
}
