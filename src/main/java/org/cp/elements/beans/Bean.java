/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * 
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * 
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * 
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * 
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.beans;

import java.io.Serializable;

import org.cp.elements.lang.Auditable;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.Visitable;

/**
 * The Bean interface defines a contract for a domain model object representing application data in a
 * software application.
 * 
 * @author John J. Blum
 * @param <ID> the Comparable class type of the identifier uniquely identifying this Bean.
 * @param <USER> the class type of the object signifying the user for auditing information.
 * @param <PROCESS> the class type of the object signifying the process for auditing information.
 * @see java.lang.Cloneable
 * @see java.lang.Comparable
 * @see java.io.Serializable
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Identifiable
 * @see org.cp.elements.lang.Visitable
 * @since 1.0.0
 * @version 1.0.0
 */
public interface Bean<ID extends Comparable<ID>, USER, PROCESS> extends Auditable<USER, PROCESS>, Cloneable, Comparable<Bean>, Identifiable<ID>, Serializable, Visitable {

}
