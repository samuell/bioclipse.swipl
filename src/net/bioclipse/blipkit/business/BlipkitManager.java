/*******************************************************************************
 * Copyright (c) 2009  The Bioclipse team <olas@bioclipse.net>
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contact: http://www.bioclipse.net/
 ******************************************************************************/
package net.bioclipse.blipkit.business;

import net.bioclipse.managers.business.IBioclipseManager;
import org.apache.log4j.Logger;
import jpl.*;
import jpl.Query;


public class BlipkitManager implements IBioclipseManager {

    private static final Logger logger = Logger.getLogger(BlipkitManager.class);

    /**
     * Gives a short one word name of the manager used as variable name when
     * scripting.
     */
    public String getManagerName() {
        return "blipkit";
    }

    public String test() {
    	String resultString;

    	Query query = new Query("consult('/home/samuel/test.pl')");
    	
        if ( !query.hasSolution() ){
            resultString = "consult('test.pl') failed";
        } else {
        	resultString = "passed.";
        }
        
    	return "Result: " + resultString;
    }

    public String printLibPath() {
    	return System.getProperty("java.library.path");
    }
 
}
