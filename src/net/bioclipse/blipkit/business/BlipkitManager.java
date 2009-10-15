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

import java.lang.reflect.Array;

import net.bioclipse.managers.business.IBioclipseManager;
import org.apache.log4j.Logger;
import jpl.*;


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
    	resultString = "";
    	
    	Query query = new Query("consult('/home/samuel/test.pl')");

        if ( !query.hasSolution() ){
        	resultString = resultString + "\nconsult('test.pl') failed";
        } else {
        	resultString = resultString + "\npassed.";
        }
        
    	return "Result: " + resultString;
    }

    public String printLibPath() {
    	return System.getProperty("java.library.path");
    }
    
    public String getActualArgs() {
    	String[] initArgs;
    	if ( JPL.getActualInitArgs() != null ) {
    		initArgs = JPL.getActualInitArgs();
    	} else {
    		initArgs = new String[0];
    	}
    	StringBuffer result = new StringBuffer("");
    	if (initArgs.length > 0) {
    		result.append(initArgs[0]);
    		for (int i=1; i<initArgs.length; i++) {
    			result.append("\n");
    			result.append(initArgs[i]);    			
    		}
    	} else {
    		result.append("Prolog engine not initialized.");
    	}
        return result.toString();      
    }
 
}
