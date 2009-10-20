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

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import jpl.*;
import net.bioclipse.managers.business.IBioclipseManager;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IFile;


public class BlipkitManager implements IBioclipseManager {

    private static final Logger logger = Logger.getLogger(BlipkitManager.class);

    /**
     * Gives a short one word name of the manager used as variable name when
     * scripting.
     */
    public String getManagerName() {
        return "blipkit";
    }
    
    public String init() {
    	String result ="";
    	String[] blipInitFile;

    	blipInitFile = readFileToStringArray("/home/samuel/blipstart");
    	for (int i=1; i<blipInitFile.length; i++) {
    		if (blipInitFile[i] != null) {
        		System.out.println(blipInitFile[i]);
    		}
    	}
    	
		 JPL.setDefaultInitArgs(blipInitFile);
		 result = Boolean.toString(JPL.init());
   	
    	return result;
    }
    
    public String query2(String predicate, String secondarg) {
    	String resultString ="";
    	Variable X = new Variable("X");
    	Query aQuery =
    		new Query(predicate, 
    				  new Term[] {X,new Atom(secondarg)} 
    				  );
    	while ( aQuery.hasMoreSolutions() ) {
    		Hashtable solution = aQuery.nextSolution();
    		resultString = resultString = resultString + "X = " + solution.get("X") + "\n"; 
    		System.out.println(resultString);
    	}
    	return resultString;
    }

    // The filepath has to be given as a String, because Prolog can not use any of
    // the variants created by IFile
    public String consult(String filepath) {
    	String resultString;
    	resultString = "";
    	
//    	System.out.println("\nfile.getFullPath().toString(): " + filepath.getFullPath().toString());
//    	System.out.println("\nfile.getFullPath().toPortableString(): " + filepath.getFullPath().toPortableString());
//    	System.out.println("\nfile.getFullPath().toOSString(): " + filepath.getFullPath().toOSString());
    	
    	Query query = new Query("consult('" + filepath + "')");

    	resultString = (query.query() ? "\nsuccess" : "\nfailed");
        
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
    
    public static String[] readFileToStringArray(String filePath) {
        File file = new File(filePath);
        FileInputStream fis = null;
        BufferedInputStream bis = null;
        DataInputStream dis = null;
        List<String> stringBuffer = new ArrayList<String>();

        try {
          fis = new FileInputStream(file);

          // Here BufferedInputStream is added for fast reading.
          bis = new BufferedInputStream(fis);
          dis = new DataInputStream(bis);

          // dis.available() returns 0 if the file does not have more lines.
          int i = 0;
          while (dis.available() != 0) {
    		  stringBuffer.add(dis.readLine());
          }

          // dispose all the resources after using them.
          fis.close();
          bis.close();
          dis.close();

        } catch (FileNotFoundException e) {
          e.printStackTrace();
        } catch (IOException e) {
          e.printStackTrace();
        }
        
        String[] result = stringBuffer.toArray(new String[]{});
        return result;
    }
 
}
