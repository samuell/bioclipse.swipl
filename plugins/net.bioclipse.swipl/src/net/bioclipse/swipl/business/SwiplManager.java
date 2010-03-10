/*******************************************************************************
ta * Copyright (c) 2009  The Bioclipse team <olas@bioclipse.net>
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contact: http://www.bioclipse.net/
 ******************************************************************************/
package net.bioclipse.swipl.business;

import java.io.BufferedInputStream;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import jpl.*;
import jpl.Integer;
import net.bioclipse.managers.business.IBioclipseManager;

import org.apache.log4j.Logger;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;

public class SwiplManager implements IBioclipseManager {

    private static final Logger logger = Logger.getLogger(SwiplManager.class);

    /**
     * Gives a short one word name of the manager used as variable name when
     * scripting.
     */
    public String getManagerName() {
        return "swipl";
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
    
    public String loadPrologCode(String prologCode) {
        String fileWriteResultMsg = "";
        String returnMessage = "";
        String workspacePath = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString();
        String tempFileName = ".bioclipse.swipl.loadPrologCode.tmp";
        String tempFilePath = workspacePath + "/" + tempFileName; // TODO: Change "/" to sth OS independent!
        try { 
            BufferedWriter outBuffer = new BufferedWriter( new FileWriter( tempFilePath ) );
            outBuffer.write( prologCode ); 
            outBuffer.close();
            fileWriteResultMsg = "succeeded";
        } catch (IOException ioe) { 
            System.out.println(ioe.getMessage());
            fileWriteResultMsg = "failed";
        } 
        returnMessage = "Writing temp file: " + fileWriteResultMsg + ". tempFilePath: " + tempFilePath;
        returnMessage += "\n Telling Prolog to consult temp file:\n" + consult(tempFilePath);
                
        return returnMessage;
    }

    public String queryRDF(String subject, String predicate, String object) {
        String[] prologArguments = { subject, predicate, object };
        String prologRDFFunctionName = "rdf";
        JPLQueryWrapper prologQueryContainer = new JPLQueryWrapper( prologRDFFunctionName, prologArguments, 10 );
        return prologQueryContainer.getResultString();
    }
    
    public List<List<String>> queryProlog( String[] args ) {
        String prologFunction = "";
        int resultsLimit = 0;
        List<List<String>> table = null;
        if ( args.length > 0 ) {
            prologFunction = args[0];
        }
        if ( args.length > 1 ) {
            resultsLimit = java.lang.Integer.parseInt(args[1]);
            System.out.println("***\nLIMIT = " + resultsLimit + "\n***");
        }
        if ( args.length > 2 ) {
            String[] tempPrologArguments = removeElementFromStringArray( args, 0 );
            String[] prologArguments = removeElementFromStringArray( tempPrologArguments, 0 );
            JPLQueryWrapper prologQueryContainer = new JPLQueryWrapper( prologFunction, prologArguments, resultsLimit );
            table = prologQueryContainer.getResultList();        
        }
        return table;
    }
    
    public boolean loadRDFToProlog(String rdfFile) {
        boolean result = false;
        String resultString = "";
        Query loadRDFQuery = new Query("rdf_load", 
                new Term[] { new Atom( rdfFile ) });
        resultString = "Result of query: " + loadRDFQuery.toString() + "\n"; 
        System.out.println("\n*********************************\n" + resultString + "\n*********************************\n");
        result = loadRDFQuery.hasSolution();
        return result;
    }
 
    // The filepath has to be given as a String, because Prolog can not use any of
    // the variants created by IFile
    public String consult(String filepath) {
        String resultString;
        resultString = "";
        
//        System.out.println("\nfile.getFullPath().toString(): " + filepath.getFullPath().toString());
//        System.out.println("\nfile.getFullPath().toPortableString(): " + filepath.getFullPath().toPortableString());
//        System.out.println("\nfile.getFullPath().toOSString(): " + filepath.getFullPath().toOSString());
        
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
    
    public static String[] removeElementFromStringArray(String[] input, int indexOfItemToDelete ) {
        if (input != null) {
                List<String> list = new ArrayList<String>(Arrays.asList(input));
                for (int i = 0; i < list.size(); i++) {
                        if (i == indexOfItemToDelete) {
                                list.remove(i);
                        }
                }
                return list.toArray(new String[0]);
        } else {
                return new String[0];
        }
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
