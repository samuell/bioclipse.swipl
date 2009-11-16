package net.bioclipse.blipkit.business;

import java.util.Hashtable;

import jpl.Atom;
import jpl.Query;
import jpl.Term;
import jpl.Variable;

public class JPLQueryWrapper {
    
    public Term[] plTerm;
    public Query  plQuery;
    public String resultString;
   
    public JPLQueryWrapper( String prologFunction, String[] prologArguments ) {
        this.resultString ="";
        this.plTerm = new Term[prologArguments.length];

        int i = 0;
        for ( String currentPrologArgument : prologArguments ) {
            if ( isVariable(currentPrologArgument) ) {
                System.out.println("***********************************\nVariable!\n***********************************");                
                Variable plSubject = new Variable(currentPrologArgument);   
                this.plTerm[i] = plSubject;
            } else if ( isAtom(currentPrologArgument) ) {
                System.out.println("***********************************\nAtom!\n***********************************");
                Atom plSubject = new Atom(currentPrologArgument.replaceAll("(\\[|\\])", ""));   
                this.plTerm[i] = plSubject;
            } else if ( isInteger(currentPrologArgument) ) {
                System.out.println("***********************************\nInteger!\n***********************************");
                jpl.Integer plSubject = new jpl.Integer(Integer.parseInt(currentPrologArgument));   
                this.plTerm[i] = plSubject;
            } else if ( isFloat(currentPrologArgument) ) {
                System.out.println("***********************************\nFloat!\n***********************************");
                jpl.Float plSubject = new jpl.Float(java.lang.Float.valueOf(currentPrologArgument.trim()));             
                this.plTerm[i] = plSubject;
            } else {
                System.out.println("********************\nCould not decide type of " + currentPrologArgument + " for the " + (i + 1) + "th item in the array\n********************");
            }
            i++;
        }
       
        this.plQuery = new Query(prologFunction, this.plTerm);
        this.resultString = this.resultString;
        System.out.println("\n***************\n" + plQuery.toString() + "\n*****************\n");
        while ( plQuery.hasMoreSolutions() ) {
            Hashtable solution = plQuery.nextSolution();
            this.resultString = appendQueryResultToString( prologFunction, prologArguments, solution, this.resultString );  
        }
    }
    
    public String appendQueryResultToString( String prologFunction, String[] prologArguments, Hashtable solution, String resultString ) {
        String newResultString = resultString + prologFunction + "( ";
        int i = 0;
        for ( String currentPrologArgument : prologArguments ) {
            if ( i > 0 ) { newResultString = newResultString + ", "; }
            if ( isVariable(currentPrologArgument) ) {
                newResultString = newResultString + currentPrologArgument + "=" + solution.get(currentPrologArgument);
            } else if ( isAtom(currentPrologArgument) || isInteger(currentPrologArgument) || isFloat(currentPrologArgument) ) {
                newResultString = newResultString + currentPrologArgument;
            } else {
                newResultString = newResultString + "[Error: Could not decide type of term, in JPLQueryWrapper.appendQueryResultToString()]";
            }
            i++;
        }
        newResultString = newResultString + " )\n";
        return newResultString;
    }
    
    boolean isVariable(String inputString) {
        return Character.isUpperCase(inputString.trim().charAt(0));
    }
    boolean isAtom(String inputString) {
        char firstChar = inputString.trim().charAt(0);
        return (Character.isLowerCase(firstChar) || firstChar == '/' || firstChar == '\'' || firstChar == '[');        
    }
    boolean isInteger(String inputString) {
        try {
            Integer.parseInt(inputString);
            return true;
        } catch (NumberFormatException nfe) {
            return false;            
        }
    }
    boolean isFloat(String inputString) {
        try {
            java.lang.Float.valueOf(inputString);
            return true;
        } catch (NumberFormatException nfe) {
            return false;            
        }
    }
    
    public String getResultString() {
        return this.resultString;
    }

}
