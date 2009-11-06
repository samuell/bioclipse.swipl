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
                Variable plSubject = new Variable(currentPrologArgument);   
                this.plTerm[i] = plSubject;
            } else if ( isAtom(currentPrologArgument) ) {
                Atom plSubject = new Atom(currentPrologArgument);   
                this.plTerm[i] = plSubject;
            } else if ( isInteger(currentPrologArgument) ) {
                jpl.Integer plSubject = new jpl.Integer(Integer.parseInt(currentPrologArgument));   
                this.plTerm[i] = plSubject;
            } else if ( isFloat(currentPrologArgument) ) {
                jpl.Float plSubject = new jpl.Float(java.lang.Float.valueOf(currentPrologArgument.trim()));             
                this.plTerm[i] = plSubject;
            } else {
                System.out.println("********************\nCould not decide type of " + currentPrologArgument + " for the " + (i + 1) + "th item in the array\n********************");
            }
            i++;
        }
       
        this.plQuery = new Query(prologFunction, this.plTerm);
        while ( plQuery.hasMoreSolutions() ) {
            Hashtable solution = plQuery.nextSolution();
            this.resultString = appendQueryResultToString( prologArguments, solution, this.resultString );  
        }
    }
    
    public String appendQueryResultToString( String[] prologArguments, Hashtable solution, String resultString ) {
        String newResultString = resultString + "\n";
        int i = 0;
        for ( String currentPrologArgument : prologArguments ) {
            if ( isVariable(currentPrologArgument) ) {
                newResultString = newResultString + "Term " + (i + 1) + " = " + solution.get(this.plTerm[i]) + ", ";
            } else if ( isAtom(currentPrologArgument) || isInteger(currentPrologArgument) || isFloat(currentPrologArgument) ) {
                newResultString = newResultString + currentPrologArgument + ", ";
            } else {
                System.out.println("********************\nCould not decide type of " + currentPrologArgument + " for the " + (i + 1) + "th item in the array\n********************");
            }
            i++;
        }
        newResultString = newResultString + "\n";
        return newResultString;
    }
    
    boolean isVariable(String inputString) {
        return Character.isUpperCase(inputString.trim().charAt(0));
    }
    boolean isAtom(String inputString) {
        return Character.isLowerCase(inputString.trim().charAt(0));        
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
