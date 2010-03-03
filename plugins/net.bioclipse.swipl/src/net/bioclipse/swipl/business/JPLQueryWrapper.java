package net.bioclipse.swipl.business;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import jpl.Atom;
import jpl.Compound;
import jpl.Query;
import jpl.Term;
import jpl.Variable;

public class JPLQueryWrapper {
    
    public Term[] plTerm;
    public Query  plQuery;
    public List<List<String>> resultList = new ArrayList<List<String>>();
    public String resultString;

    public JPLQueryWrapper( String prologFunction, String[] prologArguments, int resultsLimit ) {
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
            } else if ( isList(currentPrologArgument) ) {
                System.out.println("***********************************\nFloat!\n***********************************");
                currentPrologArgument = currentPrologArgument.replace(" ", "").replace("[", "").replace("]", "");
                System.out.println("currentPrologArgument: " + currentPrologArgument);
                String[] currentPrologArguments = currentPrologArgument.split("\\,");
                jpl.Term[] plSubjectArray = new jpl.Term[currentPrologArguments.length];             
                int j = 0;
                for( String currentArg : currentPrologArguments ) {
                    jpl.Float currentArgObj = new jpl.Float( java.lang.Float.valueOf(currentArg) );
                    plSubjectArray[j] = currentArgObj;  
                    j++;
                }
                jpl.Term plSubject = termArrayToList( plSubjectArray );
                this.plTerm[i] = plSubject;          
            } else {
                System.out.println("********************\nCould not decide type of " + currentPrologArgument + " for the " + (i + 1) + "th item in the array\n********************");
            }
            i++;
        }
       
        this.plQuery = new Query(prologFunction, this.plTerm);
        System.out.println("\n***************\n" + plQuery.toString() + "\n*****************\n");
        
        int j = 0;
        while ( plQuery.hasMoreSolutions() && ( j < resultsLimit || resultsLimit == 0 ) ) {
            System.out.println("\nj = " + Integer.toString(j));
            Hashtable solution = plQuery.nextSolution();
            this.resultList.add(queryResultAsStringList( prologFunction, prologArguments, solution ));
            j++;
        }
        // Maybe this (added 20091117) can help avoid stack overflow errors:
        plQuery.close();
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

    public List<String> queryResultAsStringList( String prologFunction, String[] prologArguments, Hashtable solution ) {
        List<String> resultStringList = new ArrayList<String>();
        int i = 0;
        for ( String currentPrologArgument : prologArguments ) {
            if ( isVariable(currentPrologArgument) ) {
                System.out.println("\nVariable (" + i + "): " + currentPrologArgument);
                resultStringList.add(solution.get(currentPrologArgument).toString());
            } 
        i++;
        }
        return resultStringList;
    }
    
    
    boolean isVariable(String inputString) {
        return Character.isUpperCase(inputString.trim().charAt(0));
    }
    boolean isAtom(String inputString) {
        char firstChar = inputString.trim().charAt(0);
        return (Character.isLowerCase(firstChar) || firstChar == '/' || firstChar == '\'' || firstChar == '{');        
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
    boolean isList(String inputString) {
        char firstChar = inputString.trim().charAt(0);
        return ( firstChar == '[' );        
    }
    
    public static Term termArrayToList(Term[] terms) {
        Term list = new Atom("[]");
        for (int i = terms.length - 1; i >= 0; --i) {
            list = new Compound(".", new Term[] { terms[i], list });
        }
        return list;
    } 
    
    public String getResultString() {
        return this.resultString;
    }

    public List<List<String>> getResultList() {
        return this.resultList;
    }

}
