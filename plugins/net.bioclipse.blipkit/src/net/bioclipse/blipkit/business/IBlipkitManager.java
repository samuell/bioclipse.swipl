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

import org.eclipse.core.resources.IFile;

import net.bioclipse.core.PublishedClass;
import net.bioclipse.core.PublishedMethod;
import net.bioclipse.core.Recorded;
import net.bioclipse.core.TestClasses;
import net.bioclipse.managers.business.IBioclipseManager;

@PublishedClass(
    value="Gives access to the BLIPKIT system, which is a PROLOG based reasoner for Semantic data.	"
)
public interface IBlipkitManager extends IBioclipseManager {

	@Recorded
	@PublishedMethod(
			methodSummary="Initialized the prolog engine (loads blipkit modules) etc."
	)
	public String init();
	

	@Recorded
	@PublishedMethod(
			params="String filepath",
			methodSummary="Loads a Prolog database (i.e. a Prolog file containing facts and rules)"
	)
	public String consult(String filepath);

	@Recorded
	@PublishedMethod(
			params="String predicate, String object",
			methodSummary="Executes a prolog query, like so: \":- [predicate](X, [object]).\" and prints out all solutions"
	)	
    public String query2(String predicate, String object);
	
    @Recorded
    @PublishedMethod(
            params="String subject, String predicate, String object",
            methodSummary="Executes a prolog query, like so: \":- [predicate]([subject], [object]).\" and prints out all solutions. If subject or object starts with a capital, they will be treated as variables instead as of atoms."
    )   
    public String query(String subject, String predicate, String object);
    
	@Recorded
	@PublishedMethod(
			methodSummary="Prints the value of java.library.path"
	)
	public String printLibPath();
	
	@Recorded
	@PublishedMethod(
			methodSummary="Prints the arguments that the current Prolog engine was called with"
	)
	public String getActualArgs();

}
