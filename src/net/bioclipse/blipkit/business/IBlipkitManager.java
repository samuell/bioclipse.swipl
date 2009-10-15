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
			methodSummary="Tests the JPL Java/Prolog API"
	)
	public String test();

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
