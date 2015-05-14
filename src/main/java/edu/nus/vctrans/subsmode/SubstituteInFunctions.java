package edu.nus.vctrans.subsmode;

import java.util.Set;

/**
* Created by seryozha on 4/8/14.
*/
public class SubstituteInFunctions extends SubstitutionMode {
    private Set<String> interestingFunctions;

    public SubstituteInFunctions(Set<String> toSubstitute) {
        this.interestingFunctions = toSubstitute;
    }

    public Set<String> getInterestingFunctions() {
        return interestingFunctions;
    }
}
